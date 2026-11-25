# Standalone clock verification for the developer check runner.
# Sourcing this file makes no requests and changes no check settings.
# HTTPS Date fields follow RFC 9110 section 6.6.1. The fixed services are
# independently operated; this is a bounded clock sanity check, not NTP.

#' Read an Independent Clock Pair From a Bounded System Command
#' @param executable One trusted local interpreter path.
#' @param args Fixed command arguments, separately quoted where needed.
#' @param source A public name for the elapsed-clock implementation.
#' @param run Command runner, injectable for offline tests.
#' @return A list containing finite wall and monotonic elapsed seconds.
.prism_clock_command <- function(executable, args, source, run = base::system2) {
  stopifnot("executable must be one nonempty path" = is.character(executable) &&
    length(executable) == 1L && !is.na(executable) && nzchar(executable),
    "args must be fixed character arguments" = is.character(args) && !anyNA(args),
    "source must name one clock implementation" = is.character(source) &&
      length(source) == 1L && !is.na(source),
    "run must be a function" = is.function(run))
  text <- tryCatch(suppressWarnings(run(executable, args, stdout = TRUE,
    stderr = TRUE, timeout = 3L)), error = function(e) character())
  status <- attr(text, "status")
  if ((length(status) && !identical(status, 0L)) || length(text) != 1L ||
      is.na(text) || !grepl("^[0-9]+([.][0-9]+)? [0-9]+([.][0-9]+)?$", trimws(text)))
    stop("A trusted monotonic clock could not be read within three seconds.", call. = FALSE)
  values <- as.double(strsplit(trimws(text), " ", fixed = TRUE)[[1L]])
  out <- list(wall = values[[1L]], elapsed = values[[2L]], source = source)
  stopifnot("clock command must return finite time readings" =
    all(is.finite(c(out$wall, out$elapsed))))
  out
}

#' Read Wall Time and a Genuine Monotonic Counter Together
#' @return A list containing finite wall and elapsed seconds plus their source.
.prism_clock_now <- function() {
  # R's proc.time() elapsed component shares the adjustable wall clock on
  # some platforms. Use an independent OS counter for detecting clock jumps.
  if (.Platform$OS.type == "windows") {
    executable <- file.path(Sys.getenv("SystemRoot"), "System32",
      "WindowsPowerShell", "v1.0", "powershell.exe")
    if (!file.exists(executable))
      stop("Windows PowerShell is required to read the monotonic clock.", call. = FALSE)
    script <- paste(
      "if(-not [Diagnostics.Stopwatch]::IsHighResolution){exit 2};",
      "$c=[Globalization.CultureInfo]::InvariantCulture;",
      "$w=[DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds()/1000.0;",
      "$e=[Diagnostics.Stopwatch]::GetTimestamp()/[double][Diagnostics.Stopwatch]::Frequency;",
      "[Console]::Out.WriteLine($w.ToString('F6',$c)+' '+$e.ToString('F6',$c))")
    return(.prism_clock_command(executable,
      c("-NoLogo", "-NoProfile", "-NonInteractive", "-Command", shQuote(script)),
      "windows-stopwatch"))
  }
  if (file.exists("/proc/uptime")) {
    line <- readLines("/proc/uptime", n = 1L, warn = FALSE)
    elapsed <- as.double(strsplit(line, " ", fixed = TRUE)[[1L]][[1L]])
    out <- list(wall = as.double(Sys.time()), elapsed = elapsed,
      source = "linux-proc-uptime")
  } else {
    # macOS and other systems may use an already installed Python 3. Its
    # isolated mode ignores user startup code. Never install a dependency.
    executable <- unname(Sys.which("python3"))
    if (!nzchar(executable))
      stop("No independent monotonic clock is available on this system.", call. = FALSE)
    script <- "import time; print('%.6f %.6f' % (time.time(), time.monotonic()))"
    out <- .prism_clock_command(executable, c("-I", "-S", "-c", shQuote(script)),
      "python-monotonic")
  }
  stopifnot("clock readings must be finite scalars" =
    all(vapply(out[c("wall", "elapsed")], function(x) is.numeric(x) && length(x) == 1L &&
      is.finite(x), logical(1))))
  out
}

#' Format Seconds Since the Epoch as UTC
#' @param seconds One finite numeric timestamp.
#' @return One UTC timestamp with an explicit Z suffix.
.prism_clock_utc <- function(seconds) {
  stopifnot("seconds must be one finite number" = is.numeric(seconds) &&
    length(seconds) == 1L && is.finite(seconds))
  out <- format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  stopifnot("UTC timestamp must have an explicit zone" =
    length(out) == 1L && !is.na(out) && endsWith(out, "Z"))
  out
}

#' Fetch Headers From a Fixed HTTPS Clock Source
#' @param url One URL in the fixed source registry.
#' @param timeout Integer request limit, from one to ten seconds.
#' @param request Header transport, injectable for offline transport tests.
#' @return Headers, status and transport guarantees; no response body.
.prism_clock_fetch <- function(url, timeout, request = base::curlGetHeaders) {
  allowed <- c("https://cran.r-project.org/",
    "https://www.google.com/generate_204", "https://www.cloudflare.com/")
  stopifnot("url must identify a fixed HTTPS source" = is.character(url) &&
    length(url) == 1L && !is.na(url) && url %in% allowed,
    "timeout must be an integer from 1 to 10" = is.numeric(timeout) &&
      length(timeout) == 1L && is.finite(timeout) && timeout == as.integer(timeout) &&
      timeout >= 1L && timeout <= 10L,
    "request must be a function" = is.function(request))
  # tempfile() only generates a name here. Its basename contains no user or
  # device name, creates no file, and does not change R's random-number state.
  # A fresh cache key prevents a previously stored Date from being reused.
  address <- paste0(url, "?prism-clock=", basename(tempfile(pattern = "")))
  headers <- suppressWarnings(request(address, redirect = FALSE, verify = TRUE,
    timeout = as.integer(timeout), TLS = "1.2"))
  out <- list(headers = headers, status = attr(headers, "status"),
    tls_verified = TRUE, cache_busted = TRUE)
  stopifnot("transport must return a list with headers and guarantees" =
    is.list(out) && is.character(out$headers) &&
      isTRUE(out$tls_verified) && isTRUE(out$cache_busted))
  out
}

#' Parse a Strict HTTP Date Without Changing the Locale
#' @param value One IMF-fixdate value, including its GMT suffix.
#' @return One finite numeric UTC timestamp.
.prism_clock_http_date <- function(value) {
  stopifnot("Date must be one character value" = is.character(value) &&
    length(value) == 1L && !is.na(value))
  pattern <- paste0("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), ",
    "([0-9]{2}) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ",
    "([0-9]{4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT$")
  fields <- regmatches(value, regexec(pattern, value))[[1L]]
  if (length(fields) != 8L) stop("invalid HTTP Date", call. = FALSE)
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec")
  numeric_date <- sprintf("%s-%02d-%s %s:%s:%s", fields[[5L]],
    match(fields[[4L]], months), fields[[3L]], fields[[6L]], fields[[7L]], fields[[8L]])
  stamp <- as.POSIXct(numeric_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  valid <- !is.na(stamp) && identical(format(stamp, "%Y-%m-%d %H:%M:%S",
    tz = "UTC"), numeric_date) && as.integer(fields[[5L]]) >= 1970L &&
    as.integer(fields[[8L]]) <= 59L
  if (valid) valid <- identical(fields[[2L]],
    c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[[as.POSIXlt(stamp,
      tz = "UTC")$wday + 1L]])
  if (!valid) stop("invalid HTTP Date", call. = FALSE)
  out <- as.double(stamp)
  stopifnot("parsed Date must be finite UTC seconds" =
    is.numeric(out) && length(out) == 1L && is.finite(out))
  out
}

#' Validate the Final HTTPS Response Header Block
#' @param answer A result from the strict header transport.
#' @return The remote UTC seconds and HTTP status.
.prism_clock_headers <- function(answer) {
  if (!is.list(answer) || !isTRUE(answer$tls_verified) ||
      !isTRUE(answer$cache_busted)) stop("unverified HTTPS transport", call. = FALSE)
  headers <- answer$headers
  if (!is.character(headers) || !length(headers) || anyNA(headers) ||
      length(headers) > 500L || any(nchar(headers, type = "bytes") > 8192L))
    stop("invalid HTTP headers", call. = FALSE)
  headers <- sub("[\r\n]+$", "", headers)
  if (any(grepl("[\r\n]", headers))) stop("invalid HTTP headers", call. = FALSE)
  starts <- grep("^HTTP/[0-9.]+[ \t]+[0-9]{3}([ \t].*)?$", headers)
  if (!length(starts)) stop("missing HTTP status", call. = FALSE)
  # A proxy CONNECT exchange can prepend its own status and Date. Only the
  # final server response is evidence; redirects are disabled by transport.
  start <- utils::tail(starts, 1L)
  status <- as.integer(sub("^HTTP/[0-9.]+[ \t]+([0-9]{3}).*$", "\\1", headers[[start]]))
  if (!is.numeric(answer$status) || length(answer$status) != 1L ||
      is.na(answer$status) || status != answer$status || status < 200L || status >= 300L)
    stop("unsuccessful HTTP status", call. = FALSE)
  block <- if (start < length(headers)) headers[seq.int(start + 1L, length(headers))]
    else character()
  blank <- match("", block)
  if (!is.na(blank)) block <- utils::head(block, blank - 1L)
  field <- function(name) trimws(sub("^[^:]+:", "", grep(paste0("^", name, ":"),
    block, value = TRUE, ignore.case = TRUE)))
  date <- field("date")
  if (length(date) != 1L) stop("missing or duplicated HTTP Date", call. = FALSE)
  age <- field("age")
  if (length(age) > 1L || (length(age) && (!grepl("^[0-9]+$", age) ||
      !is.finite(as.double(age)) || as.double(age) != 0)))
    stop("stale or cached response", call. = FALSE)
  cached <- c(field("x-cache"), field("x-cache-status"), field("cf-cache-status"))
  if (any(grepl("\\bHIT\\b|\\bSTALE\\b", cached, ignore.case = TRUE, perl = TRUE)) ||
      any(grepl("^(110|111)[ \t]", field("warning"))))
    stop("stale or cached response", call. = FALSE)
  out <- list(remote = .prism_clock_http_date(date), status = status)
  stopifnot("response must contain finite remote time and success status" =
    is.finite(out$remote) && out$status >= 200L && out$status < 300L)
  out
}

#' Stop a Clock Check With Auditable Evidence
#' @param message One plain diagnostic message.
#' @param observations A data frame of bounded public time observations.
#' @return Does not return; signals a prism_clock_error condition.
.prism_clock_stop <- function(message, observations) {
  stopifnot("message must be one plain diagnostic" = is.character(message) &&
    length(message) == 1L && !is.na(message),
    "observations must be a data frame" = is.data.frame(observations))
  stop(structure(list(message = message, call = NULL, observations = observations),
    class = c("prism_clock_error", "error", "condition")))
}

#' Verify the Local Clock Against Independent HTTPS Sources
#'
#' Makes at most three bounded header requests with certificate verification,
#' a TLS 1.2 minimum, disabled redirects and fresh anonymous cache keys. At
#' least two independently operated sources must agree. A valid response with
#' excessive clock skew, stale cache evidence, or a clock jump fails the check.
#' Network failures never count as verification. No system, locale, RNG,
#' environment or R CMD check settings are changed.
#'
#' @param timeout Integer per-request limit, from one to ten seconds.
#' @param tolerance Maximum permitted clock difference, greater than zero and
#'   no more than sixty seconds. Transport uncertainty is included in this bound.
#' @param fetch Header transport, injectable for offline tests.
#' @param now Wall and elapsed clock reader, injectable for offline tests.
#' @return A list with verified, verified_at, tolerance_seconds,
#'   elapsed_seconds, elapsed_clock and an observations data frame. On failure, a
#'   prism_clock_error carries the observations collected so far.
prism_check_clock <- function(timeout = 10L, tolerance = 60,
    fetch = .prism_clock_fetch, now = .prism_clock_now) {
  stopifnot("timeout must be an integer from 1 to 10" = is.numeric(timeout) &&
    length(timeout) == 1L && is.finite(timeout) && timeout == as.integer(timeout) &&
    timeout >= 1L && timeout <= 10L,
    "tolerance must be greater than zero and at most 60 seconds" =
      is.numeric(tolerance) && length(tolerance) == 1L && is.finite(tolerance) &&
      tolerance > 0 && tolerance <= 60,
    "fetch and now must be functions" = is.function(fetch) && is.function(now))
  read_now <- function() {
    stamp <- now()
    stopifnot("now must return finite wall and elapsed seconds" = is.list(stamp) &&
      all(c("wall", "elapsed") %in% names(stamp)) &&
      all(vapply(stamp[c("wall", "elapsed")], function(x) is.numeric(x) &&
        length(x) == 1L && is.finite(x), logical(1))))
    stamp
  }
  sources <- c(CRAN = "https://cran.r-project.org/",
    Google = "https://www.google.com/generate_204",
    Cloudflare = "https://www.cloudflare.com/")
  started <- read_now()
  observations <- do.call(rbind, lapply(names(sources), function(source) {
    before <- read_now()
    # External exception text could contain proxy credentials. Keep only a
    # fixed diagnostic; neither raw headers nor bodies enter the audit record.
    answer <- tryCatch(suppressWarnings(fetch(sources[[source]], as.integer(timeout))),
      error = function(e) NULL)
    after <- read_now()
    duration <- after$elapsed - before$elapsed
    wall_duration <- after$wall - before$wall
    midpoint <- (before$wall + after$wall) / 2
    row <- data.frame(source = source, url = unname(sources[[source]]),
      status = NA_integer_, remote_utc = NA_character_,
      local_midpoint_utc = .prism_clock_utc(midpoint),
      offset_seconds = NA_real_, duration_seconds = duration,
      uncertainty_seconds = duration / 2 + 1,
      accepted = FALSE, fatal = FALSE, reason = "HTTPS request unavailable",
      stringsAsFactors = FALSE)
    if (duration < 0 || duration > timeout + 4 ||
        abs(wall_duration - duration) > 1) {
      row$fatal <- TRUE
      row$reason <- "wall clock changed or request exceeded its time limit"
      return(row)
    }
    if (is.null(answer)) return(row)
    result <- tryCatch(.prism_clock_headers(answer), error = function(e) e)
    if (inherits(result, "error")) {
      # These messages originate only in our bounded parser, not in transport.
      row$reason <- conditionMessage(result)
      row$fatal <- identical(row$reason, "stale or cached response")
      return(row)
    }
    row$status <- result$status
    row$remote_utc <- .prism_clock_utc(result$remote)
    # HTTP dates have one-second resolution. Use the centre of that second,
    # then account for the full round trip and date quantisation uncertainty.
    row$offset_seconds <- result$remote + 0.5 - midpoint
    if (abs(row$offset_seconds) + row$uncertainty_seconds >= tolerance) {
      row$fatal <- TRUE
      row$reason <- "remote clock difference reaches the allowed bound"
    } else {
      row$accepted <- TRUE
      row$reason <- "fresh verified HTTPS Date"
    }
    row
  }))
  finished <- read_now()
  elapsed <- finished$elapsed - started$elapsed
  # Eight bounded OS-counter reads add at most 24 seconds on Windows;
  # the Linux reader is a small local read. Including network time, the
  # complete verifier remains below a minute at the maximum timeout.
  if (elapsed < 0 || elapsed > 3 * timeout + 24 ||
      abs((finished$wall - started$wall) - elapsed) > 1)
    .prism_clock_stop("Clock changed or verification exceeded its total time limit.", observations)
  if (any(observations$fatal))
    .prism_clock_stop("Clock verification rejected stale, skewed or unstable time evidence.", observations)
  good <- observations[observations$accepted, , drop = FALSE]
  if (nrow(good) < 2L)
    .prism_clock_stop("Clock verification requires two independent fresh HTTPS sources.", observations)
  # Intersect the offset intervals, accounting for measured transport delay.
  if (max(good$offset_seconds - good$uncertainty_seconds) >
      min(good$offset_seconds + good$uncertainty_seconds))
    .prism_clock_stop("Independent HTTPS clock sources disagree.", observations)
  out <- list(verified = TRUE, verified_at = .prism_clock_utc(finished$wall),
    tolerance_seconds = tolerance, elapsed_seconds = elapsed,
    elapsed_clock = if (is.null(started$source)) "injected" else started$source,
    observations = observations)
  stopifnot("clock evidence must contain an independent source quorum" =
    isTRUE(out$verified) && is.character(out$verified_at) &&
      sum(out$observations$accepted) >= 2L &&
      all(abs(good$offset_seconds) + good$uncertainty_seconds < tolerance))
  out
}
