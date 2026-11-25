# Offline developer tests; no network and no installed-package dependency.
# Run from the repository root: Rscript --vanilla tools/test-check-clock.R
local({
  arguments <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", arguments[grepl("^--file=", arguments)])
  here <- if (length(script)) dirname(normalizePath(script[[1L]])) else "tools"
  env <- new.env(parent = baseenv())
  sys.source(file.path(here, "check-clock.R"), envir = env)
  origin <- as.double(as.POSIXct("2026-11-25 12:00:00", tz = "UTC"))
  cases <- 0L
  assert <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
    cases <<- cases + 1L
  }
  # Construct IMF-fixdates independently of the parser and of LC_TIME.
  http_date <- function(seconds) {
    value <- as.POSIXlt(seconds, origin = "1970-01-01", tz = "UTC")
    sprintf("%s, %02d %s %04d %02d:%02d:%02d GMT",
      c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[[value$wday + 1L]],
      value$mday, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec")[[value$mon + 1L]],
      value$year + 1900L, value$hour, value$min, as.integer(value$sec))
  }
  answer <- function(seconds, status = 200L, extra = character(),
      verified = TRUE, fresh = TRUE) {
    list(headers = c(paste0("HTTP/2 ", status, "\r\n"),
      paste0("Date: ", http_date(seconds), "\r\n"), extra, "\r\n"),
      status = status, tls_verified = verified, cache_busted = fresh)
  }
  reader <- function(wall_adjustment = rep(0, 8L), elapsed = c(0, 0, 2, 2, 4, 4, 6, 6)) {
    position <- 0L
    function() {
      position <<- position + 1L
      list(wall = origin + elapsed[[position]] + wall_adjustment[[position]],
        elapsed = elapsed[[position]])
    }
  }
  transport <- function(offsets = c(0, 0, 0), extras = rep(list(character()), 3L),
      unavailable = integer(), statuses = c(200L, 204L, 200L),
      verified = rep(TRUE, 3L), fresh = rep(TRUE, 3L)) {
    position <- 0L
    function(url, timeout) {
      position <<- position + 1L
      if (position %in% unavailable)
        stop("unreachable proxy https://private-user:private-secret@invalid/", call. = FALSE)
      answer(origin + c(1, 3, 5)[[position]] + offsets[[position]],
        status = statuses[[position]], extra = extras[[position]],
        verified = verified[[position]], fresh = fresh[[position]])
    }
  }
  run <- function(fetch = transport(), now = reader(), ...) {
    env$prism_check_clock(fetch = fetch, now = now, ...)
  }
  rejected <- function(expression, label, pattern = NULL) {
    result <- tryCatch(force(expression), error = function(e) e)
    assert(inherits(result, "prism_clock_error"), paste(label, "must fail closed"))
    assert(is.data.frame(result$observations) && nrow(result$observations) == 3L,
      paste(label, "must retain bounded evidence"))
    if (!is.null(pattern)) assert(grepl(pattern, conditionMessage(result)),
      paste(label, "must explain the failed check"))
    result
  }

  before <- list(options = options(), environment = Sys.getenv(),
    locale = Sys.getlocale(), timezone = Sys.getenv("TZ", unset = NA_character_),
    seed_exists = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  if (before$seed_exists) before$seed <- get(".Random.seed", envir = .GlobalEnv)

  # Test actual transport arguments without opening a socket.
  requests <- list()
  mock_request <- function(url, redirect, verify, timeout, TLS) {
    requests[[length(requests) + 1L]] <<- list(url = url, redirect = redirect,
      verify = verify, timeout = timeout, TLS = TLS)
    out <- answer(origin)$headers
    attr(out, "status") <- 200L
    out
  }
  invisible(env$.prism_clock_fetch("https://cran.r-project.org/", 10L, mock_request))
  invisible(env$.prism_clock_fetch("https://cran.r-project.org/", 10L, mock_request))
  assert(all(vapply(requests, function(x) isTRUE(x$verify) && identical(x$TLS, "1.2") &&
    identical(x$redirect, FALSE) && identical(x$timeout, 10L), logical(1))),
    "transport must keep TLS verification and a bounded timeout enabled")
  assert(!identical(requests[[1L]]$url, requests[[2L]]$url) &&
    all(vapply(requests, function(x) startsWith(x$url,
      "https://cran.r-project.org/?prism-clock="), logical(1))),
    "cache keys must be unique and remain on the fixed HTTPS source")
  invalid_url <- tryCatch(env$.prism_clock_fetch("http://cran.r-project.org/",
    10L, mock_request), error = function(e) e)
  assert(inherits(invalid_url, "error") && length(requests) == 2L,
    "insecure or unlisted URLs must be rejected before transport")
  certificate_failure <- function(...) stop("SSL certificate verification failed", call. = FALSE)
  assert(inherits(tryCatch(env$.prism_clock_fetch("https://cran.r-project.org/", 10L,
    certificate_failure), error = function(e) e), "error"),
    "certificate verification failures must never trigger an insecure retry")

  counter_request <- NULL
  mock_counter <- function(command, args, stdout, stderr, timeout) {
    counter_request <<- list(command = command, args = args, stdout = stdout,
      stderr = stderr, timeout = timeout)
    sprintf("%.6f %.6f", origin, 123.5)
  }
  counter <- env$.prism_clock_command("fixed-interpreter", "fixed-script",
    "test-monotonic", mock_counter)
  assert(identical(counter$wall, origin) && identical(counter$elapsed, 123.5) &&
    identical(counter_request$timeout, 3L), "independent counter reads must have bounded execution")
  bad_counter <- function(...) "not a monotonic counter"
  assert(inherits(tryCatch(env$.prism_clock_command("fixed-interpreter", "fixed-script",
    "test-monotonic", bad_counter), error = function(e) e), "error"),
    "malformed counter output must fail closed")
  slow_counter <- function(...) structure(character(), status = 124L)
  assert(inherits(tryCatch(env$.prism_clock_command("fixed-interpreter", "fixed-script",
    "test-monotonic", slow_counter), error = function(e) e), "error"),
    "counter timeout must fail closed")
  native_counter <- env$.prism_clock_now()
  assert(is.finite(native_counter$wall) && is.finite(native_counter$elapsed) &&
    native_counter$source %in% c("linux-proc-uptime", "windows-stopwatch", "python-monotonic"),
    "the host must provide an independent native monotonic counter without network access")

  ok <- run()
  assert(isTRUE(ok$verified) && sum(ok$observations$accepted) == 3L,
    "three agreeing fresh sources must verify")
  assert(identical(ok$verified_at, "2026-11-25T12:00:06Z") &&
    identical(ok$elapsed_seconds, 6), "evidence must record exact UTC and elapsed time")
  assert(all(ok$observations$offset_seconds == 0.5) &&
    all(ok$observations$duration_seconds == 2), "offsets must account for round trips")
  assert(!any(grepl("[?]", ok$observations$url)), "audit URLs must not contain cache tokens")
  fallback <- run(fetch = transport(unavailable = 1L))
  assert(isTRUE(fallback$verified) && sum(fallback$observations$accepted) == 2L,
    "two independent fallback sources must verify")
  assert(!any(grepl("private-user|private-secret", capture.output(str(fallback)))),
    "transport exception details must not leak proxy credentials")

  invisible(rejected(run(fetch = transport(unavailable = 1:3)), "offline sources", "two independent"))
  invisible(rejected(run(fetch = transport(unavailable = 2:3)), "a single source", "two independent"))
  invisible(rejected(run(fetch = transport(offsets = c(0, 12, 0))),
    "source disagreement", "disagree"))
  invisible(rejected(run(fetch = transport(offsets = rep(60, 3))),
    "sixty-second clock skew", "skewed"))
  invisible(rejected(run(fetch = transport(offsets = rep(-300, 3))),
    "five-minute clock skew", "skewed"))
  invisible(rejected(run(fetch = transport(offsets = rep(58, 3))),
    "uncertain near-limit skew", "skewed"))
  invisible(rejected(run(fetch = transport(extras = list(character(), character(), "Age: 90"))),
    "cached stale time", "stale"))
  invisible(rejected(run(fetch = transport(extras = list("Age: nonsense", character(), character()))),
    "malformed cache age", "stale"))
  invisible(rejected(run(fetch = transport(extras = list("X-Cache: HIT", character(), character()))),
    "cached response without age", "stale"))
  invisible(rejected(run(fetch = transport(extras = list("Warning: 110 stale", character(), character()))),
    "stale response warning", "stale"))
  invisible(rejected(run(fetch = transport(verified = c(FALSE, FALSE, TRUE))),
    "unverified certificates", "two independent"))
  invisible(rejected(run(fetch = transport(fresh = c(FALSE, FALSE, TRUE))),
    "missing cache prevention", "two independent"))
  invisible(rejected(run(fetch = transport(statuses = c(302L, 500L, 200L))),
    "redirect or failed response", "two independent"))
  invisible(rejected(run(now = reader(wall_adjustment = c(0, 0, 10, 10, 10, 10, 10, 10))),
    "wall clock jump", "Clock changed"))
  invisible(rejected(run(now = reader(wall_adjustment = c(0, 0, 3, 0, 0, 0, 0, 0))),
    "temporary wall clock jump", "unstable"))
  invisible(rejected(run(now = reader(elapsed = c(0, 0, 20, 20, 22, 22, 24, 24))),
    "excessive request duration", "unstable"))

  proxy <- answer(origin)
  proxy$headers <- c("HTTP/1.1 200 Connection established\r\n",
    "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n", "\r\n", proxy$headers)
  assert(identical(env$.prism_clock_headers(proxy)$remote, origin),
    "the final server Date must override proxy CONNECT dates")
  repeated <- answer(origin, extra = paste0("Date: ", http_date(origin + 10)))
  bad_dates <- list("Wed, 31 Feb 2026 12:00:00 GMT", "Wed, 25 Nov 2026 12:00:00 PST",
    "Thu, 25 Nov 2026 12:00:00 GMT", "Wed, 25 Nov 2026 12:00:99 GMT",
    "$(do-not-run)", paste0(http_date(origin), "\r\nAge: 0"))
  invisible(lapply(bad_dates, function(value) {
    result <- tryCatch(env$.prism_clock_http_date(value), error = function(e) e)
    assert(inherits(result, "error"), "malformed or injected Date must be rejected")
  }))
  assert(inherits(tryCatch(env$.prism_clock_headers(repeated),
    error = function(e) e), "error"), "duplicate Date headers must be rejected")
  assert(identical(env$.prism_clock_http_date("Thu, 29 Feb 2024 01:02:03 GMT"),
    as.double(as.POSIXct("2024-02-29 01:02:03", tz = "UTC"))), "valid leap days must parse")
  assert(inherits(tryCatch(run(tolerance = 300), error = function(e) e), "error"),
    "callers must not weaken the sixty-second bound")
  assert(inherits(tryCatch(run(timeout = 11L), error = function(e) e), "error"),
    "callers must not exceed the bounded network budget")

  assert(identical(options(), before$options) && identical(Sys.getenv(), before$environment) &&
    identical(Sys.getlocale(), before$locale) &&
    identical(Sys.getenv("TZ", unset = NA_character_), before$timezone),
    "verification must not change options, environment, locale or timezone")
  assert(identical(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE), before$seed_exists) &&
    (!before$seed_exists || identical(get(".Random.seed", envir = .GlobalEnv), before$seed)),
    "cache keys must not change R random-number state")
  cat("Clock verification: ", cases, " offline assertions passed on R ",
    as.character(getRversion()), ". No network requests made.\n", sep = "")
})
