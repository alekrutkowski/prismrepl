# Read-only diagnosis of local R CMD check compiler and clock NOTEs.
# From a repository checkout:
#   Rscript --no-init-file --no-site-file tools/check-environment.R
#   Rscript --no-init-file --no-site-file tools/check-environment.R --network
# These flags skip R profiles but retain existing Renviron settings for diagnosis.
# The optional network probes use only fixed public time-service URLs.
# No check settings, compiler flags, files or system clocks are changed.

#' Capture a Bounded Read-Only Diagnostic Command
#' @param executable One existing executable path.
#' @param args Separate, fixed command arguments.
#' @param timeout Maximum command time in seconds.
#' @return A list containing an integer status and character output.
diagnostic_command <- function(executable, args, timeout = 10L) {
  stopifnot("executable must be one existing path" = is.character(executable) &&
    length(executable) == 1L && !is.na(executable) && file.exists(executable),
    "args must be nonmissing character values" = is.character(args) && !anyNA(args),
    "timeout must be a positive scalar" = is.numeric(timeout) &&
      length(timeout) == 1L && is.finite(timeout) && timeout > 0)
  out <- tryCatch({
    lines <- suppressWarnings(system2(executable, args, stdout = TRUE,
      stderr = TRUE, timeout = timeout))
    status <- attr(lines, "status")
    list(status = if (is.null(status)) 0L else as.integer(status),
      output = as.character(lines))
  }, error = function(e) list(status = NA_integer_, output = conditionMessage(e)))
  stopifnot("command result must contain a scalar status and text" =
    is.list(out) && is.integer(out$status) && length(out$status) == 1L &&
      is.character(out$output) && !anyNA(out$output))
  out
}

#' Probe One Fixed Time Service Without Changing the Clock
#' @param service A service key from the fixed registry below.
#' @param timeout Maximum request time in seconds.
#' @return A data frame describing connectivity and any reported UTC time.
diagnostic_time_service <- function(service, timeout = 10L) {
  urls <- c(
    cran_headers = "https://CRAN.R-project.org",
    timeapi = "https://timeapi.io/api/time/current/zone?timeZone=UTC",
    worldtime_https = "https://worldtimeapi.org/api/timezone/etc/UTC",
    worldtime_http = "http://worldtimeapi.org/api/timezone/etc/UTC",
    worldclock_http = "http://worldclockapi.com/api/json/utc/now")
  stopifnot("service must identify one fixed public endpoint" =
    is.character(service) && length(service) == 1L && !is.na(service) &&
      service %in% names(urls),
    "timeout must be a positive scalar" = is.numeric(timeout) &&
      length(timeout) == 1L && is.finite(timeout) && timeout > 0)
  previous <- options(timeout = timeout)
  on.exit(options(previous), add = TRUE)
  address <- unname(urls[[service]])
  warnings <- character()
  answer <- tryCatch(withCallingHandlers({
    if (service == "cran_headers") {
      # Keep certificate verification enabled; HTTP status is inspected below.
      headers <- curlGetHeaders(address, verify = TRUE, timeout = as.integer(timeout), TLS = "1.2")
      status <- attr(headers, "status")
      date <- grep("^Date:", headers, ignore.case = TRUE, value = TRUE)
      detail <- if (length(date)) trimws(sub("^[^:]+:[[:space:]]*", "", utils::tail(date, 1L)))
        else "No Date header returned"
      list(ok = identical(status, 200L), detail = paste0("HTTP ", status, "; ", detail))
    } else {
      connection <- url(address, open = "", method = "libcurl")
      on.exit(close(connection), add = TRUE)
      open(connection, "rb")
      body <- rawToChar(readBin(connection, "raw", n = 65536L))
      field <- if (service == "timeapi") "dateTime" else
        if (service == "worldclock_http") "currentDateTime" else "datetime"
      pattern <- paste0('"', field, '"[[:space:]]*:[[:space:]]*"([^"]+)"')
      found <- regmatches(body, regexec(pattern, body, perl = TRUE))[[1L]]
      list(ok = length(found) > 1L, detail = if (length(found) > 1L)
        paste0(field, ": ", found[[2L]]) else paste0("Reached endpoint, but no ", field, " field"))
    }
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) list(ok = FALSE, detail = conditionMessage(e)))
  out <- data.frame(service = service, url = address, ok = isTRUE(answer$ok),
    detail = paste(unique(c(answer$detail, warnings)), collapse = " | "),
    stringsAsFactors = FALSE)
  stopifnot("time probe must return one bounded observation" =
    is.data.frame(out) && nrow(out) == 1L && is.logical(out$ok) &&
      !anyNA(out))
  out
}

#' Report Local Compiler-Flag and Clock-Verification Evidence
#' @param network Probe fixed public clock endpoints; FALSE by default.
#' @param timeout Per-command and per-request limit in seconds.
#' @return A list of runtime information, compiler evidence and optional network results.
check_environment <- function(network = FALSE, timeout = 10L) {
  stopifnot("network must be one logical value" = is.logical(network) &&
    length(network) == 1L && !is.na(network),
    "timeout must be a positive scalar" = is.numeric(timeout) &&
      length(timeout) == 1L && is.finite(timeout) && timeout > 0)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  stopifnot("the current R executable must exist" = file.exists(executable))
  config <- list(
    compiler = diagnostic_command(executable, c("CMD", "config", "CC"), timeout),
    active_cflags = diagnostic_command(executable, c("CMD", "config", "CFLAGS"), timeout),
    build_cflags = diagnostic_command(executable,
      c("CMD", "config", "--no-user-files", "--no-site-files", "CFLAGS"), timeout))
  environment <- Sys.getenv(c("_R_CHECK_COMPILATION_FLAGS_", "_R_CHECK_COMPILATION_FLAGS_KNOWN_",
    "_R_CHECK_SYSTEM_CLOCK_", "_R_CHECK_FUTURE_FILE_TIMESTAMPS_"), unset = "<unset>")
  check_profile <- Sys.getenv("R_CHECK_ENVIRON", unset = NA_character_)
  if (is.na(check_profile)) {
    candidates <- c(if (nzchar(.Platform$r_arch))
      paste0("~/.R/check.Renviron.", .Platform$r_arch), "~/.R/check.Renviron")
    existing <- Filter(file.exists, candidates)
    check_profile <- if (length(existing)) existing[[1L]] else "~/.R/check.Renviron"
  }
  runtime <- list(R = as.character(getRversion()), platform = R.version$platform,
    executable = executable, local_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    check_environment = environment, check_profile = check_profile)
  cat("R CMD check environment diagnosis\n",
    "R: ", runtime$R, "\nPlatform: ", runtime$platform,
    "\nExecutable: ", executable, "\nLocal UTC: ", runtime$local_utc, "\n", sep = "")
  invisible(lapply(names(config), function(name) {
    value <- config[[name]]
    cat("\n", name, " (exit ", value$status, "):\n", sep = "")
    cat(value$output, sep = "\n")
    cat("\n")
  }))
  cat("\nRelevant existing check settings:\n")
  cat(paste(names(environment), environment, sep = "="), sep = "\n")
  cat("\nUnset values above are outside R CMD check; --as-cran enables compilation-flag and timestamp checks.\n")
  cat("Check-specific environment file: ", if (nzchar(check_profile)) path.expand(check_profile)
    else "<disabled by empty R_CHECK_ENVIRON>", "\n", sep = "")
  cat("R CMD check loads that file if it exists; this utility does not load or change it.\n")
  target <- "-mno-omit-leaf-frame-pointer"
  has_flag <- function(value) any(grepl(paste0("(^|[[:space:]])", target,
    "($|[[:space:]])"), value$output))
  inherited <- identical(config$build_cflags$status, 0L) && has_flag(config$build_cflags)
  active <- identical(config$active_cflags$status, 0L) && has_flag(config$active_cflags)
  cat("\nFlag origin: ", if (inherited)
    "present in R's CFLAGS with user and site files disabled."
    else if (active) "present only when user/site configuration is enabled."
    else "not found in these CFLAGS; inspect 00install.out for the actual compiler command.", "\n", sep = "")
  if (inherited) cat(
    "R supports _R_CHECK_COMPILATION_FLAGS_KNOWN_ for verified flags inherited from its build.\n",
    "You may append this one flag for a local check, preserving any existing value.\n",
    "This utility does not change that setting, compiler flags or any check.\n", sep = "")
  distribution <- list()
  dpkg <- unname(Sys.which("dpkg-buildflags"))
  if (.Platform$OS.type != "windows" && nzchar(dpkg)) {
    distribution <- list(current_cflags = diagnostic_command(dpkg, c("--get", "CFLAGS"), timeout),
      origin = diagnostic_command(dpkg, c("--origin", "CFLAGS"), timeout))
    cat("\nCurrent dpkg CFLAGS policy (may differ from when R was built):\n")
    cat(distribution$current_cflags$output, sep = "\n")
    cat("\nPolicy origin: ", paste(distribution$origin$output, collapse = " "), "\n", sep = "")
  }
  clock_status <- NULL
  if (.Platform$OS.type == "windows") {
    w32tm <- unname(Sys.which("w32tm.exe"))
    if (nzchar(w32tm)) {
      clock_status <- diagnostic_command(w32tm, c("/query", "/status"), timeout)
      cat("\nWindows time-service status (read-only):\n")
      cat(clock_status$output, sep = "\n")
      cat("\n")
    }
  }
  services <- c("cran_headers", "timeapi", "worldtime_https", "worldtime_http", "worldclock_http")
  observations <- if (network) do.call(rbind, lapply(services,
    diagnostic_time_service, timeout = timeout)) else NULL
  if (network) {
    cat("\nPublic time-service observations:\n")
    invisible(lapply(seq_len(nrow(observations)), function(i) {
      cat(observations$service[[i]], ": ", if (observations$ok[[i]]) "REACHABLE" else "UNAVAILABLE",
        "\n  ", observations$url[[i]], "\n  ", observations$detail[[i]], "\n", sep = "")
    }))
    cat("R 4.5.0 tries the three worldtime/worldclock endpoints above.\n",
      "The reviewed R 4.6.2 checker starts with CRAN Date headers and timeapi instead.\n",
      "A returned header can be cached; a reachable endpoint is not proof that its timestamp is current.\n",
      "The R 4.5.0 NOTE means no usable remote time was obtained; it does not establish a wrong local clock.\n", sep = "")
  } else cat("\nNo network requests made. Add --network to probe R's public clock endpoints.\n")
  cat("\nNo files, compiler flags, check settings or system clocks were changed.\n")
  out <- list(runtime = runtime, config = config, inherited_leaf_flag = inherited,
    active_leaf_flag = active, distribution = distribution,
    windows_clock_status = clock_status, network = observations)
  stopifnot("diagnosis must return complete runtime and compiler evidence" =
    is.list(out) && is.list(out$runtime) && is.list(out$config) &&
      is.logical(out$inherited_leaf_flag) && length(out$inherited_leaf_flag) == 1L &&
      (is.null(out$network) || (is.data.frame(out$network) && nrow(out$network) == length(services))))
  invisible(out)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot("only --network is accepted" = all(args %in% "--network") && !anyDuplicated(args))
  check_environment(network = "--network" %in% args)
}
