#' Create a Fake Managed Worker Without Starting a Process or Using a Repository
#' @param alive Initial process state.
#' @param kill_exits Whether kill immediately changes the process state.
#' @param status Simulated exit status.
#' @param .local_envir Test scope owning temporary files.
#' @return Worker, owner, and observable process state.
cran_worker_fixture <- function(alive = TRUE, kill_exits = TRUE, status = 0L,
                                .local_envir = parent.frame()) {
  stopifnot("fake flags must be logical" = .is_flag(alive) && .is_flag(kill_exits),
            "test scope must be an environment" = is.environment(.local_envir))
  root <- withr::local_tempdir("cran worker ", .local_envir = .local_envir)
  worker <- new.env(parent = emptyenv())
  worker$directory <- file.path(root, "job")
  dir.create(worker$directory)
  worker$repository <- "https://example.invalid"
  worker$destination <- file.path(root, "cache.txt")
  worker$key <- paste(worker$repository, worker$destination, sep = "\n")
  worker$started <- as.numeric(Sys.time())
  worker$task <- file.path(worker$directory, "task.dcf")
  worker$log <- file.path(worker$directory, "worker.log")
  worker$done <- file.path(worker$directory, "done.txt")
  worker$result <- file.path(worker$directory, "index.txt")
  process <- new.env(parent = emptyenv())
  process$alive <- alive
  process$events <- character()
  worker$process <- list(
    is_alive = function() process$alive,
    kill = function(grace) {
      expect_true(dir.exists(worker$directory))
      process$events <- c(process$events, "kill")
      process$grace <- grace
      if (kill_exits) process$alive <- FALSE
      invisible(TRUE)
    },
    wait = function(timeout) {
      expect_true(dir.exists(worker$directory))
      process$events <- c(process$events, paste0("wait:", timeout))
      invisible(NULL)
    },
    get_exit_status = function() if (process$alive) NULL else status)
  owner <- new.env(parent = emptyenv())
  owner$worker <- worker
  owner$key <- worker$key
  out <- list(worker = worker, owner = owner, process = process)
  stopifnot("fixture must retain the owned worker" = identical(out$owner$worker, out$worker))
  out
}

test_that("launch failure removes all scratch before returning without ownership", {
  owner <- new.env(parent = emptyenv())
  directory <- NULL
  local_mocked_bindings(.package_index = owner,
    .new_cran_process = function(executable, script, task, log) {
      directory <<- dirname(task)
      expect_true(file.exists(task))
      writeLines("partial launch log", log)
      stop("launch failed")
    })
  expect_error(.start_cran_index("https://example.invalid", tempfile()), "launch failed")
  expect_false(dir.exists(directory))
  expect_null(owner$worker)
  expect_length(owner$retired, 0L)
})

test_that("a task-write error rolls back before a child is started", {
  owner <- new.env(parent = emptyenv())
  directory <- NULL
  local_mocked_bindings(.package_index = owner,
    .new_cran_process = function(...) stop("a child must not be started"))
  start <- .start_cran_index
  environment(start) <- list2env(list(write.dcf = function(x, file, ...) {
    directory <<- dirname(file)
    writeLines("partial task", file)
    stop("task write failed")
  }), parent = environment(start))
  expect_error(start("https://example.invalid", tempfile()), "task write failed")
  expect_false(dir.exists(directory))
  expect_null(owner$worker)
  expect_length(owner$retired, 0L)
})

test_that("successful construction transfers ownership and preserves long literal paths", {
  fixture <- cran_worker_fixture()
  unlink(fixture$worker$directory, recursive = TRUE)
  fixture$owner$worker <- NULL
  repository <- paste0("file://", paste(rep("long path segment", 12L), collapse = "/"))
  local_mocked_bindings(.package_index = fixture$owner,
    .new_cran_process = function(executable, script, task, log) {
      fields <- as.list(read.dcf(task, all = TRUE)[1L, ])
      expect_identical(fields$Repository, repository)
      expect_false(grepl("\n", fields$Result, fixed = TRUE))
      fixture$worker$directory <- dirname(task)
      fixture$worker$process
    })
  worker <- .start_cran_index(repository, fixture$worker$destination)
  on.exit(.cleanup_cran_workers(wait = TRUE, state = fixture$owner), add = TRUE)
  expect_identical(fixture$owner$worker, worker)
  expect_identical(fixture$owner$key, worker$key)
  expect_true(dir.exists(worker$directory))
  expect_true(file.exists(worker$task))
  expect_true(.cran_worker_alive(worker))
})

test_that("polling a running job never waits or trusts an early completion marker", {
  fixture <- cran_worker_fixture()
  worker <- fixture$worker
  writeLines("ok", worker$done)
  writeLines(c(paste0("Repository: ", worker$repository), "cli"), worker$result)
  .poll_cran_index(fixture$owner)
  expect_identical(fixture$owner$worker, worker)
  expect_length(fixture$process$events, 0L)
  expect_true(file.exists(worker$result))
  expect_false(file.exists(worker$destination))
})

test_that("successful completion validates and publishes once before cleanup", {
  fixture <- cran_worker_fixture(alive = FALSE)
  worker <- fixture$worker
  writeLines("ok", worker$done)
  writeLines(c(paste0("Repository: ", worker$repository), "zoo", "cli", "cli"), worker$result)
  writes <- 0L
  original <- .write_cran_cache
  local_mocked_bindings(.write_cran_cache = function(...) {
    writes <<- writes + 1L
    original(...)
  })
  .poll_cran_index(fixture$owner)
  .poll_cran_index(fixture$owner)
  expect_identical(writes, 1L)
  expect_identical(.read_cran_index(worker$destination, worker$repository)$names, c("cli", "zoo"))
  expect_false(fixture$owner$failed)
  expect_null(fixture$owner$worker)
  expect_false(dir.exists(worker$directory))
  expect_identical(fixture$process$events, "wait:0")
})

test_that("a failed child keeps the earlier cache and releases its job directory", {
  fixture <- cran_worker_fixture(alive = FALSE, status = 1L)
  worker <- fixture$worker
  old <- c(paste0("Repository: ", worker$repository), "oldPackage")
  writeLines(old, worker$destination)
  writeLines("failed: offline repository", worker$done)
  .poll_cran_index(fixture$owner)
  expect_identical(readLines(worker$destination), old)
  expect_true(fixture$owner$failed)
  expect_match(fixture$owner$failure, "offline repository", fixed = TRUE)
  expect_null(fixture$owner$worker)
  expect_false(dir.exists(worker$directory))
})

test_that("an early worker crash is detected even without a completion marker", {
  fixture <- cran_worker_fixture(alive = FALSE, status = 1L)
  .poll_cran_index(fixture$owner)
  expect_true(fixture$owner$failed)
  expect_match(fixture$owner$failure, "without a result", fixed = TRUE)
  expect_null(fixture$owner$worker)
  expect_false(dir.exists(fixture$worker$directory))
})

test_that("invalid and obsolete completed results cannot replace a valid cache", {
  fixture <- cran_worker_fixture(alive = FALSE)
  worker <- fixture$worker
  old <- c(paste0("Repository: ", worker$repository), "oldPackage")
  writeLines(old, worker$destination)
  writeLines("ok", worker$done)
  writeLines(c("Repository: https://other.invalid", "newPackage"), worker$result)
  fixture$owner$key <- "another job"
  expect_false(.publish_cran_index(worker, fixture$owner))
  expect_identical(readLines(worker$destination), old)
  fixture$owner$key <- worker$key
  .poll_cran_index(fixture$owner)
  expect_true(fixture$owner$failed)
  expect_match(fixture$owner$failure, "validation")
  expect_identical(readLines(worker$destination), old)
  expect_false(dir.exists(worker$directory))
})

test_that("timeout kills first and retains files until a delayed exit is reaped", {
  fixture <- cran_worker_fixture(kill_exits = FALSE)
  worker <- fixture$worker
  .poll_cran_index(fixture$owner, now = worker$started + 121)
  expect_identical(fixture$process$events, c("kill", "wait:0"))
  expect_identical(fixture$process$grace, 0)
  expect_true(fixture$owner$failed)
  expect_identical(fixture$owner$failure, "timed out")
  expect_null(fixture$owner$worker)
  expect_identical(fixture$owner$retired, list(worker))
  expect_true(dir.exists(worker$directory))
  fixture$process$alive <- FALSE
  expect_identical(.reap_cran_workers(state = fixture$owner), 0L)
  expect_false(dir.exists(worker$directory))
  expect_length(fixture$owner$retired, 0L)
})

test_that("repository switching cancels an owned job and cannot publish its late result", {
  fixture <- cran_worker_fixture()
  worker <- fixture$worker
  writeLines(c(paste0("Repository: ", worker$repository), "latePackage"), worker$result)
  local_mocked_bindings(.package_index = fixture$owner,
    .cran_repository = function() "https://new.invalid",
    .cached_cran_index = function(...) NULL)
  result <- .cran_package_names(start = FALSE)
  expect_length(result$names, 0L)
  expect_false(fixture$process$alive)
  expect_null(fixture$owner$worker)
  expect_false(dir.exists(worker$directory))
  expect_false(.publish_cran_index(worker, fixture$owner))
  expect_false(file.exists(worker$destination))
})

test_that("shutdown cleanup kills and reaps before unlink and is idempotent", {
  fixture <- cran_worker_fixture()
  expect_identical(.cleanup_cran_workers(wait = TRUE, state = fixture$owner), 0L)
  expect_identical(fixture$process$events, c("kill", "wait:1000"))
  expect_identical(fixture$process$grace, 0.1)
  expect_false(dir.exists(fixture$worker$directory))
  expect_identical(.cleanup_cran_workers(wait = TRUE, state = fixture$owner), 0L)
  expect_identical(fixture$process$events, c("kill", "wait:1000"))
})

test_that("a staged Windows-style replacement restores the old cache on failure", {
  directory <- withr::local_tempdir()
  destination <- file.path(directory, "cache.txt")
  old <- c("Repository: https://example.invalid", "oldPackage")
  writeLines(old, destination)
  replace <- .write_cran_cache
  calls <- 0L
  environment(replace) <- list2env(list(file.rename = function(from, to) {
    calls <<- calls + 1L
    if (calls %in% c(1L, 3L)) FALSE else base::file.rename(from, to)
  }), parent = environment(replace))
  expect_error(replace("newPackage", "https://example.invalid", destination), "Cannot publish")
  expect_identical(calls, 4L)
  expect_identical(readLines(destination), old)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "cache.txt")
})

test_that("a Windows-style replacement removes the backup after success", {
  directory <- withr::local_tempdir()
  destination <- file.path(directory, "cache.txt")
  writeLines(c("Repository: https://example.invalid", "oldPackage"), destination)
  replace <- .write_cran_cache
  calls <- 0L
  environment(replace) <- list2env(list(file.rename = function(from, to) {
    calls <<- calls + 1L
    if (calls == 1L) FALSE else base::file.rename(from, to)
  }), parent = environment(replace))
  expect_identical(replace("newPackage", "https://example.invalid", destination), destination)
  expect_identical(.read_cran_index(destination, "https://example.invalid")$names, "newPackage")
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "cache.txt")
})

test_that("a failed rollback retains a complete recovery copy and removes staging", {
  directory <- withr::local_tempdir()
  destination <- file.path(directory, "cache.txt")
  old <- c("Repository: https://example.invalid", "oldPackage")
  writeLines(old, destination)
  replace <- .write_cran_cache
  calls <- 0L
  environment(replace) <- list2env(list(file.rename = function(from, to) {
    calls <<- calls + 1L
    if (calls == 2L) base::file.rename(from, to) else FALSE
  }), parent = environment(replace))
  expect_warning(expect_error(replace("newPackage", "https://example.invalid", destination),
    "Cannot publish"), "retained at", fixed = TRUE)
  files <- list.files(directory, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  expect_length(files, 1L)
  expect_match(basename(files), "^\\.prism-cran-backup-")
  expect_identical(readLines(files), old)
})

test_that("staging errors remove partial files without touching the prior cache", {
  directory <- withr::local_tempdir()
  destination <- file.path(directory, "cache.txt")
  old <- c("Repository: https://example.invalid", "oldPackage")
  writeLines(old, destination)
  replace <- .write_cran_cache
  environment(replace) <- list2env(list(writeLines = function(text, con, ...) {
    base::writeLines("partial stage", con)
    stop("stage write failed")
  }), parent = environment(replace))
  expect_error(replace("newPackage", "https://example.invalid", destination), "stage write failed")
  expect_identical(readLines(destination), old)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "cache.txt")
})

test_that("package-name completions agree with available names without attaching packages", {
  before_search <- search()
  before_namespaces <- loadedNamespaces()
  expected <- sort(unique(base::.packages(all.available = TRUE)))
  attached <- complete_r(":attach ", limit = length(expected) + 1L)$candidates
  documented <- complete_r(":docs ", limit = length(expected) + 1L)$candidates
  expect_identical(attached, expected)
  expect_identical(documented, expected)
  expect_identical(search(), before_search)
  expect_setequal(loadedNamespaces(), before_namespaces)
})
