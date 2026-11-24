test_that("CRAN completion reads and sorts cached data only when the file changes", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  repository <- "https://cloud.r-project.org"
  writeLines(c(paste0("Repository: ", repository), "zoo", "cli", "data.table"), path)
  original <- .read_cran_index
  reads <- 0L
  local_mocked_bindings(.read_cran_index = function(path, repository) {
    reads <<- reads + 1L
    original(path, repository)
  })
  first <- .cached_cran_index(path, repository)
  expect_identical(first$names, c("cli", "data.table", "zoo"))
  expect_identical(.cached_cran_index(path, repository), first)
  expect_identical(.cached_cran_index(path, repository), first)
  expect_identical(reads, 1L)
  writeLines(c(paste0("Repository: ", repository), "data.table", "mirai"), path)
  expect_identical(.cached_cran_index(path, repository)$names, c("data.table", "mirai"))
  expect_identical(reads, 2L)
  expect_null(.cached_cran_index(path, "https://another.example.org"))
  expect_identical(reads, 3L)
  unlink(path)
  expect_null(.cached_cran_index(path, repository))
  expect_identical(reads, 4L)
  expect_null(.cached_cran_index(path, repository))
  expect_identical(reads, 4L)
})
