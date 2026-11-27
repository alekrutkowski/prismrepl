# Keep the preference override alive until the entire test suite finishes.
# A helper file's default withr cleanup scope ends before the tests run.
# Explicit temporary preference files in persistence tests still work normally.
local({
  teardown <- testthat::teardown_env()
  directory <- withr::local_tempdir("prism-test-settings-", .local_envir = teardown)
  withr::local_envvar(c(PRISMREPL_CONFIG_FILE = file.path(directory, "preferences.dcf"),
                      PRISMREPL_COMMANDS_FILE = file.path(directory, "commands.rds")),
    .local_envir = teardown)
})
