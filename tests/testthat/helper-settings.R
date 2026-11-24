# Tests never read or write the developer's preferences.
withr::local_envvar(c(PRISMREPL_CONFIG_FILE = tempfile("prism-test-defaults-")))
