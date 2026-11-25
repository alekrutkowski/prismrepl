expected <- Sys.getenv("EXPECTED_ARCH")
actual <- if (grepl("aarch64|arm64", R.version$arch, ignore.case = TRUE)) {
  "arm64"
} else if (grepl("x86_64|x64|amd64", R.version$arch, ignore.case = TRUE)) "x86_64" else "unknown"
stopifnot("R must be 64-bit" = .Machine$sizeof.pointer == 8L,
          "R must satisfy the package minimum of 4.5.0" = getRversion() >= "4.5.0",
          "expected architecture must be explicit" = expected %in% c("arm64", "x86_64"),
          "runner must use native R for the requested architecture" = identical(actual, expected))
cat(R.version.string, "\n", R.version$platform, "\n", actual, "\n", sep = "")

