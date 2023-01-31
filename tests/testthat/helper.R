test_model <- function(file) test_path("models", file)

expect_error_free <- function(...) {
  expect_error(..., regexp = NA)
}
