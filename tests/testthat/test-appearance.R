
if (requireNamespace("lintr", quietly = TRUE)) {
  skip_on_appveyor()
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
