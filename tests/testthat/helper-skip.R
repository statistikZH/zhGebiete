skip_if_not_local <- function() {
  # List all the cases that skip the integration tests
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()
}
