skip_if_no_live_api <- function() {
  # List all the cases that skip the integration tests
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()
}
