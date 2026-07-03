test_that("get_health works against the live Gebietsstammdaten API", {
  skip_if_not_local()

  result <- get_health()
  expect_equal(result$status, "healthy")
})
