test_that("get_health works against the live Gebietsstammdaten API", {
  result <- get_health()
  expect_equal(result$status, "healthy")
})

test_that("get_gebiete works against the live Gebietsstammdaten API", {
  result <- get_gebiete()
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("gebietstyp_code", "gebietstyp_name", "gebiet_code", "gebiet_name")
  )
  expect_equal(ncol(result), 4)
  expect_all_true(nrow(result) > 0)
})

test_that("get_gebietstypen works against the live Gebietsstammdaten API", {
  result <- get_gebietstypen()
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("gebietstyp_code", "gebietstyp_name"))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 4)
})

test_that("get_gemeindemutationen works against the live Gebietsstammdaten API", {
  result <- get_gemeindemutationen()
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c(
      "mutationstyp",
      "gemeinde_code_alt",
      "gemeinde_name_alt",
      "gemeinde_code_neu",
      "gemeinde_name_neu",
      "mutationsdatum"
    )
  )
  expect_equal(ncol(result), 6)
  expect_all_true(nrow(result) > 0)
})

test_that("get_gemeinden works against the live Gebietsstammdaten API", {
  result <- get_gemeinden(gemeinde_name = "züri")
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("gebietstyp_code", "gemeinde_code", "gemeinde_name")
  )
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 1)
})

test_that("get_bezirke works against the live Gebietsstammdaten API", {
  result <- get_bezirke(bezirk_name = "züri")
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("gebietstyp_code", "bezirk_code", "bezirk_name")
  )
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 1)
})

test_that("get_raumplanungsregionen works against the live Gebietsstammdaten API", {
  result <- get_raumplanungsregionen(raumplanungsregion_name = "züri")
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("gebietstyp_code", "raumplanungsregion_code", "raumplanungsregion_name")
  )
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 1)
})

test_that("get_gemeindezuweisungen works against the live Gebietsstammdaten API", {
  result <- get_gemeindezuweisungen(gemeinde_code = 111)
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c(
      "gebietstyp_code",
      "gemeinde_code",
      "gemeinde_name",
      "bezirk_code",
      "bezirk_name",
      "raumplanungsregion_code",
      "raumplanungsregion_name"
    )
  )
  expect_equal(ncol(result), 7)
  expect_equal(nrow(result), 1)
})

test_that("gget_gemeindenhist works against the live Gebietsstammdaten API", {
  result <- get_gemeindenhist(jahr = 2025, gemeinde_code = 111)
  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c(
      "gebietstyp_code",
      "gemeinde_code",
      "gemeinde_name",
      "jahr"
    )
  )
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 1)
})
