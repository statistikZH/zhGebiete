test_that("test functionality of select_name", {
  # Import JSON
  json_string_1 <- '{"name":"züri","gemeinden":[{"gemeinde_code":261,"gemeinde_name":"Zürich"}]}'
  json_string_2 <- '{"name":"am albis","gemeinden":[{"gemeinde_code":1,"gemeinde_name":"Aeugst am Albis"},{"gemeinde_code":2,"gemeinde_name":"Affoltern am Albis"},{"gemeinde_code":4,"gemeinde_name":"Hausen am Albis"},{"gemeinde_code":6,"gemeinde_name":"Kappel am Albis"},{"gemeinde_code":14,"gemeinde_name":"Wettswil am Albis"},{"gemeinde_code":136,"gemeinde_name":"Langnau am Albis"}]}'

  data_1 <- jsonlite::fromJSON(json_string_1, simplifyVector = FALSE)
  data_2 <- jsonlite::fromJSON(json_string_2, simplifyVector = FALSE)
  nested_data <- list(data_1, data_2)

  make_reader <- function(vals) {
    i <- 1
    function(prompt = "") {
      v <- vals[i]
      i <<- i + 1
      v
    }
  }


  result_1 <- parse_to_df(select_name(list = data_1, selection = TRUE))

  result_2 <- with_mocked_bindings(
    parse_to_df(select_name(list = data_2, selection = TRUE)),
    readline = make_reader(c(NA, "0", "99", "", "1")),
    .package = "base"
  )

  raw_result_3 <- with_mocked_bindings(
    select_name(list = nested_data, selection = TRUE),
    readline = make_reader(c(NA, "0", "99", "", "1")),
    .package = "base"
  )
  result_3 <- lapply(raw_result_3, parse_to_df)


  expect_s3_class(result_1, "data.frame")
  expect_equal(result_1$gemeinde_code, 261)

  expect_s3_class(result_2, "data.frame")
  expect_equal(result_2$gemeinde_code, 1)

  expect_type(result_3, "list")
  expect_equal(result_3[[1]]$gemeinde_code, 261)
  expect_equal(result_3[[2]]$gemeinde_code, 1)
})


test_that("check if select_name performs the error handling correctly", {
  json_string_1 <- '{"name":"test","gemeinden":[],"error":"Kein Treffer"}'
  data_1 <- jsonlite::fromJSON(json_string_1, simplifyVector = FALSE)

  expect_error(
    select_name(data_1),
    "Eine Filteroption liefert keinen Treffer."
  )
})


test_that("check if select_name performs the error handling correctly", {
  # JSON auf das neue Format 'gemeinden' angepasst
  json_string_1 <- '{"name":"iöhoasdhjilöfadsjklghads","gemeinden":[],"error":"Kein Treffer gefunden"}'
  data_1 <- jsonlite::fromJSON(json_string_1)

  # Test for errors
  expect_error(
    select_name(data_1),
    "Eine Filteroption liefert keinen Treffer."
  )
})

