test_that("test functionality of select_name", {
  # Import JSON from API string
  json_string_1 <- '{"name":"züri","treffer":[{"gebietstyp_code":3,"gemeinde_code":261,"gemeinde_name":"Zürich"}]}'
  json_string_2 <- '{"name":"am albis","treffer":[{"gebietstyp_code":3,"gemeinde_code":1,"gemeinde_name":"Aeugst am Albis"},{"gebietstyp_code":3,"gemeinde_code":2,"gemeinde_name":"Affoltern am Albis"},{"gebietstyp_code":3,"gemeinde_code":4,"gemeinde_name":"Hausen am Albis"},{"gebietstyp_code":3,"gemeinde_code":6,"gemeinde_name":"Kappel am Albis"},{"gebietstyp_code":3,"gemeinde_code":14,"gemeinde_name":"Wettswil am Albis"},{"gebietstyp_code":3,"gemeinde_code":136,"gemeinde_name":"Langnau am Albis"}]}'

  # Convert to R object
  data_1 <- jsonlite::fromJSON(json_string_1)
  data_2 <- jsonlite::fromJSON(json_string_2)
  nested_data <- list(data_1, data_2) # Combine into list

  # Implement a function that passes different values to the realine function
  make_reader <- function(vals) {
    i <- 1
    function(prompt = "") {
      v <- vals[i]; i <<- i + 1; v
    }
  }

  # Mock the console input
  result_1 <- select_name(list = data_1)
  result_2 <- with_mocked_bindings(
    select_name(list = data_2),
    readline = make_reader(c(NA, "0", "99", "", "1")),
    .package = "base"
  )
  result_3 <- with_mocked_bindings(
    select_name(list = nested_data),
    readline = make_reader(c(NA, "0", "99", "", "1")),
    .package = "base"
  )


  # Perform tests
  expect_s3_class(result_1, "data.frame")
  expect_equal(nrow(result_1), 1)
  expect_equal(ncol(result_1), 3)
  expect_equal(result_1$gebietstyp_code, 3)
  expect_equal(result_1$gemeinde_code, 261)
  expect_equal(result_1$gemeinde_name, "Zürich")

  expect_s3_class(result_2, "data.frame")
  expect_equal(nrow(result_2), 1)
  expect_equal(ncol(result_2), 3)
  expect_equal(result_2$gebietstyp_code, 3)
  expect_equal(result_2$gemeinde_code, 1)
  expect_equal(result_2$gemeinde_name, "Aeugst am Albis")

  expect_type(result_3, "list")
  expect_equal(nrow(result_3[[1]]), 1)
  expect_equal(ncol(result_3[[1]]), 3)
  expect_equal(result_3[[1]]$gebietstyp_code, 3)
  expect_equal(result_3[[1]]$gemeinde_code, 261)
  expect_equal(result_3[[1]]$gemeinde_name, "Zürich")
  expect_equal(nrow(result_3[[2]]), 1)
  expect_equal(ncol(result_3[[2]]), 3)
  expect_equal(result_3[[2]]$gebietstyp_code, 3)
  expect_equal(result_3[[2]]$gemeinde_code, 1)
  expect_equal(result_3[[2]]$gemeinde_name, "Aeugst am Albis")
})

test_that("check if select_name performs the error handling correctly", {
  # Import JSON from API string
  json_string_1 <- '{"name":"iöhoasdhjilöfadsjklghads","treffer":[],"error":"Kein Treffer gefunden"}'

  # Convert to R object
  data_1 <- jsonlite::fromJSON(json_string_1)

  # Test for errors
  expect_error(
    select_name(data_1),
    "Eine Filteroption liefert keinen treffer."
  )
})
