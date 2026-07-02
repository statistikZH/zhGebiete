test_that("test get_base_URL", {
  # Base URL must not change, else it must be a wanted change including
  # a change of this test
  URL_loaded <- get_base_URL()

  expect_equal(URL_loaded, "https://gebietsstammdaten.statistik.zh.ch/api")
})

test_that("generate_api_request request object has correct attributes", {
  # Mock base url so that in case of a change not all URLS need to be changed
  local_mocked_bindings(
    get_base_URL = function() "https://gebietsstammdaten.statistik.zh.ch/api"
  )

  # Perform a request that can happen during a single get_gemeindehist() call
  req <- generate_api_request(
    endpoint = "gemeindenhist",
    jahr = 2025,
    code = 111
  )

  # Verify object attribute
  expect_s3_class(req, "httr2_request")
  # Verify if the URL and the query parameters are handled correctly
  expect_equal(
    req$url,
    "https://gebietsstammdaten.statistik.zh.ch/api/gemeindenhist?jahr=2025&code=111"
  )
  # Verify the accepted header
  expect_equal(req$headers$Accept, "application/json")
})

test_that("call_and_parse return object are correctly treated", {
  fake_response <- httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(
      '{"gebietstypen":[{"gebietstyp_code":1,"gebietstyp_name":"Kanton"},{"gebietstyp_code":2,"gebietstyp_name":"Bezirk"},{"gebietstyp_code":3,"gebietstyp_name":"Gemeinde"},{"gebietstyp_code":6,"gebietstyp_name":"Raumplanungsregion"}]}'
    )
  )

  local_mocked_bindings(
    req_perform = function(req, ...) fake_response,
    .package = "httr2"
  )

  result <- call_and_parse(httr2::request("https://example.com"))

  # Data type
  expect_type(result, "list")

  # In the first element a df
  expect_s3_class(result$gebietstypen, "data.frame")
  expect_equal(nrow(result$gebietstypen), 4) # 4x rows
  expect_equal(ncol(result$gebietstypen), 2) # 2x cols

  # Check the content
  expect_equal(result$gebietstypen$gebietstyp_code, c(1, 2, 3, 6))
  expect_equal(
    result$gebietstypen$gebietstyp_name,
    c("Kanton", "Bezirk", "Gemeinde", "Raumplanungsregion")
  )
})

test_that("Test the error handling of check_input_param", {
  # No imput = no error
  expect_no_error(check_input_param())

  # Error handling two parameters
  expect_error(
    check_input_param(code = 111, name = "züri"),
    "either `code` or `name` must be NULL"
  )

  # Error handling for code
  expect_error(
    check_input_param(code = "111"),
    "`code` must be numeric or NULL"
  )
  expect_error(check_input_param(code = 111.75), "`code` must be a intager")
  expect_error(check_input_param(code = 0), "`code` must be larger than 0")

  # Error handling for name
  expect_error(check_input_param(name = 42), "`name` must be a character")

  # Error handling for year
  expect_error(
    check_input_param(jahr = "1991"),
    "`jahr` must be numeric or NULL"
  )
  expect_error(check_input_param(jahr = 1991.91), "`jahr` must be a intager")
  expect_error(
    check_input_param(jahr = 1989),
    "`jahr` must be larger or equal than 1990"
  )
  expect_error(
    check_input_param(jahr = 9989),
    "`jahr` must be smaler or equal the current jahr"
  )

  # check if current year is not a problem
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_no_error(check_input_param(jahr = current_year))
})

test_that("api_call chains generate_api_request and call_and_parse correctly", {
  # Mock a function that just passes arguments
  local_mocked_bindings(
    generate_api_request = function(endpoint, ...) {
      list(endpoint = endpoint, dots = list(...))
    },
    call_and_parse = function(request) request
  )

  result <- api_call("gemeinden", code = 111)

  expect_equal(result$endpoint, "gemeinden")
  expect_equal(result$dots, list(code = 111))
})

test_that("api_calls dispatches correctly based on vector-length branching", {
  # Mock a function that just passes arguments
  local_mocked_bindings(
    api_call = function(endpoint, jahr = NULL, code = NULL, name = NULL) {
      list(endpoint = endpoint, jahr = jahr, code = code, name = name)
    }
  )

  # Case: jahr vector -> recurses via api_calls itself
  res_jahr <- api_calls("gemeindenhist", jahr = c(2022, 2023))
  expect_equal(length(res_jahr), 2)
  expect_equal(res_jahr[[1]]$jahr, 2022)
  expect_equal(res_jahr[[2]]$jahr, 2023)

  # Case: code vector -> calls api_call directly
  res_code <- api_calls("gemeinden", code = c(111, 112))
  expect_equal(length(res_code), 2)
  expect_equal(res_code[[1]]$code, 111)
  expect_equal(res_code[[2]]$code, 112)

  # Case: name vector -> calls api_call directly
  res_name <- api_calls("gemeinden", name = c("züri", "winti"))
  expect_equal(length(res_name), 2)
  expect_equal(res_name[[1]]$name, "züri")
  expect_equal(res_name[[2]]$name, "winti")

  # Case: no vectors -> single api_call
  res_single <- api_calls("gemeindenhist", jahr = 2024)
  expect_equal(res_single, list(endpoint = "gemeindenhist", jahr = 2024, code = NULL, name = NULL))
})
