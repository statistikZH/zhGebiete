test_that("test functionality of parse_to_df", {
  # Import JSON from API string
  json_string_1 <- '{"gemeinde":{"gebietstyp_code":3,"gemeinde_code":111,"gemeinde_name":"Bäretswil"}}'
  json_string_2 <- '{"gemeinde":{"gebietstyp_code":3,"gemeinde_code":112,"gemeinde_name":"Bubikon"}}'

  # Convert to R object
  data_1 <- jsonlite::fromJSON(json_string_1)
  data_2 <- jsonlite::fromJSON(json_string_2)
  nested_data <- list(data_1, data_2) # Combine into list

  result_1 <- parse_to_df(list = data_1) # Remove "gemeinden"
  result_2 <- parse_to_df(list = nested_data) # Remove "gemeinden"

  # Test the structure and content of the data frame
  expect_s3_class(result_1, "data.frame")
  expect_equal(nrow(result_1), 1)
  expect_equal(ncol(result_1), 3)
  expect_equal(result_1$gebietstyp_code, 3)
  expect_equal(result_1$gemeinde_code, 111)
  expect_equal(result_1$gemeinde_name, "Bäretswil")

  expect_s3_class(result_2, "data.frame")
  expect_equal(nrow(result_2), 2)
  expect_equal(ncol(result_2), 3)
  expect_equal(result_2$gebietstyp_code, c(3, 3))
  expect_equal(result_2$gemeinde_code, c(111, 112))
  expect_equal(result_2$gemeinde_name, c("Bäretswil", "Bubikon"))
})

test_that("check if parse_to_df performs the error handling correctly", {
  # Import JSON from API string
  json_string_1 <- '{"name":"iöhoasdhjilöfadsjklghads","treffer":[],"error":"Kein Treffer gefunden"}'
  json_string_2 <- '{"name":"züri","treffer":[{"gebietstyp_code":3,"gemeinde_code":261,"gemeinde_name":"Zürich"}]}'

  # Convert to R object
  data_1 <- jsonlite::fromJSON(json_string_1)
  data_2 <- jsonlite::fromJSON(json_string_2)
  nested_data <- list(data_1, data_2) # Combine into list

  # Test for errors
  expect_error(parse_to_df(data_1), "Kein Treffer gefunden.")
  expect_error(
    parse_to_df(nested_data),
    "Eine Filteroption liefert keinen treffer."
  )
})

test_that("test the functionality of remove_gemeinden", {
  # Import JSON from API string
  json_string_1 <- '{"bezirk":{"gebietstyp_code":2,"bezirk_code":111,"bezirk_name":"Dietikon"},"gemeinden":[{"gemeinde_code":241,"gemeinde_name":"Aesch (ZH)"},{"gemeinde_code":242,"gemeinde_name":"Birmensdorf (ZH)"},{"gemeinde_code":243,"gemeinde_name":"Dietikon"},{"gemeinde_code":244,"gemeinde_name":"Geroldswil"},{"gemeinde_code":245,"gemeinde_name":"Oberengstringen"},{"gemeinde_code":246,"gemeinde_name":"Oetwil an der Limmat"},{"gemeinde_code":247,"gemeinde_name":"Schlieren"},{"gemeinde_code":248,"gemeinde_name":"Uitikon"},{"gemeinde_code":249,"gemeinde_name":"Unterengstringen"},{"gemeinde_code":250,"gemeinde_name":"Urdorf"},{"gemeinde_code":251,"gemeinde_name":"Weiningen (ZH)"}]}'
  json_string_2 <- '{"bezirk":{"gebietstyp_code":2,"bezirk_code":112,"bezirk_name":"Zürich"},"gemeinden":[{"gemeinde_code":261,"gemeinde_name":"Zürich"}]}'

  # Convert to R object
  data_1 <- jsonlite::fromJSON(json_string_1)
  data_2 <- jsonlite::fromJSON(json_string_2)
  nested_data <- list(data_1, data_2) # Combine into list

  result_1 <- remove_gemeinden(list = data_1) # Remove "gemeinden"
  result_2 <- remove_gemeinden(list = nested_data) # Remove "gemeinden"

  # Test if "gemeinden" is removed and rest is as it should be
  expect_type(result_1, "list")
  expect_all_true(is.null(result_1$gemeinden))
  expect_equal(length(result_1), 1)
  expect_equal(result_1$bezirk$gebietstyp_code, 2)
  expect_equal(result_1$bezirk$bezirk_code, 111)
  expect_equal(result_1$bezirk$bezirk_name, "Dietikon")

  expect_type(result_2, "list")
  expect_all_true(is.null(result_2[[1]]$gemeinden))
  expect_all_true(is.null(result_2[[1]]$gemeinden))
  expect_equal(length(result_2[[1]]), 1)
  expect_equal(length(result_2[[2]]), 1)
  expect_equal(result_2[[1]]$bezirk$gebietstyp_code, 2)
  expect_equal(result_2[[2]]$bezirk$gebietstyp_code, 2)
  expect_equal(result_2[[1]]$bezirk$bezirk_code, 111)
  expect_equal(result_2[[2]]$bezirk$bezirk_code, 112)
  expect_equal(result_2[[1]]$bezirk$bezirk_name, "Dietikon")
  expect_equal(result_2[[2]]$bezirk$bezirk_name, "Zürich")
})
