#' Base URL definieren
#'
#' @description Festlegung der Base URL für die Gebietsstammdaten-API.
#'
#' @returns Base URL
#'
#' @keywords internal
get_base_URL <- function() {
  return("https://gebietsstammdaten.statistik.zh.ch/api")
}

#' Request-Objekt erstellen
#'
#' @description Request-Objekt erstellen mit angegebener URL-Endpunkt und
#' gegebenenfalls Filtern.
#'
#' @param endpoint URL-Endpunkt
#' @param ... Zusätzliche Komponenten `query parameter = value`. Z.B.: code = 111,
#' name = "Züri" oder jahr = 1991.
#'
#' @returns Ein Request-Objekt.
#'
#' @keywords internal
generate_api_request <- function(endpoint, ...) {
  req <- httr2::request(base_url = get_base_URL()) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(...) |> # Append additional variables
    # Strict header format handling
    httr2::req_headers(Accept = "application/json") |>
    # Visualize the error handling
    httr2::req_error(is_error = \(resp) httr2::resp_status(resp) >= 400) |>
    # Define retry and delay with an exponential growth (2, 4, 8, 16, 32 sec)
    httr2::req_retry(max_tries = 5, backoff = \(tries) 2^tries)

  return(req)
}

#' API-Request senden und parsen
#'
#' @description API-Request senden und JSON-Response parsen.
#'
#' @param request Request-Objekt von `generate_api_request()`.
#'
#' @returns JSON-Response als Dataframe oder Liste.
#'
#' @keywords internal
call_and_parse <- function(request) {
  call <- httr2::req_perform(req = request) |>
    httr2::resp_body_json(simplifyVector = TRUE, flatten = TRUE)
  return(call)
}

#' Fehler abfangen
#'
#' @description In dieser Funktion werden mögliche fehlerhafte Eingaben
#' abgefangen.
#'
#' @param jahr Jahr von interesse.
#' @param code Code der Gemeinde, Region oder Raumplanungsregion.
#' @param name Name der Gemeinde, Region oder Raumplanungsregion.
#'
#' @returns Kein Rückgabeobjekt.
#'
#' @keywords internal
check_input_param <- function(jahr = NULL, code = NULL, name = NULL) {
  stopifnot(
    "either `code` or `name` must be NULL" = (is.null(code) | is.null(name))
  )
  stopifnot(
    "`code` must be numeric or NULL" = (is.null(code) | is.numeric(code))
  )
  stopifnot("`code` must be a intager" = (code %% 1 == 0))
  stopifnot("`code` must be larger than 0" = (code > 0))
  stopifnot("`name` must be a character" = (is.null(name) | is.character(name)))
  stopifnot(
    "`jahr` must be numeric or NULL" = (is.null(jahr) | is.numeric(jahr))
  )
  stopifnot("`jahr` must be a intager" = (jahr %% 1 == 0))
  stopifnot("`jahr` must be larger or equal than 1990" = (jahr >= 1990))
  stopifnot(
    "`jahr` must be smaler or equal the current jahr" = (jahr <=
      format(Sys.Date(), "%Y"))
  )
  return(invisible())
}

#' API-Request ausführen
#'
#' @description Funktion zum Abfragen sämtlicher API Endpunkte
#' \itemize{
#' \item \code{api_call()}: Für einzellne abfragen.
#' \item \code{api_calls()}: Für deb Fall dass \code{jahr}, \code{code} oder
#'  \code{name} nicht \code{NULL} ist.
#' }
#' @inheritParams generate_api_request
#'
#' @returns JSON-Response als Dataframe oder Liste.
#'
#' @keywords internal
api_call <- function(endpoint, ...) {
  # Get data from API
  res <- generate_api_request(endpoint = endpoint, ...) |>
    call_and_parse()
  return(res)
}

#' @rdname api_call
#' @keywords internal
api_calls <- function(endpoint, jahr = NULL, code = NULL, name = NULL) {
  if (length(jahr) > 1) {
    # Case that jahr is a vector
    lapply(jahr, \(y) {
      api_calls(endpoint = endpoint, jahr = y, code = code, name = name)
    })
  } else if (length(code) > 1) {
    # Case of code vector
    lapply(code, \(c) api_call(endpoint = endpoint, jahr = jahr, code = c))
  } else if (length(name) > 1) {
    # Case of name vector
    lapply(name, \(n) api_call(endpoint = endpoint, jahr = jahr, name = n))
  } else {
    # Case of one endpoint
    api_call(endpoint = endpoint, jahr = jahr, code = code, name = name)
  }
}
