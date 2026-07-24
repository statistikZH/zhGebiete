#' Alle Gebiete abrufen
#'
#' @description Ruft alle Gebiete (Kanton, Bezirke, Gemeinden und
#' Raumplanungsregionen) des Kantons Zürich (Stand heute) ab.
#'
#' @return Dataframe mit Gebietstyp-Code und Name sowie Code und Name der Gebiete.
#'
#' @examples \dontrun{
#' # Alle Gebiete abrufen
#' get_gebiete()
#' }
#'
#' @export
get_gebiete <- function() {
  # API call
  gebiete_list <- api_call(endpoint = "gebiete")
  # Parse into data frame
  return(parse_to_df(list = gebiete_list))
}

#' Alle Gebietstypen abrufen
#'
#' @description Ruft alle Gebietstypen auf.
#'
#' @return Dataframe mit Gebietstyp-Code und Name.
#'
#' @examples \dontrun{
#' # Alle Gebietstypen abrufen
#' get_gebietstypen()
#' }
#'
#' @export
get_gebietstypen <- function() {
  # API call
  gebietstypen_list <- api_call(endpoint = "gebietstypen")
  # Parse into data frame
  return(parse_to_df(list = gebietstypen_list))
}

#' Gemeinden abrufen
#'
#' @description Ermöglicht das Laden aller Gemeinden mit oder ohne Filter.
#'
#' @param auswahl Boolean, welcher beschreibt, ob eine Auswahl über die gefundenen
#' Namen gemacht werden soll oder nicht. Verwendungszweck: beim Harmonisieren
#' von Namen.
#' @param ... Unten gelistete Filterargumente:
#' @param gemeinde_code Filter nach Gemeindecode. Muss eine ganze positive Zahl
#' sein. Ein Vektor aus verschiedenen Gemeindecodes ist ebenfalls zulässig.
#' @param gemeinde_name Filter nach Gemeinde Name. Muss vom Typ `character` sein.
#' Ein Vektor aus verschiedenen Gemeinde Namen ist ebenfalls zulässig.
#'
#' @details Es kann nur \code{gemeinde_code} oder \code{gemeinde_name} angegeben
#' werden, aber nicht beide Argumente gemeinsam.\cr\cr
#' Sämtliche angegebenen Filteroptionen müssen zu einem Treffer führen.
#'
#' @returns Dataframe mit Gebietstyp-Code, Offiziellem Code der Gemeinde
#' (BFS-Nummer) und Offiziellem Namen der Gemeinde (BFS-Name).
#' @examples \dontrun{
#' # Alle Gemeinden abrufen
#' get_gemeinden()
#'
#' # Filtern nach einem oder mehreren Codes
#' get_gemeinden(gemeinde_code = 111)
#' get_gemeinden(gemeinde_code = c(111, 112))
#' get_gemeinden(gemeinde_code = c(111:120))
#'
#' # Filtern nach einem oder mehreren Namen
#' get_gemeinden(auswahl = TRUE, gemeinde_name = "am Albis")
#' get_gemeinden(gemeinde_name = c("am Albis", "Züri"))
#' }
#'
#' @export
get_gemeinden <- function(
  auswahl = FALSE,
  ...,
  gemeinde_code = NULL,
  gemeinde_name = NULL
) {
  # Error handling
  rlang::check_dots_empty()
  check_input_param(code = gemeinde_code, name = gemeinde_name)
  # API call
  gemeinde_list <- api_calls(
    endpoint = "gemeinden",
    code = gemeinde_code,
    name = gemeinde_name
  )

  # Select one name
  if (!is.null(gemeinde_name)) {
    gemeinde_list <- select_name(list = gemeinde_list, selection = auswahl)
  }
  # Parse into data frame
  return(parse_to_df(list = gemeinde_list))
}

#' Gemeindezuweisungen abrufen
#'
#' @description Ruft die aktuelle Zuweisung aller Gemeinden zu einem Bezirk und
#' einer Raumplanungsregion ab. Diese Abfrage ist mit oder ohne Filter verfügbar.
#'
#' @param ... Unten gelistete Filterargumente:
#' @param gemeinde_code Filter nach Gemeindecode. Muss eine ganze positive Zahl
#' sein. Ein Vektor aus verschiedenen Gemeindecodes ist ebenfalls zulässig.
#'
#' @return Dataframe mit offiziellem Code der Gemeinde (BFS-Nummer),
#' offiziellem Namen der Gemeinde (BFS-Name), offiziellem Code des Bezirks
#' (BFS-Nummer), offiziellem Name des Bezirks (BFS-Name), offiziellem Code der
#' Raumplanungsregion (Vergabe durch ARE/Kantone) und offiziellem Name der
#' Raumplanungsregion (Vergabe durch ARE/Kantone).
#'
#' @examples \dontrun{
#' # Alle Gemeindezuweisungen abrufen
#' get_gemeindezuweisungen()
#'
#' # Filtern nach einem oder mehreren Gemeindecodes
#' get_gemeindezuweisungen(gemeinde_code = 111)
#' get_gemeindezuweisungen(gemeinde_code = c(111, 113))
#' }
#'
#' @export
get_gemeindezuweisungen <- function(..., gemeinde_code = NULL) {
  # Error handling
  rlang::check_dots_empty()
  check_input_param(code = gemeinde_code)
  # API call
  gemeindezuweisungen_list <- api_calls(
    endpoint = "gemeindezuweisungen",
    code = gemeinde_code
  )

  # Parse into data frame
  return(parse_to_df(list = gemeindezuweisungen_list))
}

#' Bezirke abrufen
#'
#' @description Ermöglicht das Laden aller Bezirke mit oder ohne Filter.
#'
#' @param auswahl Boolean, welcher beschreibt, ob eine Auswahl über die gefundenen
#' Namen gemacht werden soll oder nicht. Verwendungszweck: beim Harmonisieren
#' von Namen.
#' @param ... Unten gelistete Filterargumente:
#' @param bezirk_code Filter nach Bezirkcode. Muss eine ganze positive Zahl sein.
#' Ein Vektor aus verschiedenen Bezirkcodes ist ebenfalls zulässig.
#' @param bezirk_name Filter nach Bezirk Name. Muss vom Typ `character` sein.
#' Ein Vektor aus verschiedenen Bezirk Namen ist ebenfalls zulässig.
#'
#' @details Es kann nur \code{bezirk_code} oder \code{bezirk_name} angegeben
#' werden, aber nicht beide Argumente gemeinsam.\cr\cr
#' Sämtliche angegebenen Filteroptionen müssen zu einem Treffer führen.
#'
#' @examples \dontrun{
#' # Alle Bezirke abrufen
#' get_bezirke()
#'
#' # Filtern nach einem oder mehreren Codes
#' get_bezirke(bezirk_code = 111)
#' get_bezirke(bezirk_code = c(111, 112))
#' get_bezirke(bezirk_code = c(110:112))
#'
#' # Filtern nach einem oder mehreren Namen
#' get_bezirke(bezirk_name = "Andeldingen")
#' get_bezirke(bezirk_name = c("Andeldingen", "Züri"))
#' }
#' @returns Dataframe mit Gebietstyp-Code, Offizieller Code des Bezirks
#' (BFS-Nummer) und Offizieller Name des Bezirks (BFS-Name).
#'
#' @export
get_bezirke <- function(
  auswahl = FALSE,
  ...,
  bezirk_code = NULL,
  bezirk_name = NULL
) {
  # Error handling
  rlang::check_dots_empty()
  check_input_param(code = bezirk_code, name = bezirk_name)
  # API call
  bezirk_list <- api_calls(
    endpoint = "bezirke",
    code = bezirk_code,
    name = bezirk_name
  )

  # Select one name
  if (!is.null(bezirk_name)) {
    bezirk_list <- select_name(list = bezirk_list, selection = auswahl)
  }

  # Remove "gemeinden" for filters
  short_bezirk_list <- remove_gemeinden(list = bezirk_list)
  # Parse into data frame
  bezirk_df <- parse_to_df(list = short_bezirk_list)
  # Remove  "gemeinden" with no filter
  bezirk_df$gemeinden <- NULL

  return(bezirk_df)
}

#' Raumplanungsregionen abrufen
#'
#' @description Ermöglicht das Laden aller Raumplanungsregionen mit oder ohne
#' Filter.
#'
#' @param auswahl Boolean, welcher beschreibt, ob eine Auswahl über die gefundenen
#' Namen gemacht werden soll oder nicht. Verwendungszweck: beim Harmonisieren
#' von Namen.
#' @param ... Unten gelistete Filterargumente:
#' @param raumplanungsregion_code Filter nach Raumplanungsregioncode. Muss eine
#' ganze positive Zahl sein. Ein Vektor aus verschiedenen
#' Raumplanungsregioncodes ist ebenfalls zulässig.
#' @param raumplanungsregion_name Filter nach Rraumplanungsregion Name. Muss
#' vom Typ `character` sein. Ein Vektor aus verschiedenen Raumplanungsregion
#' Namen ist ebenfalls zulässig.
#'
#' @details Es kann nur \code{raumplanungsregion_code} oder
#' \code{raumplanungsregion_name} angegeben werden, aber nicht beide Argumente
#' gemeinsam.\cr\cr
#' Sämtliche angegebenen Filteroptionen müssen zu einem Treffer führen.
#'
#' @returns Dataframe mit Gebietstyp-Code, Offizieller Code der
#' Raumplanungsregion (Vergabe durch ARE/Kantone) und Offizieller Name der
#' Raumplanungsregion (Vergabe durch ARE/Kantone).
#'
#' @examples \dontrun{
#' # Raumplanungsregionen abrufen
#' get_raumplanungsregionen()
#'
#' # Filtern nach einem oder mehreren Codes
#' get_raumplanungsregionen(raumplanungsregion_code = 101)
#' get_raumplanungsregionen(raumplanungsregion_code = c(101, 103))
#' get_raumplanungsregionen(raumplanungsregion_code = c(101:103))
#'
#' # Filtern nach einem oder mehreren Namen
#' get_raumplanungsregionen(auswahl = TRUE, raumplanungsregion_name = "tal")
#' get_raumplanungsregionen(raumplanungsregion_name = c("tal", "Züri"))
#' }
#'
#' @export
get_raumplanungsregionen <- function(
  auswahl = FALSE,
  ...,
  raumplanungsregion_code = NULL,
  raumplanungsregion_name = NULL
) {
  # Error handling
  rlang::check_dots_empty()
  check_input_param(
    code = raumplanungsregion_code,
    name = raumplanungsregion_name
  )
  # API call
  raumplanungsregionen_list <- api_calls(
    endpoint = "raumplanungsregionen",
    code = raumplanungsregion_code,
    name = raumplanungsregion_name
  )

  # Select one name
  if (!is.null(raumplanungsregion_name)) {
    raumplanungsregionen_list <- select_name(
      list = raumplanungsregionen_list,
      selection = auswahl
    )
  }

  # Remove "gemeinden" for filters
  short_raumplanungsregionen_list <- remove_gemeinden(
    list = raumplanungsregionen_list
  )
  # Parse into data frame
  raumplanungsregionen_df <- parse_to_df(list = short_raumplanungsregionen_list)
  # Remove  "gemeinden" with no filter
  raumplanungsregionen_df$gemeinden <- NULL

  return(raumplanungsregionen_df)
}

#' Alle Gemeindemutationen abrufen
#'
#' @description Ruft alle Gemeindemutationen seit 1990 ab.
#'
#' @return Ein Dataframe mit dem Mutationstyp, dem alten Gemeindecode und Namen
#'  sowie dem neuen Gemeindecode und Namen, so wie das Mutationsdatum.
#'
#' @examples \dontrun{
#' # Alle Gemeindemutationen abrufen
#' get_gemeindemutationen()
#' }
#'
#' @export
get_gemeindemutationen <- function() {
  # API call
  gemeindemutationen_list <- api_call(endpoint = "gemeindemutationen")
  # Parse into data frame
  return(parse_to_df(list = gemeindemutationen_list))
}

#' Jahresstände von Gemeinden abrufen
#'
#' @description Jahresstände aller Gemeinden abrufen, ab 1990 mit oder ohne
#' Filter.
#'
#' @param ... Unten gelistete Filterargumente:
#' @param jahr Filter nach spezifischem Jahr. Muss eine ganze Zahl sein, welche
#' zwischen 1990 und dem heutigen Jahr liegt.
#' @param gemeinde_code Filter nach Gemeindecode. Muss eine ganze positive Zahl
#' sein. Ein Vektor aus verschiedenen Gemeindecodes ist ebenfalls zulässig.
#'
#' @details Alle Kombinationen von \code{jahr} und \code{gemeinde_code} sind
#'  möglich.
#'
#' @return Dataframe mit Gebietstyp-Code, offiziellem Code der Gemeinde
#' (BFS-Nummer), offiziellem Namen der Gemeinde (BFS-Name) und Jahr des
#' Jahresbestands.
#'
#' @examples \dontrun{
#' # Alle Jahresstände von Gemeinden abrufen
#' get_gemeindenhist()
#'
#' # Filtern nach einem oder mehreren Jahren
#' get_gemeindenhist(jahr = 2026)
#' get_gemeindenhist(jahr = c(2021:2026))
#'
#' # Filtern nach einem oder mehreren Gemeindecodes
#' get_gemeindenhist(gemeinde_code = 111)
#' get_gemeindenhist(gemeinde_code = c(111, 113))
#'
#' # Filtern nach Jahr und Gemeindecode
#' get_gemeindenhist(jahr = 2021, gemeinde_code = 111)
#' get_gemeindenhist(jahr = c(2021:2026), gemeinde_code = c(111, 113))
#' }
#'
#' @export

get_gemeindenhist <- function(
  ...,
  jahr = NULL,
  gemeinde_code = NULL
) {
  # Error handling
  rlang::check_dots_empty()
  check_input_param(jahr = jahr, code = gemeinde_code)
  # API call
  gemeindehist_list <- api_calls(
    endpoint = "gemeindenhist",
    jahr = jahr,
    code = gemeinde_code
  )

  # Parse into data frame
  gemeindehist_df <- parse_to_df(list = gemeindehist_list)

  # Remove unwanted parameters
  if (names(gemeindehist_df[1]) == "gemeinde_code") {
    gemeindehist_df <- gemeindehist_df[-1]
  }
  if (names(gemeindehist_df[1]) == "jahr") {
    gemeindehist_df <- gemeindehist_df[-1]
  }
  # Standardize the names
  names(gemeindehist_df) <- gsub("\\.1", "", names(gemeindehist_df))

  return(gemeindehist_df)
}

#' Health-Check der API
#'
#' @description Fragt die Health-Parameter der API ab.
#'
#' @return Liste mit den folgenden Komponenten:
#' \describe{
#'   \item{status}{Aktueller Status der API.}
#'   \item{timestamp}{Zeitpunkt der Abfrage.}
#'   \item{data_available}{Boolean, der angibt, ob Daten verfügbar sind.}
#' }
#'
#' @examples \dontrun{
#' # Health-Check ausfuehren
#' get_health()
#' }
#'
#' @export
get_health <- function() {
  # API call
  health_list <- api_call(endpoint = "health")
  # No parsing needed
  return(health_list)
}
