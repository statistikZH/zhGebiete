#' Data frame Parser
#'
#' @description Konvertiert eine Liste aus einer API-Abfrage in einen Datenframe.
#' Die Funktion erkennt automatisch die API-Struktur (Wrapper-Objekt)
#' und extrahiert das entsprechende Daten-Array.
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#'
#' @returns Ein Datenframe mit den parseden Ergebnissen.
#'
#' @keywords internal
parse_to_df <- function(list) {

  # 1. Prüfung, ob die Liste verschachtelt ist (z.B. Ergebnis von api_calls lapply)
  if (is.null(names(list))) {
    # Rekursiver Aufruf
    tryCatch(
      {
        do.call(rbind, lapply(list, \(l) parse_to_df(list = l)))
      },
      error = function(e) {
        stop("Eine Filteroption liefert keinen Treffer.", call. = FALSE)
      }
    )
  } else {
    # 2. Extraktion des Daten-Arrays aus dem Wrapper
    # Die API gibt Daten in einem Array zurück (z.B. { "gemeinden": [...] }).
    # Wir suchen nach dem Element, das eine Liste ist und selbst Namen besitzt (die Datenzeilen).
    data_element <- NULL
    for (name in names(list)) {
      if (is.list(list[[name]]) && !is.null(names(list[[name]]))) {
        data_element <- list[[name]]
        break
      }
    }

    if (is.null(data_element)) {
      # Fallback: Wenn kein Array gefunden wurde, versuche das Objekt direkt zu konvertieren
      df <- as.data.frame(list, stringsAsFactors = FALSE)
    } else {
      # Konvertiere das gefundene Array (Liste von Objekten) in einen Dataframe
      if (length(data_element) == 0) {
        stop("Keine Daten im Response-Array gefunden.", call. = FALSE)
      }

      # Alle Listenelemente zu einem Dataframe zusammenführen
      df <- do.call(rbind, lapply(data_element, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
    }

    # 3. Spaltennamen korrigieren
    # Entfernt Präfixe aus den Namen (z.B. "gemeinde.name" -> "name")
    names(df) <- gsub(".*\\.", "", names(df))

    return(df)
  }
}

#' Entferne "Gemeinden"-Liste
#'
#' @description Bei den Bezirken und Raumplanungsregionen wird im API-Abruf eine
#' Liste der zugehörigen Gemeinden zurückgegeben. Diese Liste wird mit dieser
#' Funktion entfernt, um die Erstellung eines sauberen Datenframes zu ermöglichen.
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#'
#' @returns Die Eingabeliste ohne das Element "gemeinden".
#'
#' @keywords internal
remove_gemeinden <- function(list) {
  # Prüfung, ob die Liste verschachtelt ist
  if (is.null(names(list))) {
    # Rekursiver Aufruf
    list <- lapply(list, \(l) remove_gemeinden(list = l))
  } else {
    # Entferne das Element "gemeinden", falls vorhanden
    if ("gemeinden" %in% names(list)) {
      list$gemeinden <- NULL
    }
  }
  return(list)
}
