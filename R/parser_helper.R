#' Data frame Parser
#'
#' @description Konvertiert eine Liste aus einer API-Abfrage in einen Datenframe.
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#'
#' @returns Ein Datenframe mit den parseden Ergebnissen.
#'
#' @keywords internal
parse_to_df <- function(list) {

  # 1. Rekursion für verschachtelte Listen (z.B. von api_calls)
  if (is.null(names(list))) {
    tryCatch(
      {
        do.call(rbind, lapply(list, \(l) parse_to_df(list = l)))
      },
      error = function(e) {
        stop("Die Filteroption liefert keinen Treffer.", call. = FALSE)
      }
    )
  } else {
    # 2. Extraktion des Daten-Arrays
    # Wir nehmen das erste Element, das eine Liste oder ein Dataframe ist (z.B. 'gemeinden')
    data_key <- names(list)[sapply(list, function(x) is.list(x) || is.data.frame(x))][1]

    if (is.null(data_key)) {
      # Fallback, falls die Struktur unerwartet ist
      df <- as.data.frame(list, stringsAsFactors = FALSE)
    } else {
      data_element <- list[[data_key]]

      # Da die API immer eine Liste/Array liefert, konvertieren wir sie direkt
      if (is.data.frame(data_element)) {
        df <- data_element
      } else {
        df <- do.call(rbind, lapply(data_element, function(x) {
          as.data.frame(x, stringsAsFactors = FALSE)
        }))
      }
    }

    # 3. Spaltennamen korrigieren (Präfixe entfernen)
    if (!is.null(names(df))) {
      names(df) <- gsub(".*\\.", "", names(df))
    }

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
#' @returns Die Eingabeliste ohne das Element/die Spalte "gemeinden".
#'
#' @keywords internal
remove_gemeinden <- function(list) {
  # Basisfall: kein Listen-Objekt (z.B. ein einzelner skalarer Wert) -> nichts zu tun
  if (!is.list(list)) {
    return(list)
  }

  # Fall: data.frame (z.B. das "bezirke"-Element nach der jsonlite-Simplifizierung,
  # wo "gemeinden" als Listen-Spalte auftaucht statt als normales Listenelement)
  if (is.data.frame(list)) {
    if ("gemeinden" %in% names(list)) {
      list$gemeinden <- NULL
    }
    # Übrige Spalten rekursiv weiter durchsuchen (falls noch tiefer verschachtelt)
    list[] <- lapply(list, remove_gemeinden)
    return(list)
  }

  # Fall: unbenannte (verschachtelte) Liste, z.B. Ergebnis mehrerer api_calls()
  # (jahr-, code- oder name-Vektor) -> rekursiv über die einzelnen Elemente
  if (is.null(names(list))) {
    return(lapply(list, remove_gemeinden))
  }

  # Fall: benannte Liste (Wrapper-Objekt einer Region/eines Bezirks)
  if ("gemeinden" %in% names(list)) {
    list$gemeinden <- NULL
  }

  # Restliche Elemente rekursiv weiter durchsuchen (z.B. "bezirke", das selbst
  # wieder ein data.frame mit "gemeinden"-Spalte sein kann)
  list <- lapply(list, remove_gemeinden)

  return(list)
}
