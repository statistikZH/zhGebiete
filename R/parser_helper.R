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

  # 1. Rekursion für verschachtelte Listen (z.B. von api_calls mit Vektor-Input)
  if (is.null(names(list))) {
    # Verarbeite alle Elemente der Liste einzeln
    processed_list <- lapply(list, \(l) parse_to_df(list = l))

    # Entferne NULL-Einträge
    processed_list <- processed_list[!sapply(processed_list, is.null)]

    if (length(processed_list) == 0) {
      return(NULL)
    }

    # Alle vorkommenden Spaltennamen über alle Dataframes hinweg sammeln
    all_cols <- unique(unlist(lapply(processed_list, names)))

    # Jeden Dataframe standardisieren: Fehlende Spalten mit NA ergänzen,
    # vorhandene Spalten in die richtige Reihenfolge bringen
    standardized_list <- lapply(processed_list, function(df) {
      if (!is.data.frame(df)) return(NULL)

      missing_cols <- setdiff(all_cols, names(df))
      if (length(missing_cols) > 0) {
        # Fehlende Spalten als NA hinzufügen
        df[missing_cols] <- NA
      }
      # Spalten in die globale Reihenfolge bringen
      return(df[, all_cols, drop = FALSE])
    })

    # Nochmal NULLs entfernen, falls jemand kein DF zurückgab
    standardized_list <- standardized_list[!sapply(standardized_list, is.null)]

    return(do.call(rbind, standardized_list))
    # ------------------------------

  } else {
    # 2. Extraktion des Daten-Arrays
    data_key <- names(list)[sapply(list, function(x) is.list(x) || is.data.frame(x))][1]

    if (is.null(data_key)) {
      df <- as.data.frame(list, stringsAsFactors = FALSE)
    } else {
      data_element <- list[[data_key]]

      if (is.data.frame(data_element)) {
        df <- data_element
      } else if (length(data_element) == 0) {
        return(NULL)
      } else {

        # Innerhalb eines einzelnen Calls könnten Elemente unterschiedlich sein
        dfs <- lapply(data_element, function(x) as.data.frame(x, stringsAsFactors = FALSE))
        all_cols <- unique(unlist(lapply(dfs, names)))

        standardized_dfs <- lapply(dfs, function(df) {
          missing_cols <- setdiff(all_cols, names(df))
          if (length(missing_cols) > 0) df[missing_cols] <- NA
          df[, all_cols, drop = FALSE]
        })
        df <- do.call(rbind, standardized_dfs)
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
