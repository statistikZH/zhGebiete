#' Namen bei Mehrfachtreffern auswaehlen
#'
#' @details Diese Funktion erlaubt es zu identifizieren, ob eine Suche nach
#' Namen mehrere Treffer geliefert hat. Falls ja, muessen Nutzende via Konsole
#' ein Namen auswäaelen.
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#' @param selection Boolean, welcher beschreibt, ob eine Auswahl über die gefundenen
#' Namen gemacht werden soll oder nicht. Verwendungszweck: beim Harmonisieren
#' von Namen.
#'
#' @returns Trefferliste mit dem ausgewählten Namen und ohne den Suchnamen.
#'
#' @keywords internal

select_name <- function(list, selection) {
  if (is.null(names(list))) {
    list_selected <- lapply(list, \(l) select_name(list = l, selection))
  } else {
    # Identifiziere das Daten-Array (z.B. 'gemeinden', 'bezirke' etc.)
    data_key <- names(list)[sapply(list, is.list)][1]
    data_array <- list[[data_key]]

    if (is.null(data_array) || length(data_array) == 0) {
      stop("Eine Filteroption liefert keinen Treffer.", call. = FALSE)
    }

    if (length(data_array) > 1 && selection) {
      cat(paste0("Mehrere Treffer gefunden. Bitte einen auswählen:\n"))

      # Vorschau der Treffer erstellen
      preview <- do.call(rbind, lapply(data_array, function(x) {
        # Nur Name und Code anzeigen
        as.data.frame(x[grep("name|code", names(x))])
      }))
      print(preview)
      cat("------------------------------------------------------------\n")

      selected_value <- suppressWarnings(as.integer(readline(prompt = "Index wählen: ")))

      while (is.na(selected_value) || (selected_value < 1) || (selected_value > length(data_array))) {
        selected_value <- suppressWarnings(as.integer(readline(prompt = "Ungültige Eingabe. Bitte Zahl wählen: ")))
      }

      # Nur das ausgewählte Element als Liste zurückgeben, damit parse_to_df funktioniert
      return(setNames(list(data_array[[selected_value]]), data_key))
    } else {
      # Nur ein Treffer oder keine Auswahl gewünscht -> das ganze Array zurückgeben
      return(list)
    }
  }
  return(list_selected)
}
