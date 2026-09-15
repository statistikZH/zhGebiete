#' Namen bei Mehrfachtreffern auswaehlen
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#' @param selection Boolean, ob eine Auswahl erfolgen soll.
#'
#' @returns Trefferliste mit dem ausgewählten Namen im Original-Wrapper.
#'
#' @keywords internal
select_name <- function(list, selection) {
  if (is.null(names(list))) {
    return(lapply(list, \(l) select_name(list = l, selection)))
  } else {
    # Daten-Key identifizieren (z.B. 'gemeinden')
    data_key <- names(list)[sapply(list, function(x) is.list(x) || is.data.frame(x))][1]

    if (is.null(data_key)) return(list)

    data_array <- list[[data_key]]
    num_results <- if (is.data.frame(data_array)) nrow(data_array) else length(data_array)

    if (num_results > 1 && selection) {
      cat("Mehrere Treffer gefunden. Bitte einen auswählen:\n")

      # Vorschau: Nur Name und Code
      if (is.data.frame(data_array)) {
        cols <- grep("name|code", names(data_array), value = TRUE)
        preview <- data_array[, cols, drop = FALSE]
      } else {
        preview <- do.call(rbind, lapply(data_array, function(x) {
          as.data.frame(x[grep("name|code", names(x))])
        }))
      }

      print(preview)
      cat("------------------------------------------------------------\n")

      selected_value <- suppressWarnings(as.integer(readline(prompt = "Index wählen: ")))

      while (is.na(selected_value) || (selected_value < 1) || (selected_value > num_results)) {
        selected_value <- suppressWarnings(as.integer(readline(prompt = "Ungültige Eingabe. Bitte Zahl wählen: ")))
      }

      # Rückgabe: Das ausgewählte Element wieder in den Wrapper packen
      if (is.data.frame(data_array)) {
        selected_item <- data_array[selected_value, , drop = FALSE]
      } else {
        selected_item <- data_array[[selected_value]]
      }

      # Wir geben eine Liste zurück, die wieder ein Array (Liste mit einem Element) enthält
      # damit parse_to_df weiterhin konsistent funktioniert.
      return(setNames(list(list(selected_item)), data_key))
    } else {
      return(list)
    }
  }
}


