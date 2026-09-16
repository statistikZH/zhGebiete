#' Namen bei Mehrfachtreffern auswaehlen
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#' @param selection Boolean, ob eine Auswahl erfolgen soll.
#' @param search_term Optional: Der verwendete Suchbegriff für die Ausgabe.
#'
#' @returns Trefferliste mit dem ausgewählten Namen im Original-Wrapper.
#'
#' @keywords internal
select_name <- function(list, selection, search_term = NULL) {
  if (is.null(names(list))) {
    # --- Paarweise Verarbeitung von Ergebnissen und Suchbegriffen ---
    if (!is.null(search_term) && length(search_term) == length(list)) {
      # Nutzen von Map, um jedes Ergebnis mit dem entsprechenden Suchbegriff zu koppeln
      return(Map(
        function(l, s) select_name(list = l, selection = selection, search_term = s),
        list,
        search_term
      ))
    } else {
      # Fallback für einzelne Suchbegriffe oder wenn kein Suchbegriff vorhanden ist
      return(lapply(list, \(l) select_name(list = l, selection = selection, search_term = search_term)))
    }
  } else {
    # Daten-Key identifizieren (z.B. 'gemeinden')
    data_key <- names(list)[sapply(list, function(x) is.list(x) || is.data.frame(x))][1]

    if (is.null(data_key)) return(list)

    data_array <- list[[data_key]]

    # Sortierung nach Code
    if (is.data.frame(data_array)) {
      code_col <- grep("code", names(data_array), value = TRUE)[1]
      if (!is.na(code_col)) {
        data_array <- data_array[order(data_array[[code_col]]), ]
        rownames(data_array) <- NULL
      }
    }

    num_results <- if (is.data.frame(data_array)) nrow(data_array) else length(data_array)

    if (num_results > 1 && selection) {
      msg <- if (!is.null(search_term)) {
        sprintf("Die Folgenden Treffer wurden erzielt bei der Suche nach \"%s\":\n", search_term)
      } else {
        "Mehrere Treffer gefunden. Bitte einen auswählen:\n"
      }
      cat(msg)

      # Vorschau:
      if (is.data.frame(data_array)) {
        cols <- grep("name|code", names(data_array), value = TRUE)
        preview <- data_array[, cols, drop = FALSE]
      } else {
        preview <- do.call(rbind, lapply(data_array, function(x) {
          res_cols <- grep("name|code", names(x), value = TRUE)
          as.data.frame(x[res_cols])
        }))
      }

      print(preview)
      cat("------------------------------------------------------------\n")

      selected_value <- suppressWarnings(as.integer(readline(prompt = "Bitte einen Treffer waehlen: ")))

      while (is.na(selected_value) || (selected_value < 1) || (selected_value > num_results)) {
        selected_value <- suppressWarnings(as.integer(readline(prompt = "Ungültige Eingabe. Bitte Zahl wählen: ")))
      }

      if (is.data.frame(data_array)) {
        selected_item <- data_array[selected_value, , drop = FALSE]
      } else {
        selected_item <- data_array[[selected_value]]
      }

      return(setNames(list(list(selected_item)), data_key))
    } else {
      return(list)
    }
  }
}




