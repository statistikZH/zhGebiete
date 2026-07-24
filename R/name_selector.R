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
  # In case the list has names
  if (is.null(names(list))) {
    # Recursive call
    list_selected <- lapply(list, \(l) select_name(list = l, selection))
  } else {
    # Error handling
    if (length(list$treffer) == 0) {
      stop("Eine Filteroption liefert keinen treffer.", call. = FALSE)
    }
    # Find out if there are multiple matches
    if (length(list$treffer[[1]]) > 1 & selection) {
      # Print selection criteria to console
      cat(paste0(
        "Die Folgenden Treffer wurden erzielt bei der suche nach \"",
        list$name,
        "\":\n"
      ))

      # Ensure a nice print out
      list$treffer$gemeinden <- NULL
      names(list$treffer) <- gsub(".*\\.", "", names(list$treffer))

      print(list$treffer)
      cat("------------------------------------------------------------\n")

      # Get user feedback
      selected_value <- suppressWarnings(as.integer(readline(
        prompt = "Bitte einen Treffer waehlen: "
      )))

      # Error handling for invalid entries
      while (
        is.na(selected_value) |
          (selected_value < 1) |
          (selected_value > length(list$treffer[[1]]))
      ) {
        selected_value <- suppressWarnings(as.integer(readline(
          prompt = "Bitte einene Zahl eingeben welche geht: "
        )))
      }

      # Perform the selection
      list_selected <- list$treffer[selected_value, ]
    } else {
      # Case that there is only one or no match
      list_selected <- list$treffer # Assign the "treffer" list
    }
  }
  return(list_selected)
}
