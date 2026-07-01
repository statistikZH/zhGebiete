#' Data frame Parser
#'
#' @description Konvertiert eine Liste in einen Datenframe
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#'
#' @returns Daten frame.
#'
#' @keywords internal
parse_to_df <- function(list) {
  # Error handling
  if (!is.null(list$error)) {
    stop("Kein Treffer gefunden.", call. = FALSE)
  }
  # Check if the list is nested
  if (is.null(names(list))) {
    # Recursive call
    tryCatch(
      {
        df <- do.call(rbind, lapply(list, \(l) parse_to_df(list = l)))
      },
      error = function(e) {
        stop("Eine Filteroption liefert keinen treffer.", call. = FALSE)
      }
    )
  } else {
    # Generate one data frame row
    df <- data.frame(list[1:length(list)])
    # Correct names
    names(df) <- gsub(".*\\.", "", names(df))
  }
  return(df)
}

#' Entferne "Gemeinden"-Liste
#'
#' @description Bei den Bezirken und Raumplanungsregionen wird im API-Abruf eine
#' Liste der zugehörigen Gemeinden zurückgegeben. Diese Liste wird mit dieser
#' Funktion entfernt.
#'
#' @param list Eine aus einer API-Abfrage stammende Liste.
#'
#' @returns Die Eingabeliste ohne das Element "Gemeinden".
#'
#' @keywords internal
remove_gemeinden <- function(list) {
  # Check if the list is nested
  if (is.null(names(list))) {
    # Recursive call
    list <- lapply(list, \(l) remove_gemeinden(list = l))
  } else {
    # Remove the part with the "gemeinden"
    list$gemeinden <- NULL
  }
  return(list)
}
