#' Count the Number of Statements in Concept Map Data
#'
#' This function calculates the number of unique statements in a given concept mapping dataset.
#'
#' @param CMData A data frame containing concept map data. This must include a column named \code{"statement"}.
#' @param verbose A logical, if \code{TRUE}, the function will print the number of statements the console.
#'
#' @return An integer representing the number of unique statements in the dataset.
#'
#' @details
#' The function first checks if the provided dataset is suitable for concept mapping using the \cr
#' \code{checkConceptMapData} function. If the data is valid, it calculates and returns the number
#' of unique \code{statement}s.
#'
#' @examples
#' # Example of valid data
#' CMData <- data.frame(
#'   sorterID = c("resp1", "resp1", "resp1", "resp2",
#'    "resp2", "resp2", "resp3", "resp3", "resp3"),
#'   statement = c("London", "Frankfurt", "Berlin", "London",
#'    "Frankfurt", "Berlin", "London", "Frankfurt", "Berlin"),
#'   stackID = c("capital city", "city", "capital city", 1, 2, 2, "A", "B", "A")
#' )
#'
#' # Count the number of statements silently
#' numberOfStatements(CMData, verbose = FALSE)
#'
#' # Count the number of statements with message
#' numberOfStatements(CMData)
#'
#' @export
numberOfStatements <- function(CMData, verbose = TRUE) {

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  nStatements <- length(unique(sort(CMData$statement)))

  if (verbose) {
    cat(sprintf("Number of statements in this cluster: %d\n", nStatements))
  }

  return(nStatements)
}
