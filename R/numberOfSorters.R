#' Count the Number of Sorters in Concept Map Data
#'
#' This function calculates the number of unique sorters (users) in a given concept mapping dataset.
#'
#' @param CMData A data frame containing concept map data. This must include a column named \code{"sorterID"}.
#' @param verbose A logical, if \code{TRUE}, the function will print the number of sorters. 
#'
#' @return An integer representing the number of unique sorters in the dataset.
#'
#' @details
#' The function first checks if the provided dataset is suitable for concept mapping using the \cr
#' \code{checkConceptMapData} function. If the data is valid, it calculates and returns the number
#' of unique \code{sorterID}s.
#'
#' @examples
#' # Example of valid data
#' CMData <- data.frame(
#'     sorterID = c("resp1", "resp1", "resp1", "resp2",
#'      "resp2", "resp2", "resp3", "resp3", "resp3"),
#'    statement = c("London", "Frankfurt", "Berlin", "London",
#'     "Frankfurt", "Berlin", "London", "Frankfurt", "Berlin"),
#'     stackID = c("capital city", "city", "capital city", 1, 2, 2, "A", "B", "A")
#' )
#'
#' # Count the number of sorters silently
#' numberOfSorters(CMData, verbose = FALSE)
#'
#' # Count the number of sorters with message
#' numberOfSorters(CMData)
#'
#' @export
numberOfSorters <- function(CMData, verbose = TRUE) {

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  allSorters <- unique(sort(CMData$sorterID))
  nSorters <- length(allSorters)

  if (verbose) {
    cat(sprintf("Number of sorters in this cluster: %d\n", nSorters))
  }

  return(nSorters)
}
