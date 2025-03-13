#' Print and Return Unique Sorters in Concept Map Data
#'
#' This function retrieves and optionally prints the unique sorters (users) in a given concept mapping dataset.
#'
#' @param CMData A data frame containing concept map data. This must include a column named \code{"sorterID"}.
#' @param verbose A logical, if \code{TRUE}, the function will print the list of unique sorters to the console.
#'
#' @return A vector of unique sorter IDs.
#'
#' @details
#' The function first checks if the provided dataset is suitable for concept mapping using the \cr
#' \code{checkConceptMapData} function. If the data is valid, it retrieves the unique sorter IDs
#' from the \code{sorterID} column. If \code{verbose = TRUE}, the function prints the sorter IDs.
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
#' # Retrieve unique sorters without printing
#' printSorters(CMData, verbose = FALSE)
#'
#' # Retrieve and print unique sorters to console
#' printSorters(CMData)
#'
#' @export
printSorters <- function(CMData, verbose = TRUE) {

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  allSorters <- unique(sort(CMData$sorterID))

  if (verbose) {
    cat(sprintf("Sorters in this cluster:\n"))
    for (i in allSorters) {
      cat(sprintf("  %s\n", i))
    }
  }

  return(allSorters)
}
