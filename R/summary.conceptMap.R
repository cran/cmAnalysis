#' Summary of a Concept Map Object
#'
#' This function provides a summary of a concept map object, including the number of statements,
#' sorters, clusters, and the distribution of statements across clusters.
#'
#' @param object An object of class \code{"conceptMap"} containing the results of concept mapping analysis
#' creating using the function \code{"conceptMapping"}.
#' @param ... arguments to be passed to methods
#'
#' @return This function does not return a value; it prints a summary of the concept map object to the console.
#'
#' @details
#' The function verifies that the input object is of class \code{"conceptMap"} and extracts key information from it.
#' It summarizes the number of statements, sorters, and clusters, and details the distribution of statements across
#' the identified clusters.
#'
#' @examples
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.90, attributeWeights=c(1,1,1,1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Subject sorter cluster 3 to concept mapping using default "network" method
#' myCMAnalysis3 <- conceptMapping(myCMDataBySorters[[3]])
#'
#' # Generate summary of concept map of sorter cluster 3
#' summary(myCMAnalysis3)
#'
#' @export
summary.conceptMap <- function(object, ...) {

  if (!inherits(object, "conceptMap")) {
    stop("'object' should be of class conceptMap")
  }

  cat(sprintf("Summary of the cluster:\n"))
  cat(sprintf("  Number of statements: %d\n", nrow(object$allStatements)))
  cat(sprintf("  Number of sorters: %d\n\n", length(unique(object$CMData$sorterID))))
  cat(sprintf("  Number of clusters in this sorter cluster: %d\n", object$numberOfClusters))

  for (k in sort(unique(object$clusterResults))) {
    idx <- which(object$clusterResults == k)
    cat(sprintf("  Cluster %d: %d statements\n", k, length(idx)))
  }
}
