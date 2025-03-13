#' Print a Concept Map Object
#'
#' This function prints a detailed summary of a concept map object, including information about
#' statements, sorters, clusters, and the specific statements within selected clusters.
#'
#' @param x An object of class \code{"conceptMap"} containing the results of concept mapping analysis
#' creating using the function \code{\link[cmAnalysis]{conceptMapping}}.
#'
#' @param whichCluster A vector of cluster numbers to display. If \code{NULL} (default), all clusters are displayed.
#'
#' @param ... arguments to be passed to methods
#'
#' @return This function does not return a value; it prints the details of the concept map to the console.
#'
#' @details
#' The function first checks if the input object is of class \code{"conceptMap"} and validates the
#' requested clusters (if specified). It provides an overview of the number of statements, sorters,
#' and clusters. For each requested cluster, the function lists the statements included in that cluster.
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
#' # Print content of concept map of sorter cluster 3
#' print(myCMAnalysis3)
#'
#' @export
print.conceptMap <- function(x, whichCluster = NULL, ...) {

  if (!inherits(x, "conceptMap")) {
    stop("Object x should be of class conceptMap")
  }

  cat(sprintf("Summary of the cluster:\n"))
  cat(sprintf("Number of statements: %d\n", nrow(x$allStatements)))
  cat(sprintf("Number of sorters: %d\n", length(unique(x$CMData$sorterID))))
  for (jj in 1:length(unique(x$CMData$sorterID))) {
    cat(sprintf("  %2d - %s\n", jj, unique(x$CMData$sorterID)[jj] ))
  }
  cat(sprintf("Number of clusters in this sorter cluster: %d\n", x$numberOfClusters))

  #check which cluster we want
  if (is.null(whichCluster)) {
    whichCluster <- 1:x$numberOfClusters
  } else {
    if (max(whichCluster) > x$numberOfClusters) {
      stop(sprintf("Requested cluster %d does not exist in the sorter cluster.", max(whichCluster)))
    }
  }

  for (k in whichCluster) {
    idx <- which(x$clusterResults == k)
    cat(sprintf("  Cluster %d: %d statements\n", k, length(idx)))
    for (jj in idx) {
      cat(sprintf("    %2d - %s\n", jj, x$allStatements[jj, "Statement"]))
    }
  }
}
