#' Create Silhouette Plot for K-means Clustering
#'
#' This helper function generates a silhouette plot for K-means clustering
#' based on the provided co-occurrence matrix and range of cluster numbers.
#'
#' @param coOccurrenceMatrix A matrix of co-occurrence data to be clustered.
#' @param rangeNumberOfClusters A vector specifying the range of cluster numbers to evaluate (default is `2:15`).
#' @param numberOfKmeansRestarts Number of k-means restarts (default is 100).
#' @param main A string for the title of the silhouette plot.
#' @param forcedClusterNumber Optional fixed number of clusters to use (default is `NULL`).
#'
#' @return A list containing:
#'   - `silhouettePlot`: A ggplot object for the silhouette plot.
#'   - `silhouetteNumberOfClusters`: The optimal number of clusters determined by silhouette analysis.
#'   - `silhouetteClusterResults`: The cluster assignments for each data point.
#'
#' @importFrom cluster silhouette
#' @importFrom stats dist
#' @importFrom ggplot2 ggplot aes geom_line geom_point annotate labs theme_minimal
#' @noRd
createSilhouettePlot <- function(coOccurrenceMatrix,
                                 rangeNumberOfClusters = 2:15,
                                 numberOfKmeansRestarts = 100,
                                 main,
                                 forcedClusterNumber=NULL) {

  #Calculate the distance matrix for cluster k
  myDistance <- dist(coOccurrenceMatrix, method='euclidean')

  main <- paste("Kmeans", main)

  mySilhouetteResults <- doSilhouette(myDistance, rangeNumberOfClusters, numberOfKmeansRestarts, main, forcedClusterNumber)

  return(list(silhouettePlot=mySilhouetteResults$silhouettePlot,
              silhouetteNumberOfClusters=mySilhouetteResults$numberOfClusters,
              silhouetteClusterResults=mySilhouetteResults$clusterResults))
}
