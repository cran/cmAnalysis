#' Perform Silhouette Analysis for Optimal Clustering
#'
#' This helper function computes the silhouette widths for a range of cluster numbers
#' and identifies the optimal number of clusters for clustering algorithms.
#'
#' @param myDf A numeric data frame or matrix for clustering.
#' @param rangeNumberOfClusters A vector specifying the number of clusters to evaluate (default is `2:15`).
#' @param numberOfKmeansRestarts Number of k-means restarts (default is 100).
#' @param main A string for the title of the silhouette plot.
#' @param forcedClusterNumber Optional fixed number of clusters to use (default is `NULL`).
#'
#' @return A list containing:
#'   - `silhouettePlot`: A ggplot object for the silhouette plot.
#'   - `numberOfClusters`: The optimal number of clusters.
#'   - `clusterResults`: The cluster assignments for each data point.
#'
#' @importFrom cluster silhouette
#' @importFrom ggplot2 ggplot aes geom_line geom_point annotate labs theme_minimal
#' @keywords internal
#' @noRd
doSilhouette <- function(myDf,
                         rangeNumberOfClusters = 2:15,
                         numberOfKmeansRestarts = 100,
                         main,
                         forcedClusterNumber=NULL) {

  mySilhouetteWidth <- NULL
  for (i in 1:length(rangeNumberOfClusters)) {
    myClus <- suppressWarnings(stats::kmeans(myDf, centers=rangeNumberOfClusters[i],
                                      iter.max=2000, nstart=numberOfKmeansRestarts))
    sil <- cluster::silhouette(myClus$cluster, dist(myDf))
    mySilhouetteWidth[i] <- mean(sil[, "sil_width"])
  }

  df <- data.frame("NumberOfClusters"=rangeNumberOfClusters,
                   "SilhouetteWidth"=mySilhouetteWidth)

  #Find the cluster number with the maximum silhouette width
  if (is.null(forcedClusterNumber)) {
    numberOfClusters <- rangeNumberOfClusters[which.max(mySilhouetteWidth)]
  } else {
    numberOfClusters <- forcedClusterNumber
  }

  silhouettePlot <-
    ggplot2::ggplot(df, ggplot2::aes(x=.data$NumberOfClusters, y=.data$SilhouetteWidth)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(shape=19) +
    ggplot2::annotate("point", x = numberOfClusters, y = mySilhouetteWidth[which(rangeNumberOfClusters==numberOfClusters)],
                      color = "red", size = 3, shape=15) +
    ggplot2::labs(title=paste("Silhouette plot", main), x="Number of clusters", y="Average silhouette width") +
    ggplot2::theme_minimal()

  myKmeans <- suppressWarnings(stats::kmeans(myDf, nstart = numberOfKmeansRestarts,
                                             iter.max = 2000, centers = numberOfClusters))

  return(list(silhouettePlot = silhouettePlot,
              numberOfClusters = numberOfClusters,
              clusterResults = myKmeans$cluster))
}
