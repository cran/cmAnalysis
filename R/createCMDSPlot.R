#' Create Classical MDS Plot with Clustering
#'
#' This helper function performs Classical Multidimensional Scaling (CMDS) on a co-occurrence matrix
#' and generates a 2D plot with clusters. It also computes silhouette results based on the clustering.
#'
#' @param coOccurrenceMatrix A numeric matrix representing the co-occurrence data.
#' @param numberOfClusters The number of clusters to form, or "auto" to automatically determine the optimal number of clusters.
#' @param main A string for the title of the plot.
#' @param rangeNumberOfClusters A range of possible cluster numbers to evaluate for optimal clustering.
#' @param numberOfKmeansRestarts The number of restarts for k-means clustering.
#'
#' @return A list with:
#'   - `cmdsPlot`: A ggplot object of the CMDS plot with clusters.
#'   - `cmdsSilhouettePlot`: A ggplot object of the silhouette plot for clustering evaluation.
#'   - `cmdsNumberOfClusters`: The number of clusters identified.
#'   - `cmdsClusterResults`: The cluster assignment for each point.
#'
#' @importFrom stats dist cmdscale
#' @importFrom factoextra fviz_cluster
#' @importFrom ggplot2 theme_minimal ggtitle
#' @noRd
createCMDSPlot <- function(coOccurrenceMatrix,
                           numberOfClusters,
                           main,
                           rangeNumberOfClusters = 2:15,
                           numberOfKmeansRestarts = 100) {

  #Calculate the distance matrix for cluster k
  myDistance <- dist(coOccurrenceMatrix, method='euclidean')
  #Perform classical multidimensional scaling (MDS) in 2 dimensions
  CMcmds <- cmdscale(myDistance, k=2)

  #kmeans op CMcmds voor CMDSClusterNumber
  #warning("CMDS based clustering of statements is suboptimal method.")
  if (numberOfClusters=="auto") {
    numberOfClusters <- NULL
  }

  main <- paste("CMDS", main)

  mySilhouetteResults <- doSilhouette(CMcmds, rangeNumberOfClusters,
                                      numberOfKmeansRestarts, main, forcedClusterNumber=numberOfClusters )

  cmdsKmeans <- suppressWarnings(stats::kmeans(CMcmds, nstart=numberOfKmeansRestarts,
                                               iter.max=2000, centers=mySilhouetteResults$numberOfClusters))

  #Create a CMDS cluster plot for cluster k
  cmdsKMeansPlot <- factoextra::fviz_cluster(cmdsKmeans,
                                             data=as.data.frame(CMcmds),
                                             repel=TRUE,
                                             xlab="MDS Dimension 1",
                                             ylab="MDS Dimension 2") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(paste("Point cluster map", main))

  return(list(cmdsPlot=cmdsKMeansPlot,
              cmdsSilhouettePlot=mySilhouetteResults$silhouettePlot,
              cmdsNumberOfClusters=mySilhouetteResults$numberOfClusters,
              cmdsClusterResults=mySilhouetteResults$clusterResults))
}

