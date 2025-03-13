#' Cross-Cluster Mapping Between Concept Maps
#'
#' This function compares two concept maps by aligning their clustering results
#' and visualizing the correspondence between clusters. It identifies matches
#' between clusters from the two maps and highlights differences visually.
#'
#' @param conceptMap1 An object of class \code{"conceptMap"} representing the first concept map.
#' @param conceptMap2 An object of class \code{"conceptMap"} representing the second concept map.
#'
#' @details The function aligns clusters between two concept maps using an optimal matching
#' algorithm. It first creates a matching matrix based on the overlap between clusters
#' in the two maps. Then, it uses the Hungarian algorithm (via the \code{solve_LSAP} function
#' from the \code{clue} package) to find an optimal alignment of clusters.
#'
#' The output is a plot that shows the alignment of clusters from the two concept maps,
#' with connecting lines colored to indicate matches or mismatches. Statements not
#' clustered in both maps are highlighted in grey.
#'
#' @return The function does not return a value but generates a \code{ggplot2} visualization.
#'
#'
#' @seealso \code{\link[clue]{solve_LSAP}}, \code{\link[ggplot2]{ggplot}}
#'
#' @import ggplot2
#' @import clue
#'
#' @examples
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.90, attributeWeights=c(1,1,1,1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Subject sorter cluster 1 to concept mapping using default "network" method
#' myCMAnalysis1 <- conceptMapping(myCMDataBySorters[[1]])
#'
#' # Subject sorter cluster 3 to concept mapping using default "network" method
#' myCMAnalysis3 <- conceptMapping(myCMDataBySorters[[3]])
#'
#' # Visualise comparison of results of two sorter clusters
#' crossClusterMap(myCMAnalysis1, myCMAnalysis3)
#'
#' @export
crossClusterMap <- function(conceptMap1, conceptMap2) {

  if (!inherits(conceptMap1, "conceptMap")) {
    stop("Object conceptMap1 should be of class conceptMap")
  }

  if (!inherits(conceptMap2, "conceptMap")) {
    stop("Object conceptMap2 should be of class conceptMap")
  }

  #create object for common statements and how they are clustered in each concept map
  uniqueStatements <- union(conceptMap1$allStatements$Statement, conceptMap2$allStatements$Statement)
  clusteringResults <- matrix(NA, ncol=2, nrow=length(uniqueStatements))
  rownames(clusteringResults) <- uniqueStatements

  clusteringResults[names(conceptMap1$clusterResults), 1] <- conceptMap1$clusterResults
  clusteringResults[names(conceptMap2$clusterResults), 2] <- conceptMap2$clusterResults

  clusteringResult1 <- clusteringResults[,1]
  clusteringResult2 <- clusteringResults[,2]

  #Get the number of unique clusters in both results
  nClusters1 <- length(unique(clusteringResult1[!is.na(clusteringResult1)]))
  nClusters2 <- length(unique(clusteringResult2[!is.na(clusteringResult2)]))

  #Determine the size of the larger dimension to make the matrix square
  maxClusters <- max(nClusters1, nClusters2)

  #Create a square matrix with dimensions maxClusters x maxClusters
  matchingMatrix <- matrix(0, maxClusters, maxClusters)

  #Fill the matching matrix with actual matching counts from clustering results
  #Use the original dimensions (nClusters1 x nClusters2) for the filling process
  for (i in 1:nClusters1) {
    for (j in 1:nClusters2) {
      matchingMatrix[i, j] <- sum(clusteringResult1 == i & clusteringResult2 == j, na.rm = TRUE)
    }
  }

  #Now apply the solve_LSAP function on the square matching matrix
  optimalMatch <- clue::solve_LSAP(matchingMatrix, maximum=TRUE)

  clusteringResult2 <- optimalMatch[clusteringResult2]

  itemOrder1 <- order(clusteringResult1)
  itemOrder2 <- order(clusteringResult2)

  left2right <- sapply(1:length(itemOrder1), function(i) {
    which(itemOrder2 == i)
  })

  plotData <- data.frame( Item=1:length(clusteringResult1),
                          Cluster1=clusteringResult1,
                          Cluster2=clusteringResult2,
                          itemOrder1=itemOrder1,
                          itemOrder2=itemOrder2,
                          Left2right=left2right
  )

  myCols <- ifelse(plotData$Cluster1 == plotData$Cluster2, "forestgreen", "darkorange")

  mainTitle <- sprintf("Cross cluster map")

  #prepare plotting symbols
  myPCH1 <- as.character(plotData$Cluster1)
  myPCH1[is.na(myPCH1)] <- "X"
  myPCH2 <- as.character(plotData$Cluster2)
  myPCH2[is.na(myPCH2)] <- "X"

  #find if there are statements excluded from both clusterings
  idx <- which(is.na(plotData$Cluster1) & is.na(plotData$Cluster2))
  myCols[idx] <- "grey"

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_text(data=plotData, ggplot2::aes(x=1 - 0.03, y=1:nrow(plotData),
                                                   label=rownames(plotData)[itemOrder1]), hjust=1, size=3) +
    ggplot2::geom_point(data=plotData[plotData$itemOrder1,], ggplot2::aes(x=1,
                                                                          y=1:nrow(plotData)), color=1, shape=myPCH1[plotData$itemOrder1], size=3)  +

    ggplot2::geom_text(data=plotData, ggplot2::aes(x=2 + 0.03, y=1:nrow(plotData),
                                                   label=rownames(plotData)[itemOrder2]), hjust=0, size=3 ) +
    ggplot2::geom_point(data=plotData[plotData$itemOrder2,], ggplot2::aes(x=2,
                                                                          y=1:nrow(plotData)), color=1, shape=myPCH2[plotData$itemOrder2], size=3) +

    ggplot2::geom_segment(data=plotData, ggplot2::aes(x=1, y=order(itemOrder1), xend=2, yend=.data$Left2right),
                          color=myCols, linewidth=0.7, alpha=0.7 ) +

    ggplot2::labs(x="Sorter clusters", y="Statement (arranged per cluster)", title=mainTitle,
                  shape="Cluster", color="Cluster") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.5, 0.5))) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank() )

  print(p1)
}
