#' Cross-Cluster Mapping Between Concept Maps
#'
#' This function compares two concept maps by visualizing how statements move
#' between clusters. Cluster labels in the second concept map can optionally be
#' relabeled to match the first concept map as well as possible.
#'
#' @param conceptMap1 An object of class \code{"conceptMap"}.
#' @param conceptMap2 An object of class \code{"conceptMap"}.
#' @param alignClusterLabels Logical. If \code{TRUE}, cluster labels in
#'   \code{conceptMap2} are relabeled to match \code{conceptMap1} as well as
#'   possible before plotting. Default is \code{TRUE}.
#' @param similarity Character string specifying how overlap between clusters is
#'   scored. Options are \code{"dice"}, \code{"jaccard"}, \code{"overlap"},
#'   and \code{"count"}. Default is \code{"dice"}.
#' @param minimumSimilarity Numeric value. A matched pair of clusters is only
#'   accepted when its similarity is larger than this value. Default is \code{0}.
#' @param returnPlotData Logical. If \code{TRUE}, a list with the plot, plot data,
#'   and cluster label mapping is returned. If \code{FALSE}, only the plot is
#'   returned invisibly. Default is \code{FALSE}.
#'
#' @details
#' Cluster numbers are arbitrary within each concept map. Therefore, cluster 1 in
#' \code{conceptMap1} does not necessarily correspond to cluster 1 in
#' \code{conceptMap2}. When \code{alignClusterLabels = TRUE}, this function uses
#' pairwise statement overlap to relabel the clusters in \code{conceptMap2}. The
#' matching is solved with the Hungarian algorithm via \code{clue::solve_LSAP()}.
#'
#' The default similarity measure is Dice similarity:
#' \deqn{2 |A \cap B| / (|A| + |B|)}
#' This is usually preferable to raw overlap counts when clusters have unequal
#' sizes.
#'
#' Different numbers of clusters are allowed. Extra clusters in \code{conceptMap2}
#' that cannot be matched to a cluster in \code{conceptMap1} receive new labels
#' above the highest label used in \code{conceptMap1}. They are still ordered near
#' the most similar cluster when possible.
#'
#' Similarity scores use only statements with a cluster assignment in both maps.
#' Statements present in only one map are still displayed, but do not contribute
#' to these scores.
#'
#' Matching first maximizes the total similarity across cluster pairs.
#' The \code{minimumSimilarity} threshold is applied afterwards: matched pairs
#' with a similarity less than or equal to the threshold are rejected.
#' Matching is not repeated after rejection.
#' 
#' @return A \code{ggplot} object, invisibly by default. If
#'   \code{returnPlotData = TRUE}, a list with elements \code{plot},
#'   \code{plotData}, and \code{clusterNumberMapping} is returned.
#'
#' @import ggplot2
#' @import clue
#'
#' @examples
#' # Simulate data with custom parameters
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters = 40, pCorrect = .90, attributeWeights = c(1, 1, 1, 1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Subject sorter cluster 1 to concept mapping using the default network method
#' myCMAnalysis1 <- conceptMapping(myCMDataBySorters[[1]])
#'
#' # Subject sorter cluster 3 to concept mapping using the default network method
#' myCMAnalysis3 <- conceptMapping(myCMDataBySorters[[3]])
#'
#' # Visualise comparison of results of two sorter clusters
#' crossClusterMap(myCMAnalysis1, myCMAnalysis3)
#'
#' @export
crossClusterMap <- function(conceptMap1,
                            conceptMap2,
                            alignClusterLabels = TRUE,
                            similarity = c("dice", "jaccard", "overlap", "count"),
                            minimumSimilarity = 0,
                            returnPlotData = FALSE) {

  similarity <- match.arg(similarity)

  if (!inherits(conceptMap1, "conceptMap")) {
    stop("Object conceptMap1 should be of class conceptMap", call. = FALSE)
  }

  if (!inherits(conceptMap2, "conceptMap")) {
    stop("Object conceptMap2 should be of class conceptMap", call. = FALSE)
  }

  if (!is.numeric(minimumSimilarity) || length(minimumSimilarity) != 1 || is.na(minimumSimilarity)) {
    stop("minimumSimilarity should be a single numeric value.", call. = FALSE)
  }

  uniqueStatements <- union(conceptMap1$allStatements$Statement,
                            conceptMap2$allStatements$Statement)

  clusteringResults <- matrix(NA_real_, ncol = 2, nrow = length(uniqueStatements))
  rownames(clusteringResults) <- uniqueStatements

  clusteringResults[names(conceptMap1$clusterResults), 1] <- conceptMap1$clusterResults
  clusteringResults[names(conceptMap2$clusterResults), 2] <- conceptMap2$clusterResults

  clusteringResult1 <- clusteringResults[, 1]
  clusteringResult2 <- clusteringResults[, 2]
  orderingCluster2 <- clusteringResult2
  clusterNumberMapping <- NULL

  if (alignClusterLabels) {
    alignment <- pairwiseClusterLabelAlignment(
      clusteringResult1 = clusteringResult1,
      clusteringResult2 = clusteringResult2,
      similarity = similarity,
      minimumSimilarity = minimumSimilarity
    )

    clusteringResult2 <- alignment$alignedClusterResult2
    orderingCluster2 <- alignment$orderingCluster2
    clusterNumberMapping <- alignment$clusterNumberMapping
  }

  statementOrder1 <- rownames(clusteringResults)[order(
    is.na(clusteringResult1),
    clusteringResult1,
    rownames(clusteringResults)
  )]

  statementOrder2 <- rownames(clusteringResults)[order(
    is.na(orderingCluster2),
    orderingCluster2,
    clusteringResult2,
    rownames(clusteringResults)
  )]

  plotData <- data.frame(
    Statement = rownames(clusteringResults),
    Cluster1 = clusteringResult1,
    Cluster2 = clusteringResult2,
    Y1 = match(rownames(clusteringResults), statementOrder1),
    Y2 = match(rownames(clusteringResults), statementOrder2),
    stringsAsFactors = FALSE
  )

  plotData$LineColor <- ifelse(
    is.na(plotData$Cluster1) | is.na(plotData$Cluster2),
    "grey",
    ifelse(plotData$Cluster1 == plotData$Cluster2, "forestgreen", "darkorange")
  )

  plotData$ClusterLabel1 <- ifelse(is.na(plotData$Cluster1), "X", as.character(plotData$Cluster1))
  plotData$ClusterLabel2 <- ifelse(is.na(plotData$Cluster2), "X", as.character(plotData$Cluster2))

  leftPlotData <- plotData[match(statementOrder1, plotData$Statement), ]
  rightPlotData <- plotData[match(statementOrder2, plotData$Statement), ]

  mainTitle <- "Cross cluster map"

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = plotData,
      ggplot2::aes(x = 1, y = .data$Y1, xend = 2, yend = .data$Y2),
      color = plotData$LineColor,
      linewidth = 0.7,
      alpha = 0.7
    ) +
    ggplot2::geom_text(
      data = leftPlotData,
      ggplot2::aes(x = 1 - 0.03, y = .data$Y1, label = .data$Statement),
      hjust = 1,
      size = 3
    ) +
    ggplot2::geom_text(
      data = leftPlotData,
      ggplot2::aes(x = 1, y = .data$Y1, label = .data$ClusterLabel1),
      size = 3
    ) +
    ggplot2::geom_text(
      data = rightPlotData,
      ggplot2::aes(x = 2 + 0.03, y = .data$Y2, label = .data$Statement),
      hjust = 0,
      size = 3
    ) +
    ggplot2::geom_text(
      data = rightPlotData,
      ggplot2::aes(x = 2, y = .data$Y2, label = .data$ClusterLabel2),
      size = 3
    ) +
    ggplot2::labs(
      x = "Sorter clusters",
      y = "Statement (arranged per cluster)",
      title = mainTitle
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.5, 0.5))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  print(p1)

  if (returnPlotData) {
    return(list(
      plot = p1,
      plotData = plotData,
      clusterNumberMapping = clusterNumberMapping
    ))
  }

  invisible(p1)
}

pairwiseClusterLabelAlignment <- function(clusteringResult1,
                                          clusteringResult2,
                                          similarity = c("dice", "jaccard", "overlap", "count"),
                                          minimumSimilarity = 0) {

  similarity <- match.arg(similarity)

  leftLabels <- sort(unique(clusteringResult1[!is.na(clusteringResult1)]))
  rightLabels <- sort(unique(clusteringResult2[!is.na(clusteringResult2)]))

  if (length(leftLabels) == 0 || length(rightLabels) == 0) {
    return(list(
      alignedClusterResult2 = clusteringResult2,
      orderingCluster2 = clusteringResult2,
      clusterNumberMapping = data.frame(
        OldCluster2 = rightLabels,
        NewCluster2 = rightLabels,
        OrderingCluster2 = rightLabels,
        Similarity = NA_real_
      )
    ))
  }

  similarityMatrix <- clusterSimilarityMatrix(
    clusteringResult1 = clusteringResult1,
    clusteringResult2 = clusteringResult2,
    leftLabels = leftLabels,
    rightLabels = rightLabels,
    similarity = similarity
  )

  matrixSize <- max(nrow(similarityMatrix), ncol(similarityMatrix))
  paddedSimilarityMatrix <- matrix(0, nrow = matrixSize, ncol = matrixSize)
  paddedSimilarityMatrix[seq_len(nrow(similarityMatrix)), seq_len(ncol(similarityMatrix))] <- similarityMatrix

  optimalMatch <- clue::solve_LSAP(paddedSimilarityMatrix, maximum = TRUE)

  rightToAligned <- rep(NA_real_, length(rightLabels))
  names(rightToAligned) <- as.character(rightLabels)

  rightToSimilarity <- rep(NA_real_, length(rightLabels))
  names(rightToSimilarity) <- as.character(rightLabels)

  for (i in seq_along(leftLabels)) {
    matchedColumn <- optimalMatch[i]

    if (matchedColumn <= length(rightLabels)) {
      matchedSimilarity <- similarityMatrix[i, matchedColumn]

      if (matchedSimilarity > minimumSimilarity) {
        rightToAligned[matchedColumn] <- leftLabels[i]
        rightToSimilarity[matchedColumn] <- matchedSimilarity
      }
    }
  }

  nextClusterLabel <- max(leftLabels)

  for (j in seq_along(rightLabels)) {
    if (is.na(rightToAligned[j])) {
      nextClusterLabel <- nextClusterLabel + 1
      rightToAligned[j] <- nextClusterLabel
    }
  }

  rightToOrder <- rep(NA_real_, length(rightLabels))
  names(rightToOrder) <- as.character(rightLabels)

  for (j in seq_along(rightLabels)) {
    bestSimilarity <- max(similarityMatrix[, j], na.rm = TRUE)

    if (is.finite(bestSimilarity) && bestSimilarity > minimumSimilarity) {
      rightToOrder[j] <- leftLabels[which.max(similarityMatrix[, j])]
    } else {
      rightToOrder[j] <- rightToAligned[j]
    }
  }

  alignedClusterResult2 <- clusteringResult2
  orderingCluster2 <- clusteringResult2

  validIndex <- !is.na(clusteringResult2)
  alignedClusterResult2[validIndex] <- rightToAligned[as.character(clusteringResult2[validIndex])]
  orderingCluster2[validIndex] <- rightToOrder[as.character(clusteringResult2[validIndex])]

  clusterNumberMapping <- data.frame(
    OldCluster2 = rightLabels,
    NewCluster2 = as.numeric(rightToAligned),
    OrderingCluster2 = as.numeric(rightToOrder),
    Similarity = as.numeric(rightToSimilarity),
    stringsAsFactors = FALSE
  )

  return(list(
    alignedClusterResult2 = alignedClusterResult2,
    orderingCluster2 = orderingCluster2,
    clusterNumberMapping = clusterNumberMapping,
    similarityMatrix = similarityMatrix
  ))
}

clusterSimilarityMatrix <- function(clusteringResult1,
                                    clusteringResult2,
                                    leftLabels,
                                    rightLabels,
                                    similarity = c("dice", "jaccard", "overlap", "count")) {

  similarity <- match.arg(similarity)
  completeIndex <- !is.na(clusteringResult1) & !is.na(clusteringResult2)

  similarityMatrix <- matrix(
    0,
    nrow = length(leftLabels),
    ncol = length(rightLabels),
    dimnames = list(as.character(leftLabels), as.character(rightLabels))
  )

  for (i in seq_along(leftLabels)) {
    for (j in seq_along(rightLabels)) {
      inLeftCluster <- completeIndex & clusteringResult1 == leftLabels[i]
      inRightCluster <- completeIndex & clusteringResult2 == rightLabels[j]

      intersectionSize <- sum(inLeftCluster & inRightCluster)
      leftSize <- sum(inLeftCluster)
      rightSize <- sum(inRightCluster)
      unionSize <- sum(inLeftCluster | inRightCluster)
      minimumSize <- min(leftSize, rightSize)

      if (similarity == "count") {
        similarityMatrix[i, j] <- intersectionSize
      }

      if (similarity == "dice" && (leftSize + rightSize) > 0) {
        similarityMatrix[i, j] <- 2 * intersectionSize / (leftSize + rightSize)
      }

      if (similarity == "jaccard" && unionSize > 0) {
        similarityMatrix[i, j] <- intersectionSize / unionSize
      }

      if (similarity == "overlap" && minimumSize > 0) {
        similarityMatrix[i, j] <- intersectionSize / minimumSize
      }
    }
  }

  return(similarityMatrix)
}
