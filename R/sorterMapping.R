#' Cluster Sorters in Concept Mapping Data
#'
#' This function performs clustering of sorters in concept mapping data
#' based on their sorting behavior. It uses hierarchical clustering and allows
#' the automatic determination of the optimal number of clusters or a user-defined number.
#'
#' @param CMData A data frame containing concept mapping data. It must include the columns:
#'        \code{"sorterID"}, \code{"statement"}, and \code{"stackID"}.
#' @param numberOfSorterClusters Either a character string (\code{"auto"}) to automatically determine
#'        the optimal number of clusters or an integer specifying the desired number of clusters.
#' @param verbose Logical, if \code{TRUE}, additional information about the processing steps is printed to the console.
#' @param rangeNumberOfClusters A vector of integers specifying the range of clusters to evaluate
#'        when \cr \code{numberOfSorterClusters = "auto"}. Default is \code{2:15}.
#' @param graph Logical. If \code{TRUE}, plots the dendrogram and silhouette method results. Default is \code{TRUE}.
#'
#' @return A list of data frames, each representing the concept mapping data for a cluster of sorters.
#'         If only one cluster is found, the original \code{CMData} is returned.
#'
#' @details
#' This function clusters sorters based on their sorting behavior using hierarchical clustering
#' with Ward's method. If \code{numberOfSorterClusters = "auto"}, the silhouette method is used
#' to determine the optimal number of clusters within the range specified by \code{rangeNumberOfClusters}.
#'
#' Each cluster's data is validated for its suitability for concept mapping, and cluster-specific data
#' is returned as a list of data frames. Graphical output includes a dendrogram and silhouette plot
#' if \code{graph = TRUE}.
#'
#' @examples
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.90, attributeWeights=c(1,1,1,1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Subject the data to sorter cluster analysis with a predefined number of sorter clusters
#' myCMDataBySorters <- sorterMapping(myCMData, numberOfSorterClusters=2)
#'
#' @export
sorterMapping <- function(CMData,
                          numberOfSorterClusters = "auto",
                          verbose = TRUE,
                          rangeNumberOfClusters = 2:15,
                          graph = TRUE) {

  oldPar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar))

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  #get the numbers on sorters and statements from the data
  allSorters <- unique(sort(CMData$sorterID))
  nSorters <- length(allSorters)
  nStatements <- length(unique(sort(CMData$statement)))

  if (is.character(numberOfSorterClusters)) {
    numberOfSorterClusters <- tolower(numberOfSorterClusters)
  }

  #make sure that the 1 cluster solution will not be used
  if (numberOfSorterClusters == "auto" & (1 %in% rangeNumberOfClusters)) {
    if (verbose) {
      cat(sprintf("Dropping cluster size of 1 in considered clusters."))
    }
    rangeNumberOfClusters <- setdiff(rangeNumberOfClusters, 1)
  }

  if (max(rangeNumberOfClusters) >= nSorters) {
    stop("Number of sorter clusters should be less than the number of sorters.")
  }

  #add column StatementNumber for each unique statement
  CMData$StatementNumber <- as.numeric(factor(CMData$statement, levels=unique(CMData$statement)))

  rawData <- matrix(NA, nrow=nSorters, ncol=((nStatements - 1) * nStatements) / 2)

  #collect the data from each individual user
  for (i in 1:nSorters) {
    userSubset <- subset(CMData, CMData$sorterID == allSorters[i])

    #this matrix stores per user which cards go together
    #create it again for each user
    tmp <- matrix(0, nrow=nStatements, ncol=nStatements)

    #now get each pile to see what is sorted together
    for (j in unique(userSubset$stackID)) {
      pileSubset <- subset(userSubset, userSubset$stackID == j)
      idx <- pileSubset$StatementNumber
      #setting it to 1 means they are grouped together
      tmp[idx, idx] <- 1
    }

    rawData[i,] <- tmp[lower.tri(tmp)]
  }

  #first we cluster the data, to see how many groups there are

  #calculate pairwise distances between rows
  theDistances <- stats::dist(rawData, method="euclidean")

  #Perform hierarchical clustering using Ward's method
  hc <- stats::hclust(theDistances, method="ward.D2")

  if (numberOfSorterClusters == "auto") {
    #Determine optimal number of clusters using Silhouette method from package cluster
    mySilhouetteWidth <- sapply(rangeNumberOfClusters, function(i) {
      sil <- cluster::silhouette(stats::cutree(hc, k=i), theDistances)
      return(mean(sil[, "sil_width"]))
    })

    optimalNumberofSorterClusters <- rangeNumberOfClusters[which.max(mySilhouetteWidth)]
    if (verbose) {
      cat(sprintf("Optimal number of sorter clusters based on Silhouette method: %d.\n",
                optimalNumberofSorterClusters))
    }

    if (graph) {
      #arrange two plots on one row
      graphics::par(mfrow=c(1, 2))
      plot(rangeNumberOfClusters, mySilhouetteWidth, type="b", pch=19,
           xlab="Number of clusters", ylab="Average silhouette width",
           main="Sorter Clusters by Silhouette Method")

      graphics::points(optimalNumberofSorterClusters, mySilhouetteWidth[which.max(mySilhouetteWidth)],
                       col="red", pch=15, cex=1.5)

      #Plot the dendrogram for total (mixed) data
      plot(hc,labels=FALSE, main="Dendrogram",
           xlab="Euclidean distance with Ward's Linkage", sub=NA)
      #visualize clusters
      stats::rect.hclust(hc, k=optimalNumberofSorterClusters, border="red")
      #return to one plot per figure
      graphics::par(mfrow=c(1, 1))
    }
  } else {
    optimalNumberofSorterClusters <- numberOfSorterClusters
    if (verbose) {
      cat(sprintf("Optimal number of sorter clusters (as set by user): %d.\n",
                optimalNumberofSorterClusters))
    }

    if (graph) {
      #Plot the dendrogram for total (mixed) data
      plot(hc,labels=FALSE, main="Dendrogram",
           xlab="Euclidean distance with Ward's Linkage", sub=NA)
      #visualize clusters
      stats::rect.hclust(hc, k=optimalNumberofSorterClusters, border="red")
    }
  }

  #now divide the raw data into sorter clusters and return a list of raw data frames
  clusteredSorters <- stats::cutree(hc, k=optimalNumberofSorterClusters)

  if (optimalNumberofSorterClusters > 1) {
    if (verbose) {
      cat(sprintf("Creating %d sorter clusters.\n", optimalNumberofSorterClusters))
    }
    sorterCMData <- list()
    for (i in 1:optimalNumberofSorterClusters) {
      if (verbose) {
        cat(sprintf("  Creating sorter cluster: %d\n", i))
      }
      idx <- CMData$sorterID %in% allSorters[clusteredSorters==i]
      sorterCMData[[i]] <- CMData[idx,]

      if (verbose) {
        cat(sprintf("    Number of sorters in this cluster: %d\n", numberOfSorters(sorterCMData[[i]])))
        cat(sprintf("    Data validity: "))

        #check resulting sorter data frame
        if (checkConceptMapData(sorterCMData[[i]])) {
          cat(sprintf("OK.\n"))
        } else {
          stop("Concept data not valid.")
        }
      }
    }
  } else {
    if (verbose) {
      cat(sprintf("Creating 1 sorter cluster.\n"))
    }
    sorterCMData <- CMData
  }

  return(sorterCMData)
}

