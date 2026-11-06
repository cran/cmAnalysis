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
#' When multiple groups are returned, each data frame has a \code{conceptMapValidation} attribute. Its value is
#' \code{"passes_current_validator"} if validation succeeds, or the validation error message otherwise. 
#' Groups are retained regardless of this status. Passing validation does not guarantee that every subsequent
#' analysis is feasible. When one group is requested, the returned data frame has no such attribute.
#' 
#' @details
#' This function clusters sorters based on their sorting behavior using hierarchical clustering
#' with Ward's method. If \code{numberOfSorterClusters = "auto"}, the silhouette method is used
#' to determine the optimal number of clusters within the range specified by \code{rangeNumberOfClusters}.
#'
#' For multiple groups, validation status is recorded independently of \code{verbose}. The \code{verbose}
#' argument controls console output only. Graphical output includes a dendrogram and silhouette plot
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

  for (flag in c("graph", "verbose")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      stop(sprintf("%s must be TRUE or FALSE.", flag))
    }
  }

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  #get the numbers on sorters and statements from the data
  allSorters <- unique(sort(CMData$sorterID))
  nSorters <- length(allSorters)
  nStatements <- length(unique(sort(CMData$statement)))

  if (is.character(numberOfSorterClusters)) {
    if (length(numberOfSorterClusters) != 1 || is.na(numberOfSorterClusters)) {
      stop("numberOfSorterClusters must be 'auto' or one integer.")
    }
    numberOfSorterClusters <- tolower(numberOfSorterClusters)
  }

  isAuto <- identical(numberOfSorterClusters, "auto")
  whole <- function(x) is.numeric(x) && length(x) > 0 &&
    all(is.finite(x)) && all(x == floor(x))
  if (nSorters < 2) stop("Sorter clustering requires at least two sorters.")
  if (isAuto) {
    if (!whole(rangeNumberOfClusters)) stop("Cluster range must contain finite integers.")
    requested <- sort(unique(rangeNumberOfClusters))
    rangeNumberOfClusters <- requested[requested >= 2 & requested < nSorters]
    if (!length(rangeNumberOfClusters)) {
      stop("No automatic silhouette k: at least three sorters are required.")
    }
    if (length(rangeNumberOfClusters) != length(requested)) {
      warning("Infeasible sorter cluster candidates were omitted.", call. = FALSE)
    }
  } else {
    if (!whole(numberOfSorterClusters) || length(numberOfSorterClusters) != 1 ||
        numberOfSorterClusters < 1 || numberOfSorterClusters > nSorters) {
      stop("Fixed sorter k must be one integer between 1 and nSorters.")
    }
  }

  #add column StatementNumber for each unique statement
  CMData$StatementNumber <- as.numeric(factor(CMData$statement, levels=unique(CMData$statement)))

  rawData <- matrix(NA, nrow=nSorters, ncol=((nStatements - 1) * nStatements) / 2)

  #collect the data from each individual user
  for (i in 1:nSorters) {
    sorterRows <- which(CMData$sorterID == allSorters[i])
    userSubset <- CMData[sorterRows, , drop = FALSE]
    
    #this matrix stores per user which cards go together
    #create it again for each user
    tmp <- matrix(0, nrow=nStatements, ncol=nStatements)

    #now get each pile to see what is sorted together
    #Unsorted items contribute no co-occurrences.
    sorted <- !is.na(userSubset$stackID) &
      nzchar(trimws(as.character(userSubset$stackID)))

    for (j in unique(userSubset$stackID[sorted])) {
      pileRows <- which(userSubset$stackID == j)
      pileSubset <- userSubset[pileRows, , drop = FALSE]

      idx <- pileSubset$StatementNumber
      #setting it to 1 means they are grouped together
      tmp[idx, idx] <- 1
    }

    rawData[i,] <- tmp[lower.tri(tmp)]
  }

  #first we cluster the data, to see how many groups there are

  #calculate pairwise distances between rows
  theDistances <- stats::dist(rawData, method = "euclidean")

  #Return one group when automatic clustering has no variation to analyse.
  if (isAuto && all(theDistances == 0)) {
    warning("All sorters have identical sorting profiles, returning one sorter cluster.", call. = FALSE)
    return(CMData)
  }

  if (graph) {
    oldPar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldPar), add = TRUE)
  }

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
           xlab="Euclidean distance with Ward's Linkage", sub = NA)
      #visualize clusters
      stats::rect.hclust(hc, k=optimalNumberofSorterClusters, border = "red")
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
      idx <- CMData$sorterID %in% allSorters[clusteredSorters == i]
      sorterCMData[[i]] <- CMData[idx,]

      group <- sorterCMData[[i]]
      groupSize <- length(unique(group$sorterID))
      mapStatus <- tryCatch({
        checkConceptMapData(group)
        "passes_current_validator"
      }, error = function(e) conditionMessage(e))
      attr(sorterCMData[[i]], "conceptMapValidation") <- mapStatus

      if (verbose) {
        cat(sprintf("    Number of sorters in this cluster: %d\n", groupSize))
        cat(sprintf("    Concept-map validation: %s\n", mapStatus))
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

