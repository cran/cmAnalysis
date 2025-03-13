#' Create a Co-Occurrence Matrix from Concept Map Data
#'
#' This helper function generates a co-occurrence matrix, which represents how often each pair of statements
#' is grouped together across all sorters in the concept mapping dataset.
#'
#' @param CMData A data frame containing concept map data. The data must include the following columns:
#'        \code{sorterID}, \code{stackID}, and \code{StatementNumber}.
#'
#' @return A square matrix where rows and columns correspond to statements, and each element represents
#'         the number of times two statements were grouped together by sorters.
#'
#' @details
#' The resulting co-occurrence matrix is square and symmetric and includes row and column names derived from
#' the statements in the concept map data.
#'
#' @keywords internal
#' @noRd
createCoOccurrenceMatrix <- function(CMData) {

  #get the numbers on sorters and statements from the data
  allSorters <- unique(sort(CMData$sorterID))
  nSorters <- length(allSorters)
  nStatements <- length(unique(sort(CMData$statement)))

  rawData <- matrix(NA, nrow=nSorters, ncol=((nStatements - 1) * nStatements) / 2)

  #collect the data from each sorter
  for (i in 1:nSorters) {
    userSubset <- subset(CMData, CMData$sorterID == allSorters[i])

    #this matrix stores per sorter which statements go together
    #it is created for each sorter again
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

  #Calculate p
  p <- (1 + sqrt(1 + 8 * ncol(rawData))) / 2

  #Check if p is an integer, i.e., the number of columns should be a perfect square
  if (p != floor(p)) {
    stop(paste("The number of columns in is not a perfect square. Check your data."))
  }

  #Initialize the sum matrix with zeros
  coOccurrenceMatrix <- matrix(0, nrow=p, ncol=p)

  #Iterate over each row in the data
  for (h in 1:nrow(rawData)) {
    clusterMatrix <- matrix(0, nrow=p, ncol=p)

    #Fill the lower triangular part with the extracted vector
    clusterMatrix[lower.tri(clusterMatrix)] <- as.numeric(rawData[h, ])

    #Restore symmetry by adding the transpose
    clusterMatrix <- clusterMatrix + t(clusterMatrix)

    #Set the diagonal to 1s (since they were 1s in the original matrix)
    diag(clusterMatrix) <- 1

    #Add the row matrix to the sum matrix
    coOccurrenceMatrix <- coOccurrenceMatrix + clusterMatrix
  }

  overviewStatements <- createStatementOverview(CMData)

  colnames(coOccurrenceMatrix) <- overviewStatements$Statement
  rownames(coOccurrenceMatrix) <- overviewStatements$Statement

  return(coOccurrenceMatrix)
}
