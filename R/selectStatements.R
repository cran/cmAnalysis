#' Select Significant Statements from Concept Map Data
#'
#' This function selects statements from a concept map dataset based on their significance in terms of co-occurrence.
#' It applies a chi-squared test on the co-occurrence matrix of the statements to identify those that are statistically significant
#' (i.e., those that co-occur more frequently than would be expected by chance).
#'
#' @param CMData A data frame containing concept map data. The data should have at least the following columns:
#'   - `statement`: The text of the statement.
#'   - `sorterID`: The identifier for the sorter.
#'   - `stackID`: The identifier for the stack.
#' @param significanceThreshold A numeric value representing the significance threshold for the chi-squared test.
#'   Statements with p-values less than this threshold are considered significant. Default is 0.05.
#' @param verbose Logical, if \code{TRUE}, additional information about the processing steps is printed to the console.
#' @param ... Additional arguments to be passed to the chi-squared test (optional).
#'
#' @return A data frame with the same structure as the input, but with non-significant statements removed (if any).
#'   If all statements are significant, the original data frame is returned unchanged.
#'
#' @seealso \code{\link{chisq.test}} for chi-squared test functionality.
#'
#' @importFrom stats chisq.test
#'
#' @examples
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.70, attributeWeights=c(1,1,1,1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Select significant statements
#' mySelectedStatementsSorterCluster3 <- selectStatements(myCMDataBySorters[[1]])
#'
#' @export
selectStatements <- function(CMData, significanceThreshold = 0.05, verbose = TRUE, ...) {

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  #add column StatementNumber for each unique statement
  CMData$StatementNumber <- as.numeric(factor(CMData$statement, levels=unique(CMData$statement)))

  #create overview of all statements
  allStatements <- createStatementOverview(CMData)

  #create co-occurence matrix for all statements
  coOccurrenceMatrix <- createCoOccurrenceMatrix(CMData)

  if (verbose) {
    cat(sprintf("Testing statements for uniformity.\n"))
  }

  pVals <- matrix(NA, nrow=nrow(allStatements), ncol=1)
  rownames(pVals) <- allStatements[,"Statement"]
  colnames(pVals) <- "p-value"

  for (i in 1:nrow(pVals)) {
    myTest <- chisq.test(coOccurrenceMatrix[i, -i])
    #myTest <- chisq.test(coOccurrenceMatrix[i, -i], ...)
    pVals[i] <- myTest$p.value
  }

  addSignificance <- function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return(" ")
    }
  }

  #output results of tests
  significanceInfo <- sapply(pVals, addSignificance)

  if (verbose) {
    cat(sprintf("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n"))
    cat(sprintf("                      ID  P-value\n"))

    for (i in 1:length(pVals)) {
      cat(sprintf("%2d. %20s  %.3e%s\n", allStatements$StatementNumber[i], allStatements$Statement[i], pVals[i], significanceInfo[i]))
    }
  }

  significant <- sum(pVals < significanceThreshold)
  nonSignificant <- sum(pVals >= significanceThreshold)

  # Display result
  if (verbose) {
    cat(sprintf("Significant statements in the sorter cluster: %d.\n", significant))
    cat(sprintf("Non-significant statements in the sorter cluster: %d.\n\n", nonSignificant))
  }

  if (nonSignificant > 0) {
    if (verbose) cat(sprintf("Overview of non-significant statements:\n"))
    idx <- which(pVals >= significanceThreshold)

    if (verbose) {
      for (jj in idx) {
        cat(sprintf("    %2d - %20s %.3e\n", jj, allStatements[jj, "Statement"], pVals[jj]))
      }
    }

    #remove the non-significant statements and return
    if (verbose) cat(sprintf("Removing %d non-significant statements from the data.\n", nonSignificant))
    idx <- which(CMData$statement %in% allStatements[idx, "Statement"])
    return(CMData[-idx,])
  } else {
    if (verbose) cat(sprintf("There are no non-significant statements to remove from the data.\n"))
  }

  return(CMData)
}
