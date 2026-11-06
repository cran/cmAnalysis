#' Select Significant Statements from Concept Map Data
#'
#' This function selects statements whose co-occurrence counts differ significantly from the expected 
#' distribution in a chi-squared goodness-of-fit test. By default, the test compares the counts
#' against a uniform distribution across the other statements.
#' 
#' @param CMData A data frame containing concept map data. The data should have at least the following columns:
#'   - `statement`: The text of the statement.
#'   - `sorterID`: The identifier for the sorter.
#'   - `stackID`: The identifier for the stack.
#' @param significanceThreshold One finite numeric value between 0 and 1, specifying the significance threshold for the chi-squared test.
#'   Statements with p-values less than this threshold are considered significant. Default is 0.05.
#' @param verbose Logical, if \code{TRUE}, additional information about the processing steps is printed to the console.
#' @param ... Additional arguments passed to \code{stats::chisq.test()}, such as \code{p} to specify expected probabilities or
#'   \code{simulate.p.value} to use Monte Carlo simulation.
#'
#' @return A data frame with the same structure as the input, but with non-significant statements removed (if any).
#'   If all statements are significant, the original data frame is returned unchanged.
#'   Untestable statements are retained with a warning.
#'   The selectionDiagnostics attribute records each statement's test status, p-value and whether it was retained.
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

  if (!is.numeric(significanceThreshold) || length(significanceThreshold) != 1 || 
        !is.finite(significanceThreshold) || significanceThreshold <= 0 ||
        significanceThreshold >= 1) {
    stop("significanceThreshold must be a finite number between 0 and 1.")
  }

  originalData <- CMData
  #add column StatementNumber for each unique statement
  CMData$StatementNumber <- as.numeric(factor(CMData$statement, levels=unique(CMData$statement)))

  #create overview of all statements
  allStatements <- createStatementOverview(CMData)

  #create co-occurence matrix for all statements
  coOccurrenceMatrix <- createCoOccurrenceMatrix(CMData)

  if (verbose) {
    cat(sprintf("Testing statements for uniformity.\n"))
  }

  statementKeys <- rownames(coOccurrenceMatrix)
  if (is.null(statementKeys)) {
    stop("Co-occurrence matrix needs statement row names.")
  }

  diagnostics <- data.frame(statement = statementKeys, status = "tested",
                            p = NA_real_, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(coOccurrenceMatrix))) {
    counts <- coOccurrenceMatrix[i, -i]

    if (length(counts) < 2) {
      diagnostics$status[i] <- "too_few_categories"
    } else if (sum(counts) == 0) {
      diagnostics$status[i] <- "no_cooccurrences"
    } else {
      test <- stats::chisq.test(counts, ...)
      diagnostics$p[i] <- test$p.value

      if (!is.finite(test$p.value)) {
        diagnostics$status[i] <- "nonfinite_p"
      }
    }
  }

  untestable <- diagnostics$status != "tested"
  diagnostics$retained <- untestable | (is.finite(diagnostics$p) & diagnostics$p < significanceThreshold)

  if (any(untestable)) {
    warning("Untestable statements were retained, see selectionDiagnostics.", call. = FALSE)
  }

  if (verbose) {
    print(diagnostics, row.names = FALSE)
  }

  keep <- diagnostics$statement[diagnostics$retained]
  result <- originalData[as.character(originalData$statement) %in% keep, , drop = FALSE]
  attr(result, "selectionDiagnostics") <- diagnostics

  return(result)
}
