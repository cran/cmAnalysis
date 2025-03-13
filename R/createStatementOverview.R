#' Create an Overview of Statements in Concept Map Data
#'
#' This function generates an overview of all unique statements in a given concept mapping dataset.
#' The function assigns a unique statement number to each distinct statement and returns a summary data frame.
#'
#' @param CMData A data frame containing concept map data. This must include a column named \code{"statement"}.
#'
#' @return A data frame with two columns: \code{StatementNumber} and \code{Statement}.
#'         Each row represents a unique statement with its corresponding number.
#'
#' @details
#' The function first checks if the provided dataset is suitable for concept mapping using the \cr
#' \code{checkConceptMapData} function. If the data is valid, it assigns a numeric statement number
#' to each distinct statement, ordered by their appearance in the dataset. The function then generates
#' an overview where each statement is paired with its corresponding statement number.
#'
#' @examples
#' # Example of valid data
#' CMData <- data.frame(
#'     sorterID = c("resp1", "resp1", "resp1", "resp2",
#'      "resp2", "resp2", "resp3", "resp3", "resp3"),
#'    statement = c("London", "Frankfurt", "Berlin", "London",
#'     "Frankfurt", "Berlin", "London", "Frankfurt", "Berlin"),
#'     stackID = c("capital city", "city", "capital city", 1, 2, 2, "A", "B", "A")
#' )
#'
#' # Create an overview of the statements
#' createStatementOverview(CMData)
#'
#' @export
createStatementOverview <- function(CMData) {

  #check if the data frame is suitable for concept mapping
  if (!checkConceptMapData(CMData)) {
    stop("Object CMdata is not suitable for concept mapping.")
  }

  CMData$StatementNumber <- as.numeric(factor(CMData$statement, levels=unique(CMData$statement)))

  #create overview of all statements
  statementOverview <- NULL
  for (i in 1:max(CMData$StatementNumber, na.rm=TRUE)) {
    idx <- which(CMData$StatementNumber == i)
    statementOverview <- rbind(statementOverview, data.frame(StatementNumber=i, Statement=CMData[idx, "statement"][1]))
  }

  return(statementOverview)
}
