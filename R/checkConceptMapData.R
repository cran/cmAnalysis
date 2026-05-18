#' Validate Concept Map Data
#'
#' Checks whether the provided data frame meets the requirements for concept map data.
#'
#' @param CMData A data frame containing concept map data. This data frame must include
#' the required columns: "sorterID", "statement", and "stackID".
#'
#' @return Returns \code{TRUE} if all checks pass. If any check fails, an error is raised
#' with a descriptive message.
#'
#' @details
#' This function performs the following checks on the input data:
#' \itemize{
#'   \item Verifies that \code{CMData} is a data frame.
#'   \item Ensures the presence of required columns: \code{"sorterID"}, \code{"statement"}, and \code{"stackID"}.
#'   \item Confirms that there are at least 2 unique values in the \code{"stackID"} column.
#'   \item Confirms that there are at least 2 unique values in the \code{"sorterID"} column.
#'   \item Confirms that there are at least 2 unique values in the \code{"statement"} column.
#' }
#'
#' @examples
#' # Example of valid data
#' validData <- data.frame(
#'   sorterID = c("resp1", "resp1", "resp1", "resp2",
#'    "resp2", "resp2", "resp3", "resp3", "resp3"),
#'   statement = c("London", "Frankfurt", "Berlin", "London",
#'   "Frankfurt", "Berlin", "London", "Frankfurt", "Berlin"),
#'   stackID = c("capital city", "city", "capital city", 1, 2, 2, "A", "B", "A")
#' )
#' checkConceptMapData(validData) # Should return TRUE
#'
#' # Example of invalid data (missing columns)
#' invalidData <- data.frame(
#'   sorterID = c(1, 2),
#'   stackID = c(1, 2)
#' )
#' # checkConceptMapData(invalidData) # Will return False as an error
#'
#' @export
checkConceptMapData <- function(CMData) {
  #Check if CMData is a data frame
  if (!is.data.frame(CMData)) {
    stop("Object CMData must be a data frame.")
  }

  #Check if the required columns are present
  requiredColumns <- c("sorterID", "statement", "stackID")
  missingColumns <- setdiff(requiredColumns, colnames(CMData))

  if (length(missingColumns) > 0) {
    stop(paste(
      "The following required columns are missing from Object CMData:",
      paste(missingColumns, collapse=", ")
    ))
  }

  #Check if there are at least 2 unique stacks (sorterID, stackID combinations)
  if (nrow(unique(CMData[, c("sorterID", "stackID")])) < 2) {
    stop("Object CMData must contain at least 2 unique stacks.")
  }

  #Check if there are at least 2 unique sorterID's
  if (length(unique(CMData$sorterID)) < 2) {
    stop("Data must contain at least 2 unique users.")
  }

  #Check if there are at least 2 unique statements
  if (length(unique(sort(CMData$statement))) < 2) {
    stop("Data must contain at least 2 unique statements.")
  }

  return(TRUE)
}
