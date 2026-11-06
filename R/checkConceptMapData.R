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
#'   \item Checks that sorter and statement identifiers are present and non-empty.
#'   \item Allows missing or blank \code{stackID} values for unsorted items.
#'   \item Confirms that there are at least two unique \code{sorterID}/\code{stackID} combinations, excluding unsorted items.
#'   \item Confirms that there are at least 2 unique values in the \code{"statement"} column.
#' }
#'
#' Unsorted items contribute no co-occurrences and do not count as stacks.
#' A single sorter is allowed if the data meet the remaining requirements.
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

  #Validate keys; missing stackID means unsorted and is allowed.
  for (nm in requiredColumns) {
    value <- CMData[[nm]]
    simple <- is.factor(value) || is.character(value) ||
      is.integer(value) || is.double(value)

    if (!simple || !is.null(dim(value))) {
      stop(sprintf("Column %s must contain simple text or numeric keys.", nm))
    }

    missing <- is.na(value) | !nzchar(trimws(as.character(value)))

    if (nm != "stackID" && any(missing)) {
      stop(sprintf("Column %s contains missing or empty keys.", nm))
    }

    if (is.numeric(value) && any(!is.finite(value) & !is.na(value))) {
      stop(sprintf("Column %s contains non-finite keys.", nm))
    }
  }

  #Only actual stacks count; unsorted items do not form a stack.
  sorted <- !is.na(CMData$stackID) &
    nzchar(trimws(as.character(CMData$stackID)))
  if (nrow(unique(CMData[sorted, c("sorterID", "stackID")])) < 2) {
    stop("Object CMData must contain at least 2 unique stacks.")
  }

  #Check if there are at least 2 unique statements
  if (length(unique(sort(CMData$statement))) < 2) {
    stop("Data must contain at least 2 unique statements.")
  }

  return(TRUE)
}
