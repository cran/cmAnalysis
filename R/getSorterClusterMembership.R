#' Extract sorter cluster membership from sorterMapping output
#'
#' Extracts the sorter cluster membership from the output returned by
#' [sorterMapping()]. The function does not perform a new cluster analysis.
#' Instead, it uses the sorter clusters already present in the `sorterMapping()`
#' result and returns a compact data frame with one row per sorter.
#'
#' @param sorterMappingResult An object returned by [sorterMapping()]. This is
#'   typically a list of data frames, where each list element contains the data
#'   for one sorter cluster. If `sorterMapping()` returns a single data frame,
#'   this is interpreted as one sorter cluster. Each data frame must contain a
#'   column named `sorterID`.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{sorterID}{The sorter identifier.}
#'   \item{sorterCluster}{The sorter cluster number assigned by [sorterMapping()].}
#' }
#'
#' @details
#' This function is intended as a small extractor/helper function. It should be
#' used after [sorterMapping()] has been run. The function checks whether each 
#' sorter occurs in only one sorter cluster. If the same sorter occurs in more
#' than one sorter cluster, an error is returned.
#'
#' @seealso [sorterMapping()]
#'
#' @export
getSorterClusterMembership <- function(sorterMappingResult) {

  if (is.data.frame(sorterMappingResult)) {
    if (!("sorterID" %in% names(sorterMappingResult))) {
      stop("sorterMappingResult does not contain a sorterID column.")
    }

    sorterIDs <- unique(sort(sorterMappingResult$sorterID))

    sorterMembership <- data.frame(
      sorterID = sorterIDs,
      sorterCluster = rep(1, length(sorterIDs)),
      stringsAsFactors = FALSE
    )

    rownames(sorterMembership) <- NULL
    return(sorterMembership)
  }

  if (!is.list(sorterMappingResult) || length(sorterMappingResult) == 0) {
    stop("sorterMappingResult must be the object returned by sorterMapping().")
  }

  sorterMembershipList <- vector("list", length(sorterMappingResult))

  for (i in seq_along(sorterMappingResult)) {
    clusterData <- sorterMappingResult[[i]]

    if (!is.data.frame(clusterData)) {
      stop("Each element of sorterMappingResult must be a data frame.")
    }

    if (!("sorterID" %in% names(clusterData))) {
      stop("Each element of sorterMappingResult must contain a sorterID column.")
    }

    sorterIDs <- unique(sort(clusterData$sorterID))

    sorterMembershipList[[i]] <- data.frame(
      sorterID = sorterIDs,
      sorterCluster = rep(as.integer(i), length(sorterIDs)),
      stringsAsFactors = FALSE
    )
  }

  sorterMembership <- do.call(rbind, sorterMembershipList)
  rownames(sorterMembership) <- NULL

  duplicatedSorters <- sorterMembership$sorterID[
    duplicated(sorterMembership$sorterID)
  ]

  if (length(duplicatedSorters) > 0) {
    stop(paste0(
      "Some sorters occur in more than one sorter cluster: ",
      paste(unique(duplicatedSorters), collapse = ", "), "."
    ))
  }

  return(sorterMembership)
}
