#' Create Co-occurrence Heatmap
#'
#' This helper function generates a heatmap for the provided co-occurrence matrix.
#' The heatmap is created using hierarchical clustering for both rows and columns.
#'
#' @param coOccurrenceMatrix A numeric matrix or data frame representing the co-occurrence data.
#' @param main A string for the title of the heatmap.
#'
#' @return A `pheatmap` object containing the heatmap plot.
#'
#' @importFrom pheatmap pheatmap
#' @importFrom stats dist
#' @noRd
createHeatmap <- function(coOccurrenceMatrix, main) {

  # Check if coOccurrenceMatrix is a data frame and convert to matrix if needed
  if (!is.matrix(coOccurrenceMatrix)) {
    if (is.data.frame(coOccurrenceMatrix)) {
      coOccurrenceMatrix <- as.matrix(coOccurrenceMatrix)
    } else {
      stop("Input must be a numeric matrix or data frame.")
    }
  }

  #create heatmap with row and column clustering
  pheatmapPlot <- pheatmap::pheatmap(
    coOccurrenceMatrix,
    color = grDevices::colorRampPalette(c("white", "red"))(50),
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    clustering_method = "complete",
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 8,
    fontsize_col = 8,
    legend = TRUE,
    silent = TRUE,
    main = paste("Co-occurrence Heatmap", main))

  return(pheatmapPlot)
}
