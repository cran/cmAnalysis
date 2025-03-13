#' Plot a Concept Map
#'
#' This function generates visualizations for an object of class \code{conceptMap}.
#' Depending on the method used to create the concept map, different types of plots
#' (e.g., heatmap, silhouette, network, or CMDS plots) are available.
#'
#' @param x An object of class \code{conceptMap}. This object must contain
#'   plotting data and attributes specific to the method used for creating the concept map.
#' @param whichPlot A character string specifying which plot to display. Options depend on the
#'   method used to create the concept map:
#'   \describe{
#'     \item{\code{"all"}}{Displays all available plots for the given method.}
#'     \item{\code{"heatmap"}}{Displays the heatmap plot (if available).}
#'     \item{\code{"silhouette"}}{Displays the silhouette plot (if available).}
#'     \item{\code{"network"}}{Displays the network plot (if available).}
#'     \item{\code{"cmds"}}{Displays the CMDs plot (if available).}
#'   }
#'   The default is \code{"all"}.
#' @param ... arguments to be passed to methods
#'
#' @details The function behaves differently depending on the method used to create
#'   the concept map. The \code{conceptMap} object must include attributes such as
#'   \code{method} (e.g., "kmeans", "network", or "cmds") and the corresponding plot
#'   objects (e.g., \code{heatmapPlot}, \code{silhouettePlot}, etc.).
#'
#'   The following methods are supported:
#'   \itemize{
#'     \item \code{"kmeans"}: Supports \code{"heatmap"} and \code{"silhouette"} plots.
#'     \item \code{"network"}: Supports \code{"heatmap"} and \code{"network"} plots.
#'     \item \code{"cmds"}: Supports \code{"heatmap"}, \code{"silhouette"}, and \code{"cmds"} plots.
#'   }
#'
#' @return This function displays the specified plot(s) in the current graphical device.
#'
#' @examples
#' # Simulate data with custom parameters:
#' set.seed(1)
#' myCMData <- simulateCardData(nSorters=40, pCorrect=.90, attributeWeights=c(1,1,1,1))
#'
#' # Subject the data to sorter cluster analysis
#' myCMDataBySorters <- sorterMapping(myCMData)
#'
#' # Subject sorter cluster 3 to concept mapping using default "network" method
#' myCMAnalysis3 <- conceptMapping(myCMDataBySorters[[3]])
#'
#' # Visualise the concept map
#' plot(myCMAnalysis3)
#'
#' @export
plot.conceptMap <- function(x, whichPlot = "all", ...) {

  if (!inherits(x, "conceptMap")) {
    stop("Object x should be of class conceptMap")
  }

  if (x$method=="kmeans") {
    if (!whichPlot %in% c("all", "heatmap", "silhouette")) {
      stop(sprintf("Plot ID \"%s\" for method \"%s\" is not valid.\n", whichPlot, x$method))
    }
    if (whichPlot=="all") {
      plot(x$heatmapPlot$gtable)
      print(x$silhouettePlot)
    }
    if (whichPlot=="heatmap") {
      plot(x$heatmapPlot$gtable)
    }
    if (whichPlot=="silhouette") {
      print(x$silhouettePlot)
    }
  }

  if (x$method=="network") {
    if (!whichPlot %in% c("all", "heatmap", "network")) {
      stop(sprintf("Plot ID \"%s\" for method \"%s\" is not valid.\n", whichPlot, x$method))
    }
    if (whichPlot=="all") {
      plot(x$heatmapPlot$gtable)
      print(x$networkPlot)
    }
    if (whichPlot=="heatmap") {
      plot(x$heatmapPlot$gtable)
    }
    if (whichPlot=="network") {
      print(x$networkPlot)
    }
  }

  if (x$method=="cmds") {
    if (!whichPlot %in% c("all", "silhouette", "heatmap", "cmds")) {
      stop(sprintf("Plot ID \"%s\" for method \"%s\" is not valid.\n", whichPlot, x$method))
    }
    if (whichPlot=="all") {
      print(x$silhouettePlot)
      plot(x$heatmapPlot$gtable)
      print(x$cmdsPlot)
    }
    if (whichPlot=="silhouette") {
      print(x$silhouettePlot)
    }
    if (whichPlot=="heatmap") {
      plot(x$heatmapPlot$gtable)
    }
    if (whichPlot=="cmds") {
      print(x$cmdsPlot)
    }
  }
}
