#' Generate Concept Mapping Network with Clustering
#'
#' This helper function constructs a network from a co-occurrence matrix, applies Leiden clustering,
#' and generates a plot of the network. It optimizes the resolution to achieve the specified number of clusters.
#'
#' @param coOccurrenceMatrix A numeric matrix representing the co-occurrence data.
#' @param numberOfClusters The number of clusters to form, or "auto" to automatically determine the optimal resolution.
#' @param main A string for the title of the plot.
#' @param verbose Logical, if \code{TRUE}, additional information about the processing steps is printed to the console.
#' @param backgroundColor The background color for the plot (default is "white").
#' @param resolution A resolution parameter for Leiden clustering (optional, defaults to `NULL`).
#'
#' @return A list with:
#'   - `networkPlot`: A ggplot object of the network plot with nodes and edges.
#'   - `networkNumberOfClusters`: The number of clusters formed in the network.
#'   - `networkClusterResults`: The cluster membership for each node.
#'
#' @importFrom igraph graph_from_adjacency_matrix cluster_leiden strength gorder as_data_frame layout_with_fr V
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text theme_void element_rect element_text labs
#' @noRd
conceptMappingNetwork <- function(coOccurrenceMatrix,
                                  numberOfClusters,
                                  main,
                                  verbose = TRUE,
                                  backgroundColor = "white",
                                  resolution = NULL) {

  graphObject <- igraph::graph_from_adjacency_matrix(coOccurrenceMatrix, mode = "undirected",
                                                     weighted = TRUE, diag = FALSE)

  if (numberOfClusters=="auto") {
    if (is.null(resolution)) {
      resolution <- stats::quantile(igraph::strength(graphObject))[2] / (igraph::gorder(graphObject) - 1)
    }
  } else {
    #get resolution value that matches with numberOfClusters
    if (verbose) {
      cat(sprintf("Opimizing resolution to get %d clusters in network...", numberOfClusters))
    }
    mySeq <- seq(from=0.1, to=50, by=0.01)
    numClusts <- numeric(length(mySeq))

    for (i in 1:length(mySeq)) {
      resolution <- mySeq[i]
      clusterResult <- igraph::cluster_leiden(graphObject, resolution=resolution, n_iterations = 10)
      numClusts[i] <- length(unique(clusterResult$membership))
      if (numClusts[i] >= numberOfClusters) {
        if (verbose) {
          cat(sprintf("done.\n"))
        }
        break
      }
    }
  }

  if (verbose) {
    cat(sprintf("Using Leiden algorithm with resolution: %.2f\n", resolution))
  }

  #get estimate for clusters
  clusterResult <- igraph::cluster_leiden(graphObject, resolution=resolution, n_iterations = 10)
  names(clusterResult$membership) <- clusterResult$names

  # Extract the edges and their weights for plotting
  edgeList <- igraph::as_data_frame(graphObject, what = "edges")

  # Create a layout for the network
  layoutDF <- as.data.frame(igraph::layout_with_fr(graphObject)) # Fruchterman-Reingold layout
  colnames(layoutDF) <- c("x", "y")
  # Check the node labels in layoutDF
  layoutDF$node <- igraph::V(graphObject)$name # Get node names from the graph object

  layoutDF$ID <- colnames(coOccurrenceMatrix)

  nodeNames <- layoutDF$node
  nodeCoords <- layoutDF[, c("x", "y")]

  # Initialize empty columns in the edgeList for from and to coordinates
  edgeList$from_x <- NA
  edgeList$from_y <- NA
  edgeList$to_x <- NA
  edgeList$to_y <- NA

  # Iteratively assign the 'from' and 'to' coordinates using a for loop and which function
  for (i in 1:nrow(edgeList)) {
    # Find the index of the 'from' node in the layoutDF
    fromIndex <- which(nodeNames == edgeList$from[i])
    toIndex <- which(nodeNames == edgeList$to[i])

    # Assign the 'from' coordinates
    edgeList$from_x[i] <- nodeCoords[fromIndex, "x"]
    edgeList$from_y[i] <- nodeCoords[fromIndex, "y"]

    # Assign the 'to' coordinates
    edgeList$to_x[i] <- nodeCoords[toIndex, "x"]
    edgeList$to_y[i] <- nodeCoords[toIndex, "y"]
  }

  #draw low weight lines first so they go to the back
  edgeList <- edgeList[order(edgeList$weight), ]

  if (backgroundColor=="black") {
    pointColor <- "blue"
    paperColor <- "black"
    textColor <- "white"
  } else {
    pointColor <- "blue"
    paperColor <- "white"
    textColor <- "black"
  }

  #Most different colors
  distinctiveColors <- c(
    "red",           #1
    "blue",          #2
    "green",         #3
    "yellow",        #4
    "purple",        #5
    "orange",        #6
    "cyan",          #7
    "pink",          #8
    "brown",         #9
    "gray",          #10
    "darkgreen",     #11
    "lightblue",     #12
    "gold",          #13
    "violet",        #14
    "darkgray"       #15
  )

  #the network plot
  theCMNetworkGraph <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edgeList, ggplot2::aes(x = .data$from_x, y = .data$from_y,
                                                        xend = .data$to_x, yend = .data$to_y,
                                                        linewidth = .data$weight, color=.data$weight)) +
    ggplot2::geom_point(data = layoutDF, ggplot2::aes(x = .data$x, y = .data$y), size = 5,
                        color = distinctiveColors[clusterResult$membership]) +
    ggplot2::geom_text(data = layoutDF, ggplot2::aes(x = .data$x, y = .data$y, label = .data$ID),
                       vjust = 1.5, size = 3, color=textColor) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = paperColor, colour=paperColor),
                   panel.background = ggplot2::element_rect(fill = paperColor, colour=paperColor),
                   text = ggplot2::element_text(color = textColor),
                   legend.position = "none")

  theCMNetworkGraph <- theCMNetworkGraph + ggplot2::labs(title=paste("Network plot", main))


  return(list(networkPlot=theCMNetworkGraph,
              networkNumberOfClusters=length(unique(clusterResult$membership)),
              networkClusterResults=clusterResult$membership,
              resolution=resolution))
}

