#' Perform Silhouette Analysis for Optimal Clustering
#'
#' This helper function computes the silhouette widths for a range of cluster numbers
#' and identifies the optimal number of clusters for clustering algorithms.
#'
#' @param myDf A numeric data frame or matrix for clustering.
#' @param rangeNumberOfClusters A vector specifying the number of clusters to evaluate (default is `2:15`).
#' @param numberOfKmeansRestarts Number of k-means restarts (default is 100).
#' @param main A string for the title of the silhouette plot.
#' @param forcedClusterNumber Optional fixed number of clusters to use (default is `NULL`).
#'
#' @return A list containing:
#'   - `silhouettePlot`: A ggplot object for the silhouette plot.
#'   - `numberOfClusters`: The optimal number of clusters.
#'   - `clusterResults`: The cluster assignments for each data point.
#'
#' @importFrom cluster silhouette
#' @importFrom ggplot2 ggplot aes geom_line geom_point annotate labs theme_minimal
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
doSilhouette <- function(myDf, rangeNumberOfClusters = 2:15,
                         numberOfKmeansRestarts = 100, main,
                         forcedClusterNumber = NULL) {
  x <- as.matrix(myDf)
  if (!is.numeric(x) || length(dim(x)) != 2 ||
        nrow(x) < 2 || ncol(x) < 1 || any(!is.finite(x))) {
    stop("Features must be a finite numeric matrix with at least two rows.")
  }
  whole <- function(z) is.numeric(z) && length(z) > 0 &&
    all(is.finite(z)) && all(z == floor(z))
  if (!whole(numberOfKmeansRestarts) ||
        length(numberOfKmeansRestarts) != 1 || numberOfKmeansRestarts < 1) {
    stop("numberOfKmeansRestarts must be one positive integer.")
  }
  n <- nrow(x)
  distinct <- nrow(unique(as.data.frame(x)))
  # stats::kmeans with randomly selected initial centres needs k < n.
  maxK <- min(n - 1, distinct)
  if (!is.null(forcedClusterNumber)) {
    if (!whole(forcedClusterNumber) || length(forcedClusterNumber) != 1 ||
          forcedClusterNumber < 2 || forcedClusterNumber > maxK) {
      stop(sprintf("Fixed k must be between 2 and %d for these features.", maxK))
    }
    ks <- forcedClusterNumber
  } else {
    if (!whole(rangeNumberOfClusters)) {
      stop("Automatic cluster range must contain finite integers.")
    }
    requested <- sort(unique(rangeNumberOfClusters))
    ks <- requested[requested >= 2 & requested <= maxK]
    if (!length(ks)) {
      stop("No feasible automatic silhouette k: need 2 <= k < n and k <= distinct profiles.")
    }
    if (length(ks) != length(requested)) {
      warning("Infeasible cluster candidates were omitted.", call. = FALSE)
    }
  }
  d <- stats::dist(x)
  fits <- lapply(ks, function(k) {
    fit <- stats::kmeans(x, centers = k, nstart = numberOfKmeansRestarts,
                         iter.max = 2000)
    if (!is.null(fit$ifault) && fit$ifault != 0) {
      stop(sprintf("K-means did not converge for k=%d.", k))
    }
    fit
  })
  widths <- vapply(fits, function(fit) {
    mean(cluster::silhouette(fit$cluster, d)[, "sil_width"])
  }, numeric(1))
  if (any(!is.finite(widths))) stop("Non-finite silhouette score.")
  chosen <- if (is.null(forcedClusterNumber)) which.max(widths) else 1
  df <- data.frame(NumberOfClusters = ks, SilhouetteWidth = widths)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$NumberOfClusters,
                                        y = .data$SilhouetteWidth))
  if (length(ks) > 1) p <- p + ggplot2::geom_line()
  p <- p + ggplot2::geom_point() +
    ggplot2::annotate("point", x = ks[chosen], y = widths[chosen],
                      colour = "red", size = 3, shape = 15) +
    ggplot2::labs(title = paste("Silhouette plot", main),
                  x = "Number of clusters", y = "Average silhouette width") +
    ggplot2::theme_minimal()
  list(silhouettePlot = p, numberOfClusters = ks[chosen],
       clusterResults = fits[[chosen]]$cluster, model = fits[[chosen]])
}