#' Generic function for calculating quantiles and mean of an object
#' @param x The object to process
#' @param ... Additional arguments
#' @export
quantmean <- function (x, ...) {
  UseMethod("quantmean")
}

#' Function for calculating the quantiles and mean of many matrices in a list
#' @param x The list of matrices to use
#' @param quantiles The quantiles to calculate, defaults to 0.05 and 0.95 (i.e. 90\%)
#' @param mean Should the mean also be calculated? Defaults to TRUE
#' @param median Should the median also be calculated? Defaults to FALSE
#' @param ... Not used.
#' @return A list containing the mean per element over all the matrices, together with the low and high quantiles selected
#'
#' @importFrom stats quantile
#'
#' @export
quantmean.list <- function (x, quantiles=c(0.05, 0.95), mean=TRUE, median=FALSE, ...) {
  mat_all <- matrix(NA, nrow(x[[1]]) * ncol(x[[1]]), length(x))
  count <- 0
  for (i in seq_along(x)) {
    if (!is.null(x[[i]])) {
      count <- count + 1
      mat_all[,count] <- as.vector(x[[i]])
    }
  }
  mat_all <- mat_all[ , seq_len(count)]
  result <- list()
  if (mean) result$mean <- matrix(rowMeans(mat_all), nrow(x[[1]]), ncol(x[[1]]))
  if (median) result$median <- matrix(apply(mat_all, 1, median), nrow(x[[1]]), ncol(x[[1]]))
  result$low <- matrix(apply(mat_all, 1, quantile, probs = quantiles[1]), nrow(x[[1]]), ncol(x[[1]]))
  result$high <- matrix(apply(mat_all, 1, quantile, probs = quantiles[2]), nrow(x[[1]]), ncol(x[[1]]))
  for (i in seq_along(result)) colnames(result[[i]]) <- colnames(x[[1]])
  return(result)
}

#' Function for calculating the quantiles and mean of an array
#' @param x The list of matrices to use
#' @param dim The dimension to take quantiles and means over
#' @param quantiles The quantiles to calculate, defaults to 0.05 and 0.95 (i.e. 90\%)
#' @param mean Should the mean also be calculated? Defaults to TRUE
#' @param median Should the median also be calculated? Defaults to FALSE
#' @param ... Not used.
#' @return A list containing the mean per element over all the matrices, together with the low and high quantiles selected
#'
#' @importFrom stats quantile
#'
#' @export
quantmean.array <- function (x, dim=1, quantiles=c(0.05, 0.95), mean=TRUE, median=FALSE, ...) {
  # Select the dimensions to apply over, i.e. all except the one that is reduced
  dims <- seq_along(dim(x))[-dim]
  result <- list()
  result$low <- apply(x, dims, quantile, probs = quantiles[1])
  result$high <- apply(x, dims, quantile, probs = quantiles[2])
  if (mean) result$mean <- apply(x, dims, mean)
  if (median) result$median <- apply(x, dims, median)
  return(result)
}
