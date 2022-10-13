#' Generic function for calculating quantiles and mean of an object
#' @param x The object to process
#' @param ... Additional arguments
#' @export
quantmean <- function (x, ...) {
  UseMethod("quantmean")
}

#' Function for calculating the quantiles and mean of many matrices in a list
#' @param matlist The list of matrices to use
#' @param quantiles The quantiles to calculate, defaults to 0.05 and 0.95 (i.e. 90\%)
#' @param mean Should the mean also be calculated? Defaults to TRUE
#' @return A list containing the mean per element over all the matrices, together with the low and high quantiles selected
#' @export
quantmean.list <- function (matlist, quantiles=c(0.05, 0.95), mean=TRUE) {
  mat_all <- matrix(NA, nrow(matlist[[1]]) * ncol(matlist[[1]]), length(matlist))
  count <- 0
  for (i in seq_along(matlist)) {
    if (!is.null(matlist[[i]])) {
      count <- count + 1
      mat_all[,count] <- as.vector(matlist[[i]])
    }
  }
  mat_all <- mat_all[ , seq_len(count)]
  result <- list()
  if (mean) result$mean <- matrix(rowMeans(mat_all), nrow(matlist[[1]]), ncol(matlist[[1]]))
  result$low <- matrix(apply(mat_all, 1, quantile, probs = quantiles[1]), nrow(matlist[[1]]), ncol(matlist[[1]]))
  result$high <- matrix(apply(mat_all, 1, quantile, probs = quantiles[2]), nrow(matlist[[1]]), ncol(matlist[[1]]))
  return(result)
}

#' Function for calculating the quantiles and mean of an array
#' @param array The list of matrices to use
#' @param dim The dimension to take quantiles and means over
#' @param quantiles The quantiles to calculate, defaults to 0.05 and 0.95 (i.e. 90\%)
#' @param mean Should the mean also be calculated? Defaults to TRUE
#' @return A list containing the mean per element over all the matrices, together with the low and high quantiles selected
#' @export
quantmean.array <- function (array, dim=1, quantiles=c(0.05, 0.95), mean=TRUE) {
  # Select the dimensions to apply over, i.e. all except the one that is reduced
  dims <- seq_along(dim(array))[-dim]
  result <- list()
  result$low <- apply(array, dims, quantile, probs = quantiles[1])
  result$high <- apply(array, dims, quantile, probs = quantiles[2])
  if (mean) result$mean <- apply(array, dims, mean)
  return(result)
}
