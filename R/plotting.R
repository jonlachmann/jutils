#' Plot many columns of data in a matrix in the same plot
#' @param mat The matrix containing the data to be plotted
#' @param logscale Should the data be log transformed before plotting
#' @param ylim The limits of the y axis of the plot, default to the min and max of mat
#' @param legend Should a legend be appended to the plot? Defaults to false
#' @param names The names to use in the legend, defaults to the names of the matrix mat
#' @export
multiplot <- function (mat, logscale=F, ylim=c(min(mat), max(mat)), legend=F, legend.pos="bottomleft", names=names(mat), ...) {
  if (logscale) {
    mat[mat > 0] <- log(mat[mat > 0])
    mat[mat < 0] <- -log(-mat[mat < 0])
  }
  mat <- as.matrix(mat)
  rbcol <- rainbow(ncol(mat))
  plot(mat[,1], type="l", ylim=ylim, col=rbcol[1], ...)
  if (ncol(mat) > 1) for (i in 2:ncol(mat)) lines(mat[,i], col=rbcol[i])
  if (legend) {
    if (is.null(names)) names <- seq_len(ncol(mat))
    legend(legend.pos, col=rbcol, legend=names, lty=1)
  }
}

#' Create a confidence interval plot
#' @param data The data to plot, should be a list that contains "mean", "low" and "high" matrices
#' @param col Which column of each matrix contains the data to plot, defaults to 1
#' @param append Should we create a new plot or append a confidence interval to one already active, defaults to FALSE
#' @param ylim The y axis limits for the plot, defaults to min of low and max of high
#' @param ci_col The confidence area color, defaults to lightgrey
#' @param ... Additional arguments to pass to the plot function.
#' @export
ciPlot <- function(data, col=1, append=FALSE, ylim=c(min(as.matrix(data$low)[,col]), max(as.matrix(data$high)[,col])), ci_col="lightgrey", ...) {
  x_size <- nrow(as.matrix(data$mean))
  if (!append) plot(-10, xlim=c(1, x_size), ylim=ylim, ...)
  polygon(c(1:x_size, x_size:1), c(as.matrix(data$low)[,col], rev(as.matrix(data$high)[,col])),
        col=ci_col, border=NA)
  lines(as.matrix(data$mean)[,col])
}
