#' Plot many columns of data in a matrix in the same plot.
#' @param mat The matrix containing the data to be plotted.
#' @param logscale Should the data be log transformed before plotting.
#' @param ylim The limits of the y axis of the plot, default to the min and max of mat.
#' @param legend Should a legend be appended to the plot? Defaults to false.
#' @param legend.pos If a legend is used, where should it be positioned, defaults to bottomleft.
#' @param names The names to use in the legend, defaults to the names of the matrix mat.
#' @param ... Additional arguments to pass to plot.
#'
#' @importFrom grDevices rainbow
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
#' @param ci.col The confidence area color, defaults to lightgrey
#' @param ci.density The confidence area density, defaults to 100
#' @param ci.angle The confidence area angle, defaults to 0
#' @param ci.border The confidence area border, defaults to NA
#' @param mean Should the mean be plotted, default is TRUE
#' @param mean.lty Line type of mean, default is solid
#' @param mean.col Line color of mean, default is black
#' @param median Should median be plotted, default is FALSE
#' @param median.lty Line type of median, default is solid
#' @param median.col Line color of median, default is blue
#' @param ... Additional arguments to pass to the plot function.
#'
#' @importFrom graphics lines polygon
#'
#' @export
ciPlot <- function(data, col=1, append=FALSE, ylim=c(min(as.matrix(data$low)[,col]), max(as.matrix(data$high)[,col])),
                   ci.col="lightgrey", ci.density=100, ci.angle=0, ci.border=NA,
                   mean=TRUE, mean.lty="solid", mean.col="black", median=FALSE, median.lty="solid", median.col="blue", ...) {
  if (!is.list(data) || is.null(data$low) || is.null(data$high) || !is.numeric(data$low) || !is.numeric(data$high)) {
    stop("Data must be a list, containing plottable data in data$low and data$high.")
  }
  if (mean && (is.null(data$mean) || !is.numeric(data$mean))) {
    stop("If a mean line should be plotted, it must be available in data$mean.")
  }
  if (median && (is.null(data$median) || !is.numeric(data$median))) {
    stop("If a median line should be plotted, it must be available in data$median.")
  }

  x_size <- nrow(as.matrix(data$low))
  if (!append) plot(-10, xlim=c(1, x_size), ylim=ylim, ...)
  polygon(c(1:x_size, x_size:1), c(as.matrix(data$low)[,col], rev(as.matrix(data$high)[,col])),
        col=ci.col, density=ci.density, angle=ci.angle, border=ci.border)
  if (mean) lines(as.matrix(data$mean)[,col], lty=mean.lty, col=mean.col)
  if (median) lines(as.matrix(data$median)[,col], lty=median.lty, col=median.col)
}
