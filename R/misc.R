#' Print a progress bar while iterating over something
#' @param progress An integer denoting how far we have come
#' @param size The total size of the progress bar
#' @return progress + 1, to update the calling function
#' @export
progressbar <- function (progress, size=40) {
  cat("\r", "|")
  for (p in 1:size-1) {
    if (progress >= p) cat("=")
    else cat(" ")
  }
  cat("|")
  return(progress+1)
}
