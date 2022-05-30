#' Load an RData file and return the contents, i.e. dont load it into the global scope
#' @param file_name The name of the file to load
#' @return The contents of the file in file_name
#' @export
loadRdata <- function (file_name) {
  load(file_name)
  rm(file_name)
  retval <- as.list(environment())
  retval[!sapply(retval, is.function)]
}

#' Create a folder with a random name inside the current folder
#' @return The name of the new folder which is guaranteed to be unique
#' @export
createRandDir <- function () {
  dirname <- as.character(sample.int(1911, 1))
  while (dir.exists(dirname)) dirname <- as.character(sample.int(1911, 1))
  dir.create(dirname)
  return(dirname)
}
