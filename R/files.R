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

#' Load the contents of an RData file that is stored in a Base64 string
#' @param .base64 The Base64 string containing the RData file
#' @return A list containing the contents of the file in .base64
#' @export
loadBase64RData <- function (.base64) {
  # Decode the base64 to a raw vector
  .base64_raw <- RCurl::base64Decode(.base64, mode="raw")
  # Load the data through a raw connection
  load(rawConnection(.base64_raw))
  closeAllConnections()

  # Clean up the environment and return the loaded data as a list
  rm(.base64)
  rm(.base64_raw)
  retval <- as.list(environment())
  retval[!sapply(retval, is.function)]
}
