#' Get the first day of a month given a date.
#' @param x The date to get the first day of the month of.
#' @return A Date which is the first day of the month of x.
#' @export
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

#' Get the last day of a month given a date.
#' @param x The date to get the last day of the month of.
#' @return A Date which is the last day of the month of x.
#' @export
monthEnd <- function (x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  x$mon <- x$mon + 1
  as.Date(x) - 1
}

#' Get the first day of a year given a date.
#' @param x The date to get the first day of the year of.
#' @return A Date which is the first day of the year of x.
#' @export
yearStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  x$mon <- 1
  as.Date(x)
}

#' Get the last day of a year given a date.
#' @param x The date to get the last day of the year of.
#' @return A Date which is the last day of the year of x.
#' @export
yearEnd <- function (x) {
  x <- as.POSIXlt(x)
  x$mday <- 31
  x$mon <- 11
  as.Date(x)
}

#' Get the first day of a quarter given a date.
#' @param x The date to get the first day of the quarter of.
#' @return A Date which is the first day of the quarter of x.
#' @export
quarterStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  x$mon <- floor((x$mon + 2) / 3)
  as.Date(x)
}

#' Get the last day of a quarter given a date.
#' @param x The date to get the last day of the quarter of.
#' @return A Date which is the last day of the v of x.
#' @export
quarterEnd <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  x$mon <- floor((x$mon + 2) / 3) + 3
  as.Date(x) - 1
}

#' Get the first day of a week given a date.
#' @param x The date to get the first day of the week of.
#' @param sunday Should weeks start on sunday, default = TRUE, otherwise start end on sunday.
#' @return A Date which is the first day of the week of x.
#' @export
weekStart <- function(x, sunday = TRUE) {
  x <- as.POSIXlt(x)
  if (sunday)
    as.Date(x) - x$wday
  else
    as.Date(x) - x$wday + 1
}

#' Get the last day of a week given a date.
#' @param x The date to get the last day of the week of.
#' @param saturday Should weeks end on saturday, default = TRUE, otherwise weeks end on sunday.
#' @return A Date which is the last day of the week of x.
#' @export
weekEnd <- function(x, saturday = TRUE) {
  x <- as.POSIXlt(x)
  if (saturday)
    as.Date(x) + 6 - x$wday
  else
    as.Date(x) + 7 - x$wday
}
