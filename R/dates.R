#' Get the first day of a period given a date.
#' @param x The date to get the first day of the period of.
#' @param frequency The frequency denoting the period, should be 1 (year), 4 (quarter), 12 (month) or 52 (week).
#' @param sunday Should weeks start on sunday, default = TRUE, otherwise weeks start on sunday. Only used if frequency == 52.
#' @return A Date which is the first day of the period of x.
#' @export
periodStart <- function (x, frequency, sunday = TRUE) {
  if (frequency == 1) {
    yearStart(x)
  } else if (frequency == 4) {
    quarterStart(x)
  } else if (frequency == 12) {
    monthStart(x)
  } else if (frequency == 52) {
    weekStart(x, sunday)
  } else {
    stop("Invalid frequency specified, should be 1 (year), 4 (quarter), 12 (month) or 52 (week).")
  }
}

#' Get the last day of a period given a date.
#' @param x The date to get the last day of the period of.
#' @param frequency The frequency denoting the period, should be 1 (year), 4 (quarter), 12 (month) or 52 (week).
#' @param sunday Should weeks start on sunday, default = TRUE, otherwise weeks start on sunday. Only used if frequency == 52.
#' @return A Date which is the last day of the period of x.
#' @export
periodEnd <- function (x, frequency, sunday = TRUE) {
  if (frequency == 1) {
    yearEnd(x)
  } else if (frequency == 4) {
    quarterEnd(x)
  } else if (frequency == 12) {
    monthEnd(x)
  } else if (frequency == 52) {
    weekEnd(x, sunday)
  } else {
    stop("Invalid frequency specified, should be 1 (year), 4 (quarter), 12 (month) or 52 (week).")
  }
}

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
  x$mon <- 0
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
#' @param sunday Should weeks start on sunday, default = TRUE, otherwise weeks start on monday.
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

#' Get the number of months between two dates.
#' @param start The start date.
#' @param end The end date.
#' @return An integer which is the number of (full) months between start and end.
#' @export
monthDiff <- function (start, end) {
  ed <- as.POSIXlt(start)
  sd <- as.POSIXlt(end)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#' Add a number of months to a date.
#' @param x The date to add months to.
#' @param n The number of months to add to x.
#' @return A Date which is n months after x.
#' @export
addMonths <- function (x, n) {
  x <- as.POSIXlt(x)
  x$mon <- x$mon + n
  return(as.Date(x))
}

#' Add a number of days to a date.
#' @param x The date to add days to.
#' @param n The number of days to add to x.
#' @return A Date which is n days after x.
#' @export
addDays <- function (x, n) {
  x <- as.POSIXlt(x)
  x$mday <- x$mday + n
  return(as.Date(x))
}
