#' Aggregate dates in intervals
#'
#' For each month all days inside the interval will be aggregated, e.g. for an
#' interval length of five 1st January till 5th January will all belong to the first interval,
#' 6th to 10th January will belong to interval 2 and so on.
#' Note that the last interval has a varying number of days,
#' e.g. 26th July to 31th July, but only 26th April to 30th April.
#'
#' @param .date (vector of class Date) Dates to be aggregated.
#' @param .interval (integer value) The length of the interval (currently 5, 10, 15 supported).
#'
#' @return integer vector. For pentades a number between 1 and 72, e.g. "2016-02-16" will be 10.
#' @export
#'
#' @examples
#' aggregate_days(c("2017-02-16"), .interval = 10) # 5
#' aggregate_days(c("2016-01-31")) # 6
#' aggregate_days(c(NA, "2014-07-28")) # 42
aggregate_days <- function(.date, .interval = 5L) {

  n_intervals_month <- 30L / .interval
  which_interval <- vapply(.date, date_in_interval,
    .interval = .interval, FUN.VALUE = 1L)

  which_interval[which_interval > n_intervals_month] <- n_intervals_month
  (lubridate::month(.date) - 1L) * n_intervals_month + which_interval
}


# which_interval("2012-08-24", interval = 5)
# which_interval(NA, interval = 5)
date_in_interval <- function(.date, .interval) {
  if (!is.na(.date)) {
    which.max(lubridate::day(.date) <= .interval * 1L:(30L / .interval + 1L))
  } else {
    NA
  }
}
