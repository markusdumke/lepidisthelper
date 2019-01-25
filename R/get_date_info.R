#' Get date information (year, month, mday, yday)
#'
#' @param .date (vector of dates)
#'
#' @return list with named entries with information extracted from the date, e.g.
#' year, month, day of month and year day. If a year is a leapyear all dates after
#' 29th February will have a yday minus 1, so e.g. 1.March will have the same yday
#' independent if it is leapyear or not.
#'
#' @export
#' @import data.table
#' @examples
#' get_date_info(c("2013-05-04", "2016-01-09"))
#' get_date_info(c(NA, "2016-03-09"))
#'
#' # Behaviour for leap years
#' get_date_info("2016-03-01")
#' get_date_info("2017-03-01")
get_date_info <- function(.date, .output = "list") {

  # # Ausnahmen, alle 100 Jahre nicht, aber 2000 schon
  # leapyears <- seq(1804, 2400, by = 4) %>%
  #   .[. %notin% c(1900, 2100, 2200, 2300)]

  data <- data.table(date = .date)

  # extract year, month, yday and mday from date
  data$year <- lubridate::year(.date)
  data$month <- lubridate::month(.date)
  data$day <- lubridate::day(.date)
  data$yday <- lubridate::yday(.date)
  data$leapyear <- lubridate::leap_year(data$year)

  # if it is a leap year we still want to have the same yday for 1.March as in a
  # non leap year
  data[leapyear == TRUE & month >= 3L, yday := yday - 1L]

  if (.output == "list") {
    list(year = data$year, month = data$month, day = data$day, yday = data$yday)
  } else {
    data[, .(year, month, day, yday)]
  }
}
