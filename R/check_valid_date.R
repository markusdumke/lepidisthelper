#' Check date for validity
#'
#' @param .year Integer value.
#' @param .month Integer value.
#' @param .day Integer value.
#'
#' @return Returns NULL if date is ok, else a string with the error message.
#' @export
#'
#' @examples
#' # errors
#' check_valid_date(.year = NA)
#'
#' date.future <- Sys.Date() + 1L
#' check_valid_date(.year = lubridate::year(date.future),
#'                  .month = lubridate::month(date.future),
#'                  .day = lubridate::day(date.future))
#'
#' check_valid_date(.year = lubridate::year(date.future) + 1L)
#' check_valid_date(.year = lubridate::year(date.future),
#'                  .month = lubridate::month(date.future) + 1L)
#'
#' check_valid_date(.year = 2016, .month = NA, .day = 10)
#' check_valid_date(.year = 2018, .month = 13)
#' check_valid_date(.year = 2018, .month = 12, .day = 32)
#'
#' # ok
#' check_valid_date(.year = 2019, .month = 1, .day = 10)
#' check_valid_date(.year = 2019, .month = 10, .day = NA)
#' check_valid_date(.year = 2019, .month = NA, .day = NA)

check_valid_date <- function(.year = NULL, .month = NULL, .day = NULL) {
  error <- NULL

  .year <- as.integer(.year)
  .month <- as.integer(.month)
  .day <- as.integer(.day)

  if (is_empty_or_na(.year)) {
    return("Jahr fehlt!")
  }

  if (is_empty_or_na(.month) && !is_empty_or_na(.day)) {
    return("Monat muss spezifiziert werden, wenn Tag spezifiziert ist!")
  }

  if (.year <= 1700) {
    return("Jahr darf nicht vor 1700 sein!")
  }

  if (!is_empty_or_na(.month) && .month %notin% 1:12) {
    return("Ungültige Eingabe für Monat!")
  }

  if (!is_empty_or_na(.day) && .day %notin% 1:31) {
    return("Ungültige Eingabe für Tag!")
  }

  today <- Sys.Date()
  today.year <- lubridate::year(today)
  today.month <- lubridate::month(today)
  today.day <- lubridate::day(today)

  if (.year > today.year) {
    return("Datum liegt in der Zukunft!")
  }

  if (!is_empty_or_na(.month) &&
      (.year == today.year && .month > today.month)) {
    return("Datum liegt in der Zukunft!")
  }

  if (!is_empty_or_na(.day) && !is_empty_or_na(.month) &&
      (.year == today.year &&
       .month == today.month &&
       .day > today.day)) {
    return("Datum liegt in der Zukunft!")
  }

  error
}
