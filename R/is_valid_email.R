#' Check if an email address is valid.
#'
#' @param .x character
#' @export
#'
is_valid_email <- function(.x) {
  stringr::str_detect(stringr::str_to_lower(.x), "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}")
}

#' Check if password is valid.
#'
#' @param .x character
#' @export
#'
is_valid_password <- function(.x) {
  stringr::str_length(.x) >= 6
}
