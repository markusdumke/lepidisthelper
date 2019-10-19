#' Show toast
#'
#' Just a wrapper around shinytoastr::toastr functions
#'
#' @param .text Text to display.
#' @param .which One of "error", "success" or "info".
#' @param .timeout How long should the toast be displayed?
#'
#' @export
show_toast <- function(.text, .which = "error", .timeout = 5000) {
  if (.which == "error") {
    shinytoastr::toastr_error(.text,
                              position = "bottom-left",
                              preventDuplicates = TRUE,
                              timeOut = .timeout)
  }
  if (.which == "success") {
    shinytoastr::toastr_success(.text,
                                position = "bottom-left",
                                preventDuplicates = TRUE,
                                timeOut = .timeout)
  }
  if (.which == "info") {
    shinytoastr::toastr_info(.text,
                             position = "bottom-left",
                             preventDuplicates = TRUE,
                             timeOut = .timeout)
  }
}
