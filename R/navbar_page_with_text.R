#' NavbarPage which can contain html text
#'
#' @param ... Arguments to navbarPage
#' @param text HTML text (or actionButton ...)
#'
#' @export
#' @rdname navbar_page_with_text
navbar_page_with_text <- function(..., text) {
  navbar <- shiny::navbarPage(...)
  textEl <- shiny::tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

#' @export
#' @rdname navbar_page_with_text
navbarPageWithText <- navbar_page_with_text
