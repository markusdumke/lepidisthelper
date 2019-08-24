#' Make popup of last sightings
#'
#' @param .Data (data.table)
#' @param .tk25.name (Character vector)\cr Names of TK25 to show in popup.
#' @param .n (Integer value) \cr How many observations to includde in popup.
#'
#' @return A character value.
#'
#' @export
#' @importFrom utils head
#' @importFrom magrittr "%>%"
#'
#' @examples
#' library(data.table)
#' Data <- data.table(Beobachter = c("A", "A", "B", "A", "C"),
#'                    Jahr = c(2018, 2018, 2018, 2016, 2016))
#' make_popup_sightings(Data, .tk25.name = "8039 Starnberg")
make_popup_sightings <- function(.Data,
                                 .tk25.name = NULL,
                                 .n = 10) {

  checkmate::assert_data_table(.Data)
  checkmate::assert_count(.n)
  stopifnot(all(c("Beobachter", "Jahr") %in% names(.Data)))

  # Get last year for each Beobachter
  Data <-
    get_latest_sighting_per_observer(.Data) %>%
    head(.n)

  stringr::str_c(Data[, Jahr],
                 " - ",
                 Data[, Beobachter],
                 collapse = "<br>") %>%
    stringr::str_c("<b>", .tk25.name, "</b></br>", .)
}


get_latest_sighting_per_observer <- function(.Data) {

  checkmate::assert_data_table(.Data)
  stopifnot(all(c("Beobachter", "Jahr") %in% names(.Data)))

  Data <-
    copy(.Data)

  Data <-
    Data %>%
    .[, max(Jahr), by = Beobachter]

  setnames(Data, "V1", "Jahr")

  Data
}
