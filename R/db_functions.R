#' Shorthands for dbGetQuery and dbExecute
#'
#' These functions assume that a database connection object `pool` exists in the
#' global environment!
#'
#' @rdname db_functions
#' @export
#' @import data.table
db_query <- function(...) {
  X <- pool::dbGetQuery(pool, ...) %>% setDT
  X[]
}

#' @export
#' @rdname db_functions
db_execute <- function(...) {
  pool::dbExecute(pool, ...)
}
