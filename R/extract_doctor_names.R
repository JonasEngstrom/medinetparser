#' Extract Doctor Names
#'
#' Returns a tibble of the doctors’ names from the schedule.
#'
#' @param table A table as returned by `extract_tables()` and indexed by
#'   `find_largest_table()`.
#'
#' @return A tibble of doctors’ names.
#' @export
#' @seealso [medinetparser::extract_tables()],
#'   [medinetparser::find_largest_table()]
#'
#' @examples
#' doctor_names <- extract_doctor_names(table)
extract_doctor_names <- function(table) {
  table %>%
    dplyr::select(X1) %>%
    dplyr::filter(grepl(',', X1) & !grepl('\\d', X1)) %>%
    unique() %>%
    return()
}
