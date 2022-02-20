#' Find Week Number
#'
#' Finds the first week number in the schedule and returns "v [week number]".
#'
#' @param table Schedule found using `extract_tables()` and `find_largest_table()`.
#'
#' @return First week number of schedule in format "v [week number]".
#' @export
#' @seealso [medinetparser::extract_tables()], [medinetparser::find_largest_table()]
#' @md
#'
#' @examples
#' week_number <- find_week_number(tables[[19]])
find_week_number <- function(table) {
  table %>%
    dplyr::filter(grepl('^v\\s\\d{1,2}$', X1)) %>%
    dplyr::slice_head() %>%
    dplyr::pull(X1) %>%
    return()
}
