#' Find First Year
#'
#' Find the first year in the schedule.
#'
#' @param tables Tables output by `extract_tables()`.
#' @param first_week_number Week number in format "v [week number] as output by `find_week_number()`.
#'
#' @return The first year of the schedule as a character variable.
#' @export
#' @seealso [medinetparser::extract_tables()], [medinetparser::find_week_number()]
#' @md
#'
#' @examples
#' first_year <- find_first_year(tables, 'v 7')
find_first_year <- function(tables, first_week_number) {
  result_list <- c()

  for (i in 1:length(tables)) {
    result_list <- tables[[i]][1] %>%
      dplyr::filter(grepl('v \\d{1,2},', X1)) %>%
      dplyr::pull() %>%
      c(result_list)
  }

  result_list %>%
    stringr::str_extract(paste('\\d{4}', first_week_number, sep = '')) %>%
    stringr::str_sub(end = 4L) %>%
    return()
}
