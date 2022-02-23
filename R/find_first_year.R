#' Find First Year
#'
#' Find the first year in the schedule.
#'
#' @param file_path The file path to the schedule HTML file.
#'
#' @return The first year of the schedule as an integer variable.
#' @export
#'
#' @examples
#' first_year <- find_first_year('./data.html')
find_first_year <- function(file_path) {
  file_path %>%
    rvest::read_html() %>%
    rvest::html_elements('#yearweek option') %>%
    stringr::str_squish() %>%
    stringr::str_extract('selected>v \\d, \\d{4}') %>%
    tibble::as_tibble() %>%
    tidyr::drop_na() %>%
    dplyr::pull() %>%
    stringr::str_extract('\\d{4}') %>%
    as.integer() %>%
    return()
}
