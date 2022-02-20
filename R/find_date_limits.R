#' Find Date Limits
#'
#' Finds the first and last date of the schedule.
#'
#' @param table Schedule table as found by `extract_tables()`.
#' @param first_year_of_schedule First year of schedule as found by
#'   `find_first_year()`.
#'
#' @return Outputs a list consisting of two lubridate dates.
#' @export
#' @seealso [medinetparser::extract_tables()],
#'   [medinetparser::find_first_year()]
#' @md
#'
#' @examples
#' dates <- find_date_limits(table, '2022')
find_date_limits <- function(table, first_year_of_schedule) {
  date_limits <- table %>%
    dplyr::filter(grepl('^..\\s\\d{1,2}/\\d{1,2}$', X2)) %>%
    tidyr::pivot_longer(names_to = 'name_col', cols = everything()) %>%
    dplyr::select(value) %>%
    dplyr::filter(grepl('^..\\s\\d{1,2}/\\d{1,2}$', value)) %>%
    slice(c(1, n())) %>%
    pull()

  first_day <- date_limits[1] %>%
    stringr::str_extract('\\d{1,2}')

  first_month <- date_limits[1] %>%
    stringr::str_extract('\\d{1,2}$')

  last_day <- date_limits[2] %>%
    stringr::str_extract('\\d{1,2}')

  last_month <- date_limits[2] %>%
    stringr::str_extract('\\d{1,2}$')

  if (first_month>last_month) {
    last_year_of_schedule <- as.integer(first_year_of_schedule) + 1
  } else {
    last_year_of_schedule <- as.integer(first_year_of_schedule)
  }

  first_date <- c(first_year_of_schedule, first_month, first_day) %>%
    stringr::str_pad(2, pad = '0') %>%
    paste(collapse = '') %>%
    lubridate::ymd()

  last_date <- c(last_year_of_schedule, last_month, last_day) %>%
    stringr::str_pad(2, pad = '0') %>%
    paste(collapse = '') %>%
    lubridate::ymd()

  return(c(first_date, last_date))
}
