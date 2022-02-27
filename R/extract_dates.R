#' Extract Dates
#'
#' Extracts the dates from a schedule extracted by `extract_table()`, setting the year of all the dates to the same year.
#'
#' @param schedule_table A schedule table extracted bu `extract_table()`
#' @param first_year The first year of the dates, as found by `find_first_year()`.
#'
#' @return A tibble of the dates in the schedule.
#' @export
#' @seealso [medinetparser::extract_table()], [medinetparser::find_first_year()]
#' @md
#'
#' @examples
#' dates <- extract_dates(schedule_data, 2022)
extract_dates <- function(schedule_table, first_year) {
  return_table <- schedule_table %>%
    dplyr::filter(
      if_any(.fns = function(x) stringr::str_detect(x, '\\d{1,2}/\\d{1,2}'))
      ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        function(x) paste(
          first_year,
          stringr::str_extract(x, '\\d{1,2}$') %>%
            stringr::str_pad(2, pad = '0'),
          stringr::str_extract(x, '\\d{1,2}') %>%
            stringr::str_pad(2, pad = '0'), sep = '-') %>%
                    lubridate::ymd() %>%
          return()
        )
      ) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::distinct(value) %>%
    tidyr::drop_na() %>%
    dplyr::rename(date = value)

  return_table %>%
    dplyr::mutate(
      date = lubridate::ymd(
        date + lubridate::years(
          as.integer(
            date <= dplyr::slice(return_table, dplyr::n())
            )
          )
        )
      ) %>%
    dplyr::mutate(
      constant = (
        date %>% lubridate::year() -
          dplyr::slice(return_table, dplyr::n()) %>%
          dplyr::pull(date) %>%
          lubridate::year()
        )
      ) %>%
    dplyr::mutate(constant = constant %>% min()) %>%
   dplyr::mutate(date = date - lubridate::years(constant)) %>%
    dplyr::select(-constant) %>%
    return()
}
