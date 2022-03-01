#' Extract Tidy Schedule
#'
#' Takes the name of an HTML file from Medinet and outputs a tidy tibble with the schedule data.
#'
#' @param file_path File path to HTML file from Medinet.
#'
#' @return A tidy tibble with schedule data.
#' @export
#'
#' @examples
#' tidy_schedule <- extract_tidy_schedule('./schedule.html')
extract_tidy_schedule <- function(file_path) {
  schedule_table <- medinetparser::extract_table(file_path)

  first_year <- medinetparser::find_first_year(file_path)

  dates <- medinetparser::extract_dates(schedule_table, first_year)

  prime_keys <- schedule_table %>%
    dplyr::select(X1) %>%
    unique() %>%
    pull() %>%
    medinetparser::make_prime_keys()

  return_table <- tibble()

  for (
    i in 1:(
      schedule_table %>%
      dplyr::filter(
        stringr::str_detect(
          X2,
          '.,.'
          )
        ) %>%
      dplyr::count() %>%
      dplyr::pull()
      )
    ) {
    return_table <- return_table %>%
    dplyr::bind_rows(
      medinetparser::tidy_schedule_row(
        schedule_table %>%
          dplyr::filter(
            stringr::str_detect(
              X2,
              '.,.'
              )
            ) %>%
          dplyr::slice(i),
        prime_keys,
        dates)
      )
  }

  return_table %>%
    dplyr::mutate(dplyr::across(c(doctor, shift_type), forcats::as_factor)) %>%
    return()
}
