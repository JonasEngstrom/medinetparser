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

  message('Importing data from HTML file.')
  schedule_table <- medinetparser::extract_table(file_path)

  message('Finding first year of schedule.')
  first_year <- medinetparser::find_first_year(file_path)

  message('Finding schedule date boundaries.')
  dates <- medinetparser::extract_dates(schedule_table, first_year)

  message('Generating encoding keys.')
  prime_keys <- schedule_table %>%
    dplyr::select(X1) %>%
    unique() %>%
    pull() %>%
    medinetparser::make_prime_keys()

  message('Initializing tibble.')
  return_table <- tibble()

  number_of_doctors <- schedule_table %>%
    dplyr::filter(stringr::str_detect(X2, '.,.')) %>%
    dplyr::count() %>%
    dplyr::pull()

  pb <- progress::progress_bar$new(total = number_of_doctors,
          format = ':elapsedfull Reformatting :percent complete. Current schedule row: :what',
          clear = FALSE)

  for (i in 1:number_of_doctors) {
    sliced_row <- schedule_table %>%
      dplyr::filter(stringr::str_detect(X2, '.,.')) %>%
      dplyr::slice(i)

    pb$tick(tokens = list(what = sliced_row %>% pull(X2)))

    return_table <- return_table %>%
    dplyr::bind_rows(
      medinetparser::tidy_schedule_row(
        sliced_row,
        prime_keys,
        dates)
      )
  }

  message('Creating factors.')
  return_table %>%
    dplyr::mutate(dplyr::across(c(doctor, shift_type), forcats::as_factor)) %>%
    return()
}
