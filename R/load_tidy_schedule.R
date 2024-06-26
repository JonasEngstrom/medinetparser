#' Load Tidy Schedule
#'
#' Loads a HTML file from Medinet and returns the schedule as a tidy tibble.
#'
#' `schedule_tibble <- load_tidy_schedule('file_from_medinet.html')`
#'
#' @param file_path String containing the path to the HTML file.
#'
#' @return A tidy tibble containing the schedule data for analysis.
#' @export
#' @md
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
load_tidy_schedule <- function(file_path) {
  html_data <- rvest::read_html(file_path)

  day_elements <- html_data %>%
    rvest::html_elements('[id^=day]')

  day_ids <- day_elements %>%
    rvest::html_attr('id') %>%
    tibble::tibble() %>%
    dplyr::rename(day_id = '.')

  shift_types <- day_elements %>%
    rvest::html_table() %>%
    tibble::tibble() %>%
    dplyr::rename(shift_type = '.')

  rm(day_elements)

  shifts_by_days <- day_ids %>%
    dplyr::bind_cols(shift_types) %>%
    dplyr::mutate(doctor_id = stringr::str_extract(.data$day_id, '\\d{1,3}') %>%
                    as.integer()) %>%
    dplyr::mutate(date = stringr::str_extract(.data$day_id, '\\d{4}-\\d{2}-\\d{2}') %>%
                    lubridate::ymd()) %>%
    dplyr::select(-.data$day_id) %>%
    tidyr::unnest(cols = c('shift_type')) %>%
    dplyr::mutate(shift_type = .data$X1 %>%
                    forcats::as_factor()) %>%
    dplyr::select(-.data$X1)

  rm(day_ids, shift_types)

  doctor_elements <- html_data %>%
    rvest::html_elements('td .js-moveSlot [onmouseover^=Medinet]')

  doctor_names <- doctor_elements %>%
    rvest::html_text() %>%
    tibble::tibble() %>%
    dplyr::rename(doctor_name = '.')

  doctor_ids <- doctor_elements %>%
    rvest::html_attr('onmouseover') %>%
    tibble::tibble() %>%
    dplyr::rename(doctor_id = '.')

  rm(doctor_elements)

  names_by_ids <- doctor_names %>%
    dplyr::bind_cols(doctor_ids) %>%
    dplyr::mutate(doctor_id = stringr::str_extract(.data$doctor_id, '\\d{1,3}') %>%
                    as.integer()) %>%
    dplyr::mutate(doctor_name = .data$doctor_name %>%
                    stringr::str_squish() %>%
                    forcats::as_factor())

  rm(doctor_names, doctor_ids)

  names_by_ids %>%
    dplyr::full_join(shifts_by_days, by = 'doctor_id') %>%
    dplyr::select(-.data$doctor_id) %>%
    dplyr::relocate(date, .data$doctor_name, .data$shift_type) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(shift_type = forcats::fct_drop(.data$shift_type)) %>%
    return()
}
