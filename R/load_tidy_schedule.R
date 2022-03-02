load_tidy_schedule <- function(path_name) {
  html_data <- rvest::read_html(path_name)

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
    dplyr::mutate(doctor_id = stringr::str_extract(day_id, '\\d{3}') %>%
                    as.integer()) %>%
    dplyr::mutate(date = stringr::str_extract(day_id, '\\d{4}-\\d{2}-\\d{2}') %>%
                    lubridate::ymd()) %>%
    dplyr::select(-day_id) %>%
    tidyr::unnest(shift_type) #%>%
    dplyr::mutate(shift_types = shift_types %>%
                    forcats::as_factor())

  rm(day_ids, shift_types)

  doctor_elements <- html_data %>%
    rvest::html_elements('td.user.js-moveSlot [onmouseover^=Medinet]')

  doctor_names <- doctor_elements %>%
    rvest::html_text() %>%
    tibble::tibble() %>%
    dplyr::rename(doctor_name = '.')

  doctor_ids <- doctor_elements %>%
    html_attr('onmouseover') %>%
    tibble::tibble() %>%
    dplyr::rename(doctor_id = '.')

  rm(doctor_elements)

  names_by_ids <- doctor_names %>%
    dplyr::bind_cols(doctor_ids) %>%
    dplyr::mutate(doctor_id = stringr::str_extract(doctor_id, '\\d{1,3}') %>%
                    as.integer()) %>%
    dplyr::mutate(doctor_name = doctor_name %>%
                    forcats::as_factor())

  rm(doctor_names, doctor_ids)

  names_by_ids %>%
    dplyr::full_join(shifts_by_days, by = 'doctor_id') %>%
    return()
}
