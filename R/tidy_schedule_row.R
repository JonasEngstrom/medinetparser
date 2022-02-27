tidy_schedule_row <- function(schedule_row, encoding_keys, schedule_dates) {
  rearranged_table <- schedule_row %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      function(x) medinetparser::encode_categories(x, encoding_keys)
      )) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::mutate(decoded = medinetparser::decode_categories(
      value,
      encoding_keys
      )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(size = length(decoded)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(size = ifelse(size == 1, 0, size)) %>%
    dplyr::mutate(counter = ifelse(size > 0, 2, 1))

  max_categories <- rearranged_table %>%
    dplyr::select(size) %>%
    max()

  return_table <- rearranged_table %>%
    dplyr::slice(1:(max_categories+1)) %>%
    dplyr::mutate(value = -1, size = 0) %>%
    dplyr::bind_rows(rearranged_table)

  for (i in 1:max_categories) {
    return_table <- return_table %>%
      dplyr::mutate(size = ifelse(dplyr::lag(size) > 0,
                                  dplyr::lag(size) - 1,
                                  size))
    }

  return_table %>%
    dplyr::mutate(size = dplyr::lag(size)) %>%
    dplyr::filter(value != -1, size == 0) %>%
    tidyr::uncount(counter) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 0) %>%
    dplyr::mutate(doctor = dplyr::slice(tab2, 1) %>% dplyr::pull(decoded)) %>%
    dplyr::slice(2:n()) %>%
    dplyr::slice(1:dplyr::pull(dplyr::count(schedule_dates))) %>%
    dplyr::bind_cols(schedule_dates) %>%
    dplyr::select(-name, -value, -size) %>%
    dplyr::relocate(date, doctor, shift_type = decoded) %>%
    tidyr::unnest(c(shift_type, doctor)) %>%
    dplyr::mutate(across(c(doctor, shift_type), forcats::as_factor)) %>%
    return()
}
