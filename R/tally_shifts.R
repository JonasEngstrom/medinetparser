#' Tally Shifts
#'
#' Outputs a tally of different shift types against each doctor in the schedule.
#'
#' @param schedule_table A table as extracted by `extract_table()`.
#'
#' @return A tidy tibble of shift types against doctors.
#' @export
#' @seealso [medinetparser::extract_table()]
#' @md
#'
#' @examples
#' shift_tallies <- tally_shifts(schedule_table)
tally_shifts <- function(schedule_table) {
  temporary_table <- schedule_table %>%
    dplyr::select(X1, X2) %>%
    dplyr::slice(
      which(
        grepl(',', schedule_table[[1]]) & !grepl('\\d', schedule_table[[1]])
      )[1]:n()
    ) %>%
    dplyr::mutate(X2 = if_else(grepl(',', X1), X1, '')) %>%
    dplyr::na_if('') %>%
    tidyr::fill(X2) %>%
    dplyr::filter(!grepl('v \\d{1,2}', X1) & !grepl('v \\d{1,2}', X2)) %>%
    dplyr::filter(!(X1 %in% X2)) %>%
    tidyr::pivot_wider(names_from = X2,
                       values_from = X1,
                       names_repair = 'unique',
                       values_fn = list)

  return_table <- tibble::tibble()

  for (i in 1:length(temporary_table)) {
    return_table <- temporary_table[i] %>%
      colnames() %>%
      tibble::as_tibble_col(column_name = 'doctor_name') %>%
      dplyr::bind_cols(
        tibble::as_tibble_row(
          temporary_table[[i]][[1]] %>% table(), .name_repair = 'minimal'
        )
      ) %>%
      dplyr::bind_rows(return_table)
  }

  return_table %>%
    dplyr::mutate(across(!doctor_name, as.integer)) %>%
    dplyr::mutate(across(!doctor_name, ~ tidyr::replace_na(.x, 0))) %>%
    tidyr::pivot_longer(-doctor_name, names_to = 'shift_type', values_to = 'n') %>%
    dplyr::arrange(doctor_name, shift_type) %>%
    dplyr::mutate(across(c(doctor_name, shift_type), as_factor)) %>%
    return()
}
