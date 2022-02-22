#' Tally Shifts
#'
#' Outputs a tally of different shift types against each doctor in the schedule.
#'
#' @param table A table as extracted by `extract_tables()` and indexed by `find_largest_table()`.
#'
#' @return A tibble of shift types against doctors.
#' @export
#' @seealso [medinetparser::extract_tables()], [medinetparser::find_largest_table()]
#' @md
#'
#' @examples
#' shift_tallies <- tally_shifts(table[[19]])
tally_shifts <- function(table) {
  temporary_table <- table %>%
    dplyr::select(X1, X2) %>%
    dplyr::slice(
      which(
        grepl(',', table[[1]]) & !grepl('\\d', table[[1]])
      )[1]:n()
    ) %>%
    dplyr::mutate(X2 = if_else(grepl(',', X1), X1, '')) %>%
    dplyr::na_if('') %>%
    tidyr::fill(X2) %>%
    dplyr::filter(!(X1 %in% X2)) %>%
    tidyr::pivot_wider(names_from = X2,
                       values_from = X1,
                       names_repair = 'unique',
                       values_fn = list)

  return_table <- tibble::tibble()

  for (i in 1:length(temporary_table)) {
    return_table <- temporary_table[i] %>%
      colnames() %>%
      tibble::as_tibble_col(column_name = 'Doctor') %>%
      dplyr::bind_cols(
        tibble::as_tibble_row(
          temporary_table[[i]][[1]] %>% table(), .name_repair = 'minimal'
        )
      ) %>%
      dplyr::bind_rows(return_table)
  }

  return_table %>%
    dplyr::mutate(across(!Doctor, as.integer)) %>%
    dplyr::mutate(across(!Doctor, ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::arrange(Doctor) %>%
    return()
}
