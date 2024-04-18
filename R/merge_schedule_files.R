#' Merge Schedule Files
#'
#' Takes a list of paths to HTML files downloaded from Medinet, imports them
#' using `load_tidy_schedule()` and merges them for analysis, removing duplicate
#' entries.
#'
#' @param list_of_file_paths A list of paths to HTML files downloaded from
#'   Medinet.
#'
#' @return A tibble like that returned by `load_tidy_schedule()`.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' \dontrun{
#' c('path/to/first/file.html', 'path/to/second/file.html') |>
#'   merge_schedule_files()}
merge_schedule_files <- function(list_of_file_paths) {
  return_tibble <-
    list_of_file_paths[1] |>
    load_tidy_schedule()

  if (length(list_of_file_paths) > 1) {
    for (schedule_file in list_of_file_paths[2:length(list_of_file_paths)]) {
      return_tibble <-
        return_tibble |>
        dplyr::bind_rows(
          schedule_file |>
            load_tidy_schedule() |>
            dplyr::filter(!(date %in% dplyr::pull(return_tibble, date)))
        )
    }
  }

  return(return_tibble)
}
