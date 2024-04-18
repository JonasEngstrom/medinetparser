merge_schedule_files <- function(list_of_file_paths) {
  return_tibble <-
    list_of_file_paths[1] |>
    load_tidy_schedule()

  return_tibble |>
    (\(x) return())()
}
