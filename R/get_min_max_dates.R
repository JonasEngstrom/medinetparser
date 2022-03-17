#' Get Min Max Dates
#'
#' Returns a list of the first and last date in a schedule as returned by
#' `load_tidy_schedule()`. Meant to be used for creating titles for plots.
#'
#' @param tidy_schedule A schedule as returned by `load_tidy_schedule()`
#'
#' @return A list where the first value is the first date in the schedule and
#'   the last value is the last value in the schedule.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' min_max_dates <- get_min_max_dates(tidy_schedule)
get_min_max_dates <- function(tidy_schedule) {
  c(tidy_schedule %>%
      dplyr::summarise(min(date)) %>%
      dplyr::pull(),
    tidy_schedule %>%
      dplyr::summarise(max(date)) %>%
      dplyr::pull()) %>%
    return()
}
