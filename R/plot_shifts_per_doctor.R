#' Plot Shifts Per Doctor
#'
#' Plots a bar graph of the total shifts per doctor, stacked by shift type.
#'
#' @param tidy_schedule A tibble containing a schedule, as loaded by
#'   `load_tidy_schedule()`.
#'
#' @return A ggplot with a stacked bar chart.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' # Display graph right away.
#' plot_shifts_per_doctor(tidy_schedule)
#'
#' # Plot only a subset of shifts.
#' tidy_schedule %>%
#'     dplyr::filter(shift_type %in% c('Pjour', 'Bjour')) %>%
#'     plot_shifts_per_doctor()
#'
#' # Same as above but also change axis label and save plot as
#' # an object for later use.
#' plot_of_shifts <- tidy_schedule %>%
#'     dplyr::filter(shift_type %in% c('Pjour', 'Bjour')) %>%
#'     plot_shifts_per_doctor() +
#'     ggplot2::ylab('Number of Night Shifts')
plot_shifts_per_doctor <- function(tidy_schedule) {
  tidy_schedule %>%
    ggplot2::ggplot(aes(x = doctor_name %>%
                          forcats::fct_infreq() %>%
                          forcats::fct_rev(),
                        fill = shift_type)) +
    ggplot2::geom_bar() +
    ggplot2::coord_flip() +
    ggplot2::ggtitle('Shifts Per Doctor',
                     paste('Between',
                           medinetparser::get_min_max_dates(tidy_schedule)[1],
                           'and',
                           medinetparser::get_min_max_dates(tidy_schedule)[2])
                     ) +
    ggplot2::xlab('Doctor') +
    ggplot2::ylab('Shift Count') +
    ggplot2::labs(fill = 'Shift Type') %>%
    return()
}
