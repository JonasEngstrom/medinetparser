#' Plot Shift Type by Frequency
#'
#' Plots a bar graph of shift type frequencies.
#'
#' @param tidy_schedule A tibble containing a schedule, as loaded by
#'   `load_tidy_schedule()`.
#'
#' @return A ggplot with a bar chart.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' # Display graph right away.
#' plot_shift_type_by_frequency(tidy_schedule)
#'
#' # Plot shifts for a single doctor, collapsing related categories and saving the ggplot to an object for later use.
#' saved_graph <- tidy_schedule %>%
#'     dplyr::filter(doctor_name == 'Åkesson, Annika') %>%
#'     dplyr::mutate(shift_type = forcats::fct_collapse(shift_type, `C-op` = c('Opkir', 'Opkk'))) %>%
#'     plot_shift_type_by_frequency()
plot_shift_type_by_frequency <- function(tidy_schedule) {
  tidy_schedule %>%
    ggplot2::ggplot(aes(x = shift_type %>%
                          forcats::fct_infreq())) +
    ggplot2::geom_bar() +
    ggplot2::ggtitle('Shift Type by Frequency',
                     paste('Between',
                           medinetparser::get_min_max_dates(tidy_schedule)[1],
                           'and',
                           medinetparser::get_min_max_dates(tidy_schedule)[2])) +
    ggplot2::xlab('Shift Type') +
    ggplot2::ylab('Frequency') %>%
    return()
}
