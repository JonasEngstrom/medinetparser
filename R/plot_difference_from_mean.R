#' Plot Difference from Mean
#'
#' Plots a bar graph of the difference between the number of shifts that a
#' doctor i schedule and the mean for all the doctors in the schedule.
#'
#' @param tidy_schedule A tibble containing a schedule, as loaded by
#'   `load_tidy_schedule()`.
#' @param text_distance The distance between the top of the bars and the number
#'   of shifts. Defaults to 0.25.
#'
#' @return A ggplot with a bar chart.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' # Show plot right away.
#' plot_difference_from_mean(tidy_schedule)
#'
#' # Show plot right away, adjusting the distance of the text from the top of the bars.
#' plot_difference_from_mean(tidy_schedule, 2)
#'
#' # Filter certain shift types, exclude one doctor, and save the ggplot as an object for later use.
#' saved_graph <- tidy_schedule %>%
#'     filter(shift_type %in% c('Pjour', 'Mjour')) %>%
#'     filter(doctor_name != 'Hansson, Anders') %>%
#'     plot_difference_from_mean()
plot_difference_from_mean <- function(tidy_schedule, text_distance = 0.25) {
  averaged_schedule <- tidy_schedule %>%
    dplyr::group_by(doctor_name) %>%
    dplyr::summarize(forcats::fct_count(shift_type)) %>%
    dplyr::summarize(sum(n)) %>%
    dplyr::mutate(difference_from_mean = `sum(n)` - mean(`sum(n)`)) %>%
    dplyr::ungroup()

  averaged_schedule %>%
    ggplot2::ggplot(ggplot2::aes(x = doctor_name %>%
                                   stats::reorder(difference_from_mean),
                                 y = difference_from_mean)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(y = difference_from_mean + text_distance * sign(difference_from_mean),
                                    label = `sum(n)`)) +
    ggplot2::ggtitle('Difference from Mean Number of Shifts',
                     paste('Between',
                           medinetparser::get_min_max_dates(tidy_schedule)[1],
                           'and',
                           medinetparser::get_min_max_dates(tidy_schedule)[2])) +
    ggplot2::ylab(paste('Axis Shows Difference in Number of Shifts from the Mean of ',
                        averaged_schedule %>%
                          dplyr::summarize(mean(`sum(n)`)) %>%
                          round(2),
                        ' Shifts\nNumbers above Bars Show Absolute Number of Shifts',
                        sep = '')) +
    xlab('Doctor') %>%
    return()
}
