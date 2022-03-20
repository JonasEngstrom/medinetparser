#' Plot Difference from Mean
#'
#' Plots a bar graph of the difference between the number of shifts that a
#' doctor is scheduled and the mean for all the doctors in the schedule.
#'
#' @param tidy_schedule A tibble containing a schedule, as loaded by
#'   `load_tidy_schedule()`.
#'
#' @return A ggplot with a bar chart.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @examples
#' # Show plot right away.
#' plot_difference_from_mean(example_schedule)
#'
#' # Filter certain shift types, exclude one doctor, and save the ggplot as an object for later use.
#' saved_graph <- example_schedule %>%
#'     dplyr::filter(shift_type %in% c('Pjour', 'Mjour')) %>%
#'     dplyr::filter(doctor_name != 'Olsson, Sofie') %>%
#'     plot_difference_from_mean()
plot_difference_from_mean <- function(tidy_schedule) {
  averaged_schedule <- tidy_schedule %>%
    dplyr::group_by(.data$doctor_name) %>%
    dplyr::summarize(forcats::fct_count(.data$shift_type)) %>%
    dplyr::summarize(sum_of_shifts = sum(.data$n)) %>%
    dplyr::mutate(difference_from_mean = .data$sum_of_shifts - mean(.data$sum_of_shifts)) %>%
    dplyr::ungroup()

  averaged_schedule %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$doctor_name %>%
                                   stats::reorder(.data$difference_from_mean),
                                 y = .data$difference_from_mean)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::ggtitle('Difference from Mean Number of Shifts',
                     paste('Between',
                           medinetparser::get_min_max_dates(tidy_schedule)[1],
                           'and',
                           medinetparser::get_min_max_dates(tidy_schedule)[2])) +
    ggplot2::ylab(paste('Difference in Number of Shifts from the Mean of ',
                        averaged_schedule %>%
                          dplyr::summarize(mean(.data$sum_of_shifts)) %>%
                          round(2),
                        ' Shifts',
                        sep = '')) +
    ggplot2::xlab('Doctor') %>%
    return()
}
