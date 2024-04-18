#' Plot Weekend Work Distribution
#'
#' Takes a schedule from `load_tidy_schedule()` and returns a plot of the
#' distribution of weekends worked by doctor, partitioned by split by quartiles
#' of weekends entirely off work.
#'
#' @param tidy_schedule A schedule loaded by `load_tidy_schedule()`.
#' @param shift_types The shift types in Medinet to count as weekend work.
#'   Certain shift types, such as vacation, should not be included in this
#'   vector to yield accurate results.
#' @param shift_days The days to count as weekend. Defaults to Friday, Saturday,
#'   and Sunday, as weekend on-call shifts would start on Friday in the
#'   schedule. This might vary by department and might need to be adjusted.
#' @param quartile_labels Labels for quartiles in the graph. As it defaults to
#'   Swedish, this parameter is included to facilitate use in other languages.
#'   Axis labels and titles can be changed using standard `ggplot2` syntax.
#'
#' @return A ggplot.
#' @export
#' @seealso [medinetparser::load_tidy_schedule()]
#' @md
#'
#' @examples
#' plot_weekend_work(example_schedule)
plot_weekend_work <- function(tidy_schedule,
                              shift_types =
                                c(
                                  'PjourD',
                                  'MjourD',
                                  'Bjour',
                                  'L\u00E5ngF',
                                  'Pjour',
                                  'Mjour'
                                ),
                              shift_days =
                                c(
                                  'Friday',
                                  'Saturday',
                                  'Sunday'
                                ),
                              quartile_labels =
                                c(
                                  '0-25% lediga helger',
                                  '25-50% lediga helger',
                                  '50-75% lediga helger',
                                  '75-100% lediga helger'
                                )
                              ) {
  NULL ->
    weekday ->
    shift_type ->
    doctor_name ->
    week_number ->
    weekend_work ->
    golden_weekend ->
    percentage ->
    quartile

  tidy_schedule |>
    dplyr::mutate(
      weekday = weekdays(date),
      week_number = lubridate::week(date),
      weekend_work = weekday %in% shift_days & shift_type %in% shift_types
    ) |>
    dplyr::group_by(
      doctor_name,
      week_number
    ) |>
    dplyr::summarize(
      golden_weekend = sum(weekend_work) |>
        as.logical() |>
        magrittr::not()
    ) |>
    dplyr::group_by(doctor_name) |>
    dplyr::mutate(percentage = sum(golden_weekend) / (max(week_number) - min(week_number)), quartile = cut(percentage, c(0,0.25,0.5,0.75,1.1), labels = quartile_labels) |> forcats::fct_rev()) |>
    dplyr::ungroup() |>
    dplyr::mutate(quartile = tidyr::replace_na(quartile, quartile_labels[4])) |>
    ggplot2::ggplot(
      ggplot2::aes(x = doctor_name |>
            stats::reorder(-golden_weekend) |>
            forcats::fct_infreq(),
          fill = golden_weekend
      )
    ) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = 'Helt lediga helger kontra arbetade helger uppdelade per l\u00E4kare, mellan' |>
        paste(
          'vecka ' |>
            rbind(
              tidy_schedule |>
                get_min_max_dates() |>
                lubridate::week() |>
                rbind(' \u00E5r ') |>
                rbind(tidy_schedule |> get_min_max_dates() |> lubridate::year())
              ) |>
            rbind(c(' och ', '.')) |>
            c() |>
            (\(x) ifelse(x[4] == x[9], paste(x[c(-3, -4)], collapse = ''), paste(x, collapse = '')))()
        ),
      subtitle = 'Som helt ledig helg r\u00E4knas helg utan l\u00E5ngfredag, prim\u00E4r-, mellan- eller bakjour. Helgdagar ut\u00F6ver l\u00F6rdag och s\u00F6ndag har ej r\u00E4knats in.'
    ) +
    ggplot2::xlab('L\u00E4kare') +
    ggplot2::ylab('Antal helger i Medinet') +
    ggplot2::scale_fill_discrete(
      name = 'F\u00E4rgskala',
      labels = c('Arbetade helger', 'Lediga helger')
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ggplot2::facet_grid(~ quartile, scales = 'free_x', space = 'free_x') |>
    (\(x) return(x))()
}
