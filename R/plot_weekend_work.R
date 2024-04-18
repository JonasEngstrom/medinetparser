plot_weekend_work <- function(tidy_schedule,
                              shift_types =
                                c(
                                  'PjourD',
                                  'MjourD',
                                  'Bjour',
                                  'LångF',
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
    dplyr::mutate(percentage = sum(golden_weekend) / (max(week_number) - min(week_number)), quartile = cut(percentage, c(0,0.25,0.5,0.75,1.1), labels = quartile_labels) |>  forcats::fct_rev()) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(
      ggplot2::aes(x = doctor_name |>
            reorder(-golden_weekend) |>
            forcats::fct_infreq(),
          fill = golden_weekend
      )
    ) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = 'Helt lediga helger kontra arbetade helger uppdelade per läkare på AnOpIVA CSK, årets första 22 veckor år 2024',
      subtitle = 'Som helt ledig helg räknas helg utan långfredag, primär-, mellan- eller bakjour. Helgdagar utöver lördag och söndag har ej räknats in.'
    ) +
    ggplot2::xlab('Läkare') +
    ggplot2::ylab('Antal helger i Medinet') +
    ggplot2::scale_fill_discrete(
      name = 'Färgskala',
      labels = c('Arbetade helger', 'Lediga helger')
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ggplot2::facet_grid(~ quartile, scales = 'free_x', space = 'free_x') |>
    (\(x) return(x))()
}
