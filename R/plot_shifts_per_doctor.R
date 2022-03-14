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
                           tidy_schedule %>%
                             summarise(min(date)) %>%
                             pull(),
                           'and',
                           tidy_schedule %>%
                             summarise(max(date)) %>%
                             pull())
                     ) +
    ggplot2::xlab('Doctor') +
    ggplot2::ylab('Shift Count') +
    ggplot2::labs(fill = 'Shift Type') %>%
    return()
}
