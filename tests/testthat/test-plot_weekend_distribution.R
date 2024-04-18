test_that("plot_weekend_work() returns a ggplot.", {
  expect_true(ggplot2::is.ggplot(plot_weekend_work(example_schedule)))
})

test_that("Title and axis labels of plot returned by plot_weekend_work() are correct on the default test data.", {
  expect_equal(plot_weekend_work(example_schedule)$labels$x, 'Läkare')
  expect_equal(plot_weekend_work(example_schedule)$labels$y, 'Antal helger i Medinet')
  expect_equal(plot_weekend_work(example_schedule)$labels$title, 'Helt lediga helger kontra arbetade helger uppdelade per läkare, mellan vecka 9 och vecka 19 år 2022.')
  expect_equal(plot_weekend_work(example_schedule)$labels$subtitle, 'Som helt ledig helg räknas helg utan långfredag, primär-, mellan- eller bakjour. Helgdagar utöver lördag och söndag har ej räknats in.')
})

test_that("Title and axis labels of plot returned by plot_weekend_work() are correct when test data is changed to span more than one calendar year.", {
  expect_equal(plot_weekend_work(example_schedule |> dplyr::mutate(date = dplyr::if_else(doctor_name == 'Jansson, Emil', lubridate::date('2021-02-28'), date)))$labels$x, 'Läkare')
  expect_equal(plot_weekend_work(example_schedule |> dplyr::mutate(date = dplyr::if_else(doctor_name == 'Jansson, Emil', lubridate::date('2021-02-28'), date)))$labels$y, 'Antal helger i Medinet')
  expect_equal(plot_weekend_work(example_schedule |> dplyr::mutate(date = dplyr::if_else(doctor_name == 'Jansson, Emil', lubridate::date('2021-02-28'), date)))$labels$title, 'Helt lediga helger kontra arbetade helger uppdelade per läkare, mellan vecka 9 år 2021 och vecka 19 år 2022.')
  expect_equal(plot_weekend_work(example_schedule |> dplyr::mutate(date = dplyr::if_else(doctor_name == 'Jansson, Emil', lubridate::date('2021-02-28'), date)))$labels$subtitle, 'Som helt ledig helg räknas helg utan långfredag, primär-, mellan- eller bakjour. Helgdagar utöver lördag och söndag har ej räknats in.')
})
