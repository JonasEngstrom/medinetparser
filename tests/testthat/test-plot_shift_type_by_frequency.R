test_that("plot_shift_type_by_frequency() returns a ggplot.", {
  expect_true(ggplot2::is.ggplot(plot_shift_type_by_frequency(example_schedule)))
})

test_that("Title and axis labels of plot returned by plot_shift_type_by_frequency() are correct.", {
  expect_equal(plot_shift_type_by_frequency(example_schedule)$labels$y, 'Frequency')
  expect_equal(plot_shift_type_by_frequency(example_schedule)$labels$x, 'Shift Type')
  expect_equal(plot_shift_type_by_frequency(example_schedule)$labels$title, 'Shift Type by Frequency')
  expect_equal(plot_shift_type_by_frequency(example_schedule)$labels$subtitle, 'Between 2022-02-28 and 2022-05-08')
})
