test_that("plot_difference_from_mean() returns a ggplot.", {
  expect_true(ggplot2::is.ggplot(plot_difference_from_mean(example_schedule)))
})

test_that("Title and axis labels of plot returned by plot_difference_from_mean() are correct.", {
  expect_equal(plot_difference_from_mean(example_schedule)$labels$x, 'Doctor')
  expect_equal(plot_difference_from_mean(example_schedule)$labels$y, 'Difference in Number of Shifts from the Mean of 61.89 Shifts')
  expect_equal(plot_difference_from_mean(example_schedule)$labels$title, 'Difference from Mean Number of Shifts')
  expect_equal(plot_difference_from_mean(example_schedule)$labels$subtitle, 'Between 2022-02-28 and 2022-05-08')
})
