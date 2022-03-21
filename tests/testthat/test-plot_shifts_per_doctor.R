test_that("plot_shifts_per_doctor() returns a ggplot.", {
  expect_true(ggplot2::is.ggplot(plot_shifts_per_doctor(example_schedule)))
})

test_that("Title and axis labels of plot returned by plot_shifts_per_doctor() are correct.", {
  expect_equal(plot_shifts_per_doctor(example_schedule)$labels$fill, 'Shift Type')
  expect_equal(plot_shifts_per_doctor(example_schedule)$labels$y, 'Shift Count')
  expect_equal(plot_shifts_per_doctor(example_schedule)$labels$x, 'Doctor')
  expect_equal(plot_shifts_per_doctor(example_schedule)$labels$title, 'Shifts Per Doctor')
  expect_equal(plot_shifts_per_doctor(example_schedule)$labels$subtitle, 'Between 2022-02-28 and 2022-05-08')
})
