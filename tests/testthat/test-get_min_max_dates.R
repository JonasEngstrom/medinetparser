test_that("get_min_max_dates() returns two dates.", {
  expect_length(get_min_max_dates(example_schedule), 2)
  expect_true(lubridate::is.Date(get_min_max_dates(example_schedule)[1]))
  expect_true(lubridate::is.Date(get_min_max_dates(example_schedule)[2]))
})

test_that("get_min_max_dates() returns correct dates.", {
  expect_equal(get_min_max_dates(example_schedule)[1], lubridate::ymd('2022-02-28'))
  expect_equal(get_min_max_dates(example_schedule)[2], lubridate::ymd('2022-05-08'))
})
