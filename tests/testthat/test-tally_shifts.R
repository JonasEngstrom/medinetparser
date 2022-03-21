test_that("tally_shifts() returns a tibble of the right dimensions.", {
  expect_true(tibble::is_tibble(tally_shifts(example_schedule)))
  expect_equal(dim(tally_shifts(example_schedule)), c(35, 27))
})
