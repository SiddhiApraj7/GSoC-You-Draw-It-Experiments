test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ytdApp works", {
  testthat::expect_silent(ydtApp())
})
