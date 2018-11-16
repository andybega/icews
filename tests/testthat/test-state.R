
context("State")

test_that("Labels are correctly parsed", {
  labels <- c("changes.txt", "events.1995.20150313082510.tab.zip")

  x <- parse_label(labels[1])
  expect_is(x, "data.frame")
  expect_false(x$is_data)

  x <- parse_label(labels[2])
  expect_true(x$is_data)
  expect_equal(x$file, "events.1995.20150313082510.tab")

  x <- parse_label(labels)
  expect_is(x, "data.frame")
  expect_equal(x$file, c("changes.txt", "events.1995.20150313082510.tab"))


})
