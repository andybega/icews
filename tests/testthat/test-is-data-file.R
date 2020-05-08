
context("is data file, is daily file")

test_that("is_data_file recognizes main patterns", {
  expect_true(is_data_file("events.2013.20150313084929.tab"))
  expect_true(is_data_file("20181004-icews-events.tab"))
})

test_that("is_data_file recognizes exceptions", {

  expect_true(is_data_file("20190309-icews-events-part1.tab"))
  expect_true(is_data_file("20190410-icews-events-1.tab"))
  expect_true(is_data_file("20190503-thru-20190519-icews-events.tab"))

})

test_that("is daily file differentiates annual from daily", {

  expect_false(is_weekly_file("events.2013.20150313084929.tab"))
  expect_true(is_weekly_file("20181004-icews-events.tab"))
  expect_true(is_weekly_file("20190309-icews-events-part1.tab"))
  expect_true(is_weekly_file("20190410-icews-events-1.tab"))
  expect_true(is_weekly_file("20190503-thru-20190519-icews-events.tab"))

})
