

test_that("is_data_file recognizes main patterns", {

  expect_true(is_data_file("events.2013.20150313084929.tab"))
  expect_true(is_data_file("20181004-icews-events.zip"))
  expect_true(is_data_file("20200517-icews-events.tab"))

})

test_that("is_data_file recognizes exceptions", {

  expect_true(is_data_file("20200510-icews-events.zip"))
  expect_true(is_data_file("Events.2017.20200602.tab.zip"))

})

test_that("is weeky file differentiates annual from weeky", {

  expect_false(is_weekly_file("events.2013.20150313084929.tab"))
  expect_true(is_weekly_file("20200510-icews-events.zip"))

})


test_that("parse_dataset works with all DVN variations", {

  # basic annual file
  expect_equal(parse_dataset("events.1995.20150313082510.tab"), "1995")

  # with .zip
  expect_equal(parse_dataset("events.1995.20150313082510.tab"), "1995")

  # weekly file
  expect_equal(parse_dataset("20200510-icews-events.zip"), "20200510")

  # special 2017 file
  expect_equal(parse_dataset("Events.2017.20200602.tab.zip"), "2017")

})
