context("update_icews")

test_that("input checks work", {

  expect_error(update_icews(dryrun = 0), "dryrun argument should be TRUE or FALSE")
  expect_error(update_icews(use_db = 0), "use_db argument should be TRUE or FALSE")

})
