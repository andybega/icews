context("Manual tests; expensive to run or depend on local state")

test_that("download_data works with user specified path", {
  skip("run manually, too slow for routine")

  plan <- download_data("~/icews_data", dryrun = TRUE)
  expect_gt(sum(plan$action=="download"), 20)

})


