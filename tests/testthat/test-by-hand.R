context("Manual tests; expensive to run or depend on local state")

test_that("read_icews versions return consistent data types", {
  skip("run manually, too slow for routine")

  from_local <- read_icews_raw(find_raw())
  from_db    <- read_icews_db(find_db())

  local_classes <- sapply(from_local, class)
  db_classes    <- sapply(from_db[, -match("source_file", names(from_db))], class)

  expect_equal(local_classes, db_classes)

  expect_equal(head(from_local), head(from_db[, -match("source_file", names(from_db))]))
})

test_that("download_data works with user specified path", {
  skip("run manually, too slow for routine")

  plan <- download_data("~/icews_data", dryrun = TRUE)
  expect_gt(sum(plan$action=="download"), 20)

})

test_that("n_max in read_icews and variants works", {
  skip("run manually, too slow for routine")

  expect_equal(nrow(read_icews(n_max = 5L)), 5L)
  expect_equal(nrow(read_icews_db(find_db(), n_max = 5L)), 5L)
  expect_equal(nrow(read_icews_raw(find_raw(), n_max = 5L)), 5L)
})
