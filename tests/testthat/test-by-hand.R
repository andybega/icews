context("Manual tests; expensive to run or depend on local state")

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


test_that("database is correctly initialized", {
  skip("run manually, too slow for routine")

  unlink("~/Downloads/icews_data/test.sqlite3")
  con <- create_db("~/Downloads/icews_data/test.sqlite3")
  expect_equal(dbListTables(con), c("events", "source_files", "stats"))
  expect_equal(dbGetQuery(con, "SELECT value from stats where name == 'events_n';")[[1]], 0L)
  dbDisconnect(con)
  unlink("~/Downloads/icews_data/test.sqlite3")
})
