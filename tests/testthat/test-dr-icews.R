context("dr_icews")

test_that("dr_icews works", {
  expect_error(capture.output(dr_icews(db_path = "", raw_file_dir = "")), NA)
})

test_that("path arguments are respected regardless of options", {

  opts <- unset_icews_opts()

  expect_error(dr_icews(NULL, NULL))

  o1 <- capture.output(dr_icews(raw_file_dir = "~/Downloads/icews_data/raw", NULL))
  o2 <- capture.output(dr_icews(db_path = "~/Downloads/icews_data/db/test.sqlite3", NULL))
  set_icews_opts("~/icews_data2", TRUE, TRUE)
  o3 <- capture.output(dr_icews())

  set_icews_opts(opts)

  expect_true(!identical(o1, o2))
  expect_true(!identical(o1, o2))
  expect_equal(o1[1], "Checking ICEWS options...")
  expect_equal(o2[1], "Checking ICEWS options...")
  expect_equal(o3[1], "Checking ICEWS options...options are set")

})

test_that("paths are used even if opts are specified", {

  opts <- unset_icews_opts()

  o1a <- capture.output(dr_icews(raw_file_dir = "~/Downloads/icews_data/raw", NULL))
  o2a <- capture.output(dr_icews(db_path = "~/Downloads/icews_data/db/test.sqlite3", NULL))

  set_icews_opts("~/icews_data2", TRUE, TRUE)

  o1b <- capture.output(dr_icews(raw_file_dir = "~/Downloads/icews_data/raw", NULL))
  o2b <- capture.output(dr_icews(db_path = "~/Downloads/icews_data/db/test.sqlite3", NULL))

  set_icews_opts(opts)

  expect_equal(o1a[8], o1b[9])
  expect_equal(o2a[8], o2b[9])

  expect_equal(o1b[6], "One or both path arguments are not NULL, disregarding option values")
  expect_equal(o2b[6], "One or both path arguments are not NULL, disregarding option values")


})
