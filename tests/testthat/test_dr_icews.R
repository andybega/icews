context("dr_icews")

test_that("path arguments are respected regardless of options", {
  skip("depends on local state")

  opts <- unset_icews_opts()

  expect_error(dr_icews(NULL, NULL))

  dr_icews(raw_file_dir = "~/Downloads/icews_data/raw", NULL)

  dr_icews(db_path = "~/Downloads/icews_data/db/test.sqlite3", NULL)

  set_icews_opts(opts)
  dr_icews()
})
