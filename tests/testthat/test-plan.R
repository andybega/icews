context("Planners")

test_that("create_plan works", {
  expect_error(create_plan(data.frame(NULL)), NA)

  plan <- structure(
    list(file = "events.2018.sample.tab",
         action = structure(4L, .Label = c("none", "download", "delete", "ingest_from_file", "ingest_from_memory", "remove"), class = "factor"),
         where = "in database",
         data_set = "events.2018",
         in_local = TRUE,
         in_db = FALSE),
    row.names = c(NA, -1L),
    class = c("icews_plan", "tbl_df", "tbl", "data.frame"))

  expect_error(format(plan), NA)
  expect_error(capture.output(print(plan)), NA)
})

test_that("local syn works", {
  p <- setup_mock_environment(TRUE, TRUE, FALSE)
  expect_error(plan <- plan_database_sync(p$db_path, p$raw_file_dir), NA)
  target <- structure(
    list(file = "events.2018.sample.tab",
         action = structure(4L, .Label = c("none", "download", "delete", "ingest_from_file", "ingest_from_memory", "remove"), class = "factor"),
         where = "in database",
         data_set = "events.2018",
         in_local = TRUE,
         in_db = FALSE),
    row.names = c(NA, -1L),
    class = c("icews_plan", "tbl_df", "tbl", "data.frame"))
  expect_equal(plan, target)
  unlink(dir(p$db_path))
  unlink(file.path(p$raw_file_dir, "events.2018.sample.tab"))
})
