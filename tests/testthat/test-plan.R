context("Planners")

test_that("create_plan works", {
  expect_error(create_plan(data.frame(NULL)), NA)

  plan <- structure(
    list(file_name = "events.2018.sample.tab",
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
    list(file_name = "events.2018.sample.tab",
         action = structure(4L, .Label = c("none", "download", "delete", "ingest_from_file", "ingest_from_memory", "remove"), class = "factor"),
         where = "in database",
         in_local = TRUE,
         in_db = FALSE,
         data_set = "2018"),
    row.names = c(NA, -1L),
    class = c("icews_plan", "tbl_df", "tbl", "data.frame"))
  expect_equal(plan, target)

  clean_mock_environment(p)
})

test_that("plan_file_changes works", {
  p <- setup_mock_environment(TRUE, FALSE, FALSE)
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  expect_error(
    plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      plan_file_changes(p$raw_file_dir)
    ),
    NA
  )

  clean_mock_environment(p)
})


test_that("plan_database_changes works", {
  p <- setup_mock_environment(TRUE, TRUE, FALSE)
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  expect_error(
    plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      plan_database_changes(p$db_path, p$raw_file_dir, keep_files = FALSE,
                            use_local = TRUE)
    ),
    NA
  )

  clean_mock_environment(p)
})

