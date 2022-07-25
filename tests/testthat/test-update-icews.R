

test_that("input checks work", {

  expect_error(update_icews(dryrun = 0), "dryrun argument should be TRUE or FALSE")
  expect_error(update_icews(use_db = 0), "use_db argument should be TRUE or FALSE")

})


test_that("update_icews works", {

  p <- setup_mock_environment(TRUE, FALSE, FALSE)
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  expect_error(
    plan <- mockr::with_mock(
      get_dvn_manifest = function() readRDS(ff),
      {
        capture.output(
          suppressMessages(update_icews(dryrun = TRUE, use_db = TRUE, keep_files = TRUE,
                                        db_path = p$db_path, raw_file_dir = p$raw_file_dir,
                                        quiet = TRUE))
        )
      }
    ),
    NA
  )

  clean_mock_environment(p)

})
