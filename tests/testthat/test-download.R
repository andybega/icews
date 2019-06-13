context("Download/file functions")

test_that("purge_data_files works", {

  p <- setup_mock_raw(populate = TRUE)
  expect_error(purge_data_files(p), NA)
  expect_true(length(dir(p))==0)

})


test_that("download_data works", {

  raw_file_dir <- file.path(tempdir(), "raw")
  if (length(dir(raw_file_dir)) > 0) {
    unlink(dir(raw_file_dir, full.names = TRUE))
  }
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  expect_error(
    plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      capture.output(
        download_data(dryrun = TRUE, to_dir = raw_file_dir, update = TRUE)
      )
    ),
    NA
  )
})
