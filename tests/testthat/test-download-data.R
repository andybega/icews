
test_that("download_data works", {

  raw_file_dir <- file.path(tempdir(), "raw")
  if (length(dir(raw_file_dir)) > 0) {
    unlink(dir(raw_file_dir, full.names = TRUE))
  }
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  # with update
  expect_error(
    foo <- capture.output(plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      download_data(dryrun = TRUE, to_dir = raw_file_dir, update = TRUE)
    )),
    NA
  )

})

test_that("download_data without update in place works", {

  raw_file_dir <- file.path(tempdir(), "raw")
  if (length(dir(raw_file_dir)) > 0) {
    unlink(dir(raw_file_dir, full.names = TRUE))
  }
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  expect_error(
    foo <- capture.output(plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      download_data(dryrun = TRUE, to_dir = raw_file_dir, update = FALSE)
    )),
    NA
  )

})

test_that("expect deprecate warning for download_icews", {

  raw_file_dir <- file.path(tempdir(), "raw")
  if (length(dir(raw_file_dir)) > 0) {
    unlink(dir(raw_file_dir, full.names = TRUE))
  }
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")
  expect_warning(
    plan <- with_mock(
      get_dvn_manifest = function() readRDS(ff),
      capture.output(
        download_icews(dryrun = TRUE, to_dir = raw_file_dir, update = TRUE)
      )
    ),
    "deprecated"
  )

})
