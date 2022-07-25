

test_that("download_data works", {

  raw_file_dir <- file.path(tempdir(), "raw")
  if (length(dir(raw_file_dir)) > 0) {
    unlink(dir(raw_file_dir, full.names = TRUE))
  }
  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")

  # with update
  expect_error(
    foo <- capture.output(mockr::with_mock(
      get_dvn_manifest = function() readRDS(ff),
      {
        plan <- download_data(dryrun = TRUE, to_dir = raw_file_dir, update = TRUE)
      }
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
    foo <- capture.output(plan <- mockr::with_mock(
      get_dvn_manifest = function() readRDS(ff),
      {
        download_data(dryrun = TRUE, to_dir = raw_file_dir, update = FALSE)
      }
    )),
    NA
  )

})

test_that("expect deprecate warning for download_icews", {

  raw_file_dir <- file.path(tempdir(), "raw2")
  # add _foo because unlinking files if they already exist in tempdir/raw
  # doesn't work on Windows...

  ff = system.file("testdata", "dvn_manifest.rds", package = "icews")
  rlang::with_options(lifecycle_verbosity = "warning", {
    expect_warning(

      plan <- mockr::with_mock(
        check_dataverse_version = function() invisible(TRUE),  # dataverse version issue, see #72
        get_dvn_manifest = function() readRDS(ff),
        {
          capture.output(
            download_icews(dryrun = TRUE, to_dir = raw_file_dir, update = TRUE)
          )
        }
      ),
      "deprecated"
    )
  })
})
