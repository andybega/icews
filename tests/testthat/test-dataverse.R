

test_that("download_file works", {
  td <- tempdir()
  # runs without error

  mockery::stub(download_file, "dataverse::get_file", function(...) readBin(tsv_sample_path(), "raw", 1e6),
       depth = 2)

  out <- mockr::with_mock(
    check_dataverse_version = function() invisible(TRUE),  # dataverse version issue, see #72
    {
      download_file(file = "events.2018.sample.tab", to_dir = td)
    }
  )
  # returns link to downloaded data
  expect_equal(basename(out), "events.2018.sample.tab")

  # complains about vector input
  expect_error(
    mockr::with_mock(
      check_dataverse_version = function() invisible(TRUE),  # dataverse version issue, see #72
      {
        download_file(file = c("a", "b"), to_dir = td)
      }
    ),
    "I'm not vectorized"
  )
})





