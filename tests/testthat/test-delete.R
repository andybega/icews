
test_that("purge_data_files works", {

  p <- setup_mock_raw(populate = TRUE)
  expect_error(purge_data_files(p), NA)
  expect_true(length(dir(p))==0)

})

