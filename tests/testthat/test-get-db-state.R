

test_that("list_source_files works", {
  p <- setup_mock_db(populate = TRUE)
  expect_error(o <- list_source_files(p), NA)
  expect_equal(o, "events.2018.sample.tab")
})

test_that("get_db_state works", {
  p <- setup_mock_db(populate = TRUE)
  expect_error(o <- get_db_state(p), NA)
  expect_equal(o$file_name, "events.2018.sample.tab")
})
