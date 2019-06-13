context("State getter functions")

test_that("get_dvn_manifest captures API errors", {

  expect_error(get_dvn_manifest("foo", server = "demo.dataverse.org"), "Something went wrong in")

})

test_that("list_local_files works", {

  p <- setup_mock_raw(populate = FALSE)
  expect_error(list_local_files(p), NA)
  unlink(file.path(p, "events.2018.sample.tab"))

})

test_that("list_local_files complains about non-data files", {

  p <- setup_mock_raw(populate = FALSE)
  writeLines("", con = file.path(p, "empty.txt"))
  expect_error(list_local_files(p), "unexpected non-data")
  unlink(file.path(p, c("events.2018.sample.tab", "empty.txt")))

})


test_that("get_local_state throws an error when non-event files are present", {

  p <- setup_mock_raw(populate = TRUE)
  writeLines("", con = file.path(p, "empty.txt"))
  expect_error(get_local_state(p))
  unlink(file.path(p, c("events.2018.sample.tab", "empty.txt")))

})

test_that("get_local_state works", {

  p <- setup_mock_raw(populate = TRUE)
  o <- get_local_state(p)
  expect_equal(o$local_file_name, "events.2018.sample.tab")
  unlink(file.path(p, "events.2018.sample.tab"))

})
