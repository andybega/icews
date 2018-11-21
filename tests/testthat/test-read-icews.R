context("read_icews & co")

test_that("read_icews input checking works", {
  opts <- unset_icews_opts()
  expect_error(read_icews(), "Options are not set and path is NULL")

  # bad n_max
  set_icews_opts("foo", TRUE, TRUE)
  expect_error(read_icews(n_max = 0L), "n_max must be a positive integer")

  # no files at this path (#36)
  #set_icews_opts("foo", TRUE, TRUE)
  #expect_error(read_icews())

})

test_that("read_events_tsv works", {

  sample_tsv <- system.file("extdata", "events.2018.sample.tab", package = "icews")

  expect_error(events <- read_events_tsv(sample_tsv), NA)
  expect_true(names(events)[1]=="event_id")

  expect_error(events <- read_events_tsv(sample_tsv, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

})

test_that("read_icews_raw works", {

  raw_file_dir <- file.path(tempdir(), "raw")
  dir.create(raw_file_dir, showWarnings=FALSE)

  sample_tsv <- system.file("extdata", "events.2018.sample.tab", package = "icews")
  file.copy(sample_tsv, raw_file_dir)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L), NA)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

})
