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

test_that("read_icews respects paths",  {
  p_empty <- setup_mock_db(populate = FALSE)
  p_full  <- setup_mock_db(populate = TRUE)
  p_def <- setup_mock_db(file.path(tempdir(), "db/icews.sqlite3"), populate = FALSE)

  opts <- unset_icews_opts()
  set_icews_opts(tempdir(), TRUE, FALSE)
  # by fallback to opts should be empty
  o_def <- read_icews(n_max = 5L)
  o_empty <- read_icews(path = p_empty, n_max = 5L)
  o_full  <- read_icews(path = p_full, n_max = 5L)
  set_icews_opts(opts)

  expect_equal(nrow(o_def), 0L)
  expect_equal(nrow(o_empty), 0L)
  expect_equal(nrow(o_full), 5L)
})

test_that("read_events_tsv works", {

  sample_tsv <- system.file("extdata", "events.2018.sample.tab", package = "icews")

  expect_error(events <- read_events_tsv(sample_tsv), NA)
  expect_true(names(events)[1]=="event_id")

  expect_error(events <- read_events_tsv(sample_tsv, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

})

test_that("read_icews_raw works", {

  raw_file_dir <- setup_mock_raw(populate = TRUE)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L), NA)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

})

test_that("read from local or db returns same column info and types", {
  p <- setup_mock_environment(pop_raw = TRUE, pop_db = TRUE)

  o_raw <- read_icews(p$raw_file_dir, n_max = 5L)
  o_db  <- read_icews(p$db_path, n_max = 5L)
  o_db  <- o_db[, -match("source_file", names(o_db))]

  local_classes <- sapply(o_raw, class)
  db_classes    <- sapply(o_db, class)

  expect_equal(local_classes, db_classes)

  expect_equal(o_db, o_raw)

})
