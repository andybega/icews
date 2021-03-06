
test_that("read_icews input checking works", {
  opts <- unset_icews_opts()
  expect_error(read_icews(), "Options are not set and path is NULL")

  # bad n_max
  set_icews_opts("foo", TRUE, TRUE)
  expect_error(read_icews(n_max = 0L), "n_max must be a positive integer")

})

test_that("read_icews gives informative errors for empty dirs (#36)", {

  set_icews_opts("foo", TRUE, TRUE)
  expect_error(read_icews(), "maybe this is the wrong path?")

  # non-relevant directory (neither db file nor data files)
  expect_error(read_icews(getwd()), "neither data file nor a .sqlite3")

  # non-existing directory
  empty <- file.path(tempdir(), "empty")
  dir.create(empty, showWarnings = FALSE)
  expect_error(read_icews(empty), "empty directory")

  # non-existing file
  expect_error(read_icews(tempfile()), "the file")

  # check the low-level readers as well
  expect_error(read_icews_raw("foo"), "maybe this is the wrong path?")
  expect_error(read_icews_db("foo/foo.sqlite3"), "maybe this is the wrong path?")

})

test_that("n_max in read_icews and variants works", {
  p <- setup_mock_environment(TRUE, TRUE, TRUE)
  opts <- unset_icews_opts()
  set_icews_opts(dirname(p$raw_file_dir), TRUE, TRUE)

  expect_equal(nrow(read_icews(n_max = 5L)), 5L)
  expect_equal(nrow(read_icews_db(p$db_path, n_max = 5L)), 5L)
  expect_equal(nrow(read_icews_raw(p$raw_file_dir, n_max = 5L)), 5L)

  set_icews_opts(opts)
  unlink(p$db_path)
  unlink(p$raw_file_dir, recursive = TRUE)
})

test_that("read_icews respects paths",  {
  p_empty <- setup_mock_db(populate = FALSE)
  p_full  <- setup_mock_db(populate = TRUE)
  p_def <- setup_mock_db(file.path(tempdir(), "db/icews.sqlite3"), populate = FALSE)

  opts <- unset_icews_opts()
  set_icews_opts(tempdir(), TRUE, FALSE)
  # by fallback to opts should be empty
  o_def   <- read_icews(n_max = 5L)
  o_empty <- read_icews(path = p_empty, n_max = 5L)
  o_full  <- read_icews(path = p_full, n_max = 5L)
  set_icews_opts(opts)

  expect_equal(nrow(o_def), 0L)
  expect_equal(nrow(o_empty), 0L)
  expect_equal(nrow(o_full), 5L)

  unlink(p_empty)
  unlink(p_full)
  unlink(p_def)
})

test_that("read_icews_raw works", {

  raw_file_dir <- setup_mock_raw(populate = TRUE)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L), NA)

  expect_error(events <- read_icews_raw(raw_file_dir, n_max = 1L, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

  unlink(raw_file_dir, recursive = TRUE)

})


test_that("read from local or db returns same column info and types", {
  p <- setup_mock_environment(pop_raw = TRUE, pop_db = TRUE)

  # read_icews() returns spec_tbl_df, tbl_df, ...; this removes the first
  # class (#63)
  o_raw <- as_tibble(read_icews(p$raw_file_dir, n_max = 5L))
  o_db  <- read_icews(p$db_path, n_max = 5L)
  o_db  <- o_db[, -match("source_file", names(o_db))]

  local_classes <- sapply(o_raw, class)
  db_classes    <- sapply(o_db, class)

  expect_equal(local_classes, db_classes)

  expect_equal(o_db, o_raw)

})

