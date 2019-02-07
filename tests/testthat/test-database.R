context("Database helper functions")

test_that("check_db works", {

  db_path <- tempfile("test.sqlite3")

  expect_true(grepl("Initializing database", capture.output(check_db_exists(db_path))))
  expect_true(file.exists(db_path))
  expect_true(check_db_exists(db_path))

})

test_that("connect throws error for not existing DB", {
  expect_error(con <- connect("foo"), "Could not find database file")
})

test_that("connect works with in memory test db", {
  expect_error(con <- connect(":memory:"), NA)
  dbDisconnect(con)

  expect_error(con <- connect(""), NA)
  dbDisconnect(con)

  expect_error(con <- connect("file::memory:"), NA)
  dbDisconnect(con)
})

test_that("ingest_from_file works", {

  db_path  <- setup_mock_db(init = TRUE, populate = FALSE)
  tsv_path <- tsv_sample_path()

  expect_error(ingest_from_file(tsv_path, db_path), NA)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  nrows <- DBI::dbGetQuery(con, "select count(*) from events;")[[1]]
  expect_equal(nrows, 4993)

  DBI::dbDisconnect(con)
})

test_that("delete_events works", {
  db_path <- setup_mock_db(init = TRUE, populate = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  expect_error(delete_events("events.2018.sample.tab", db_path), NA)
  nrows <- DBI::dbGetQuery(con, "select count(*) from events;")[[1]]
  expect_equal(nrows, 0)
  DBI::dbDisconnect(con)
})

test_that("purge_db works", {
  db_path <- setup_mock_db(init = TRUE, populate = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  expect_error(purge_db(db_path), NA)
  expect_error(DBI::dbGetQuery(con, "select count(*) from events;"),
               "no such table: events")
  DBI::dbDisconnect(con)
})

test_that("remove_db works", {
  db_path <- setup_mock_db(init = TRUE, populate = FALSE)
  expect_error(remove_db(db_path), NA)
  expect_false(file.exists(db_path))
})

test_that("optimize_db works", {
  db_path <- setup_mock_db(init = TRUE, populate = FALSE)
  expect_error(optimize_db(db_path, vacuum = TRUE, optimize = TRUE), NA)
})
