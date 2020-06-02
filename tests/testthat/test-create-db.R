
library("DBI")

test_that("create_db works", {

  db_path <- tempfile("test.sqlite3")

  create_db(db_path)

  con <- connect(db_path)
  expect_true("events" %in% DBI::dbListTables(con))

  DBI::dbDisconnect(con)
})

test_that("create_db creates dir if it does not exist", {

  db_path <- file.path(tempdir(), "new_db")
  expect_error(create_db(db_path), NA)
  unlink(db_path)

})

test_that("create_db will error if db already exists", {

  db_path <- file.path(tempdir(), "new_db2")
  create_db(db_path)
  expect_error(create_db(db_path), "Database already exists")
  unlink(db_path)

})
