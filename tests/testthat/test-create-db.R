
library("DBI")

test_that("create_db works", {

  db_path <- tempfile("test.sqlite3")

  create_db(db_path)

  con <- connect(db_path)
  expect_true("events" %in% DBI::dbListTables(con))

  DBI::dbDisconnect(con)
})
