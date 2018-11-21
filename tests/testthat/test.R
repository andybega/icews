
context("Misc tests")

test_that("column names are normalized", {
  df <- data.frame(
    `Event ID` = 1:5,
    `Source Actor` = 1:5,
    `yearmonth` = 1:5,
    check.names = FALSE
  )
  expect_equal(colnames(normalize_column_names(df)),
               c("event_id", "source_actor", "yearmonth"))
})


test_that("option setter works", {
  opts <- get_icews_opts()
  old_opts <- unset_icews_opts()

  expect_equal(opts, old_opts)
  expect_null(get_icews_opts()$data_dir)

  set_icews_opts(old_opts)
  expect_equal(opts, old_opts, get_icews_opts())
})

test_that("state functions respect path arguments", {
  # there should be nothing at ~/
  expect_equal(nrow(get_local_state("~/")), 0)
})


test_that("find_path works", {
  opts <- get_icews_opts()
  set_icews_opts(x = "~/icews_data", use_db = TRUE, keep_files = TRUE)

  expect_error(find_raw(), NA)
  expect_error(find_db(), NA)
  expect_error(find_docs(), NA)

  set_icews_opts(x = NULL, use_db = TRUE, keep_files = TRUE)

  expect_error(find_path("raw"))

  set_icews_opts(opts)
})


test_that("get_doi works", {
  expect_error(get_doi(), NA)
})


test_that("execute_sql works", {
  test_str <- "create table test1 (col1 integer);\n\ncreate table test2 (col2 text);"
  out <- read_sql_statements(test_str)
  expect_length(out, 2)

  test_str <- "-- comment\n\ncreate table test1 (col1 integer);\n\ncreate table test2 (col2 text);"
  out <- read_sql_statements(test_str)
  expect_length(out, 2)

  sql <- c("create table test1 (col1 integer);", "create table test2 (col2 text);")
  expect_error(execute_sql_statements(sql, ":memory:"), NA)

  expect_error(execute_sql("events.sql", ":memory:"), NA)
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




context("data helpers")

test_that("gw code mapping works", {
  df <- dplyr::tibble(
    country = c("Serbia", "Serbia"),
    event_date = as.Date(c("2006-06-04", "2006-06-05")),
    stringsAsFactors = FALSE)
  df$gwcode <- icews_to_gwcode(df$country, df$event_date)
  expect_equal(df$gwcode, c(345L, 340L))

  df <- dplyr::tibble(
    country = c("United States", "Puerto Rico", "Guam"),
    event_date = rep(as.Date("2018-01-01")),
    stringsAsFactors = FALSE)
  df$gwcode <- icews_to_gwcode(df$country, df$event_date)
  expect_equal(df$gwcode, rep(2L, 3))
})


