
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

test_that("dr_icews works", {
  expect_error(dr_icews(db_path = "", raw_file_dir = ""), NA)
})


test_that("option setter works", {
  opts <- get_icews_opts()
  old_opts <- unset_icews_opts()

  expect_equal(opts, old_opts)
  expect_null(get_icews_opts()$data_dir)

  set_icews_opts(old_opts)
  expect_equal(opts, old_opts, get_icews_opts())
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


