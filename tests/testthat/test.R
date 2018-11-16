
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


test_that("print options works when options are not set", {

})
