
test_that("read_events_tsv works", {

  sample_tsv <- system.file("extdata", "events.2018.sample.tab", package = "icews")

  expect_error(events <- read_events_tsv(sample_tsv), NA)
  expect_true(names(events)[1]=="event_id")

  expect_error(events <- read_events_tsv(sample_tsv, fix_names = FALSE), NA)
  expect_true(names(events)[1]=="Event ID")

})

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
