
test_that("duplicate labels are modified", {
  x <- c("20190308-icews-events.zip", "20190309-icews-events.zip", "20190309-icews-events.zip")
  expect_equal(
    normalize_label(x),
    c("20190308-icews-events.tab", "20190309-icews-events-part1.tab", "20190309-icews-events-part2.tab")
  )
})

