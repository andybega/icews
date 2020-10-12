
library("icews")
library("readr")
library("dplyr")
library("pryr")

rfd <- find_raw()

local_files <- dir(rfd, full.names = TRUE)
events2018 <- local_files[grepl("\\.2018\\.", local_files)]

events2018 <- read_tsv(events2018)

object_size(events2018)

events_sample <- events2018 %>%
  filter(`Event Date` > "2018-05-31" & `Event Date` < "2018-06-03")

object_size(events_sample)

write_tsv(events_sample, "inst/extdata/events.2018.sample.tab")
