

library("icews")
library("dplyr")
library("pryr")

sample_events <- query_icews("SELECT * FROM events WHERE yearmonth==201806;") %>%
  mutate(source_file = NULL,
         event_date = as.Date(as.character(event_date), "%Y%m%d")) %>%
  filter(event_date < "2018-06-04")

object_size(sample_events)

icews_sample <- as_tibble(sample_events)

usethis::use_data(icews_sample, overwrite = TRUE)
