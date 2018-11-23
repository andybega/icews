
stop()

library("devtools")
library("usethis")
library("pkgdown")

# Before syncing to Github
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()


check_win()
check_win_devel()


#
#   Figure out which indices to build
#   ____________________________

library("tictoc")

tic(); foo = dbGetQuery(con, "select distinct(source_file) from events;"); toc()


################





#
#   Make sure all TSV files are correctly parsed
#   _____________________________________________

tsv_files <- dir(find_raw(), full.names = TRUE)
for (f in tsv_files) {
  events <- readr::read_tsv(
    f,
    col_types = cols(
      .default = col_character(),
      `Event ID` = col_integer(),
      `Event Date` = col_date(format = ""),
      Intensity = col_double(),
      `Story ID` = col_integer(),
      `Sentence Number` = col_integer(),
      Latitude = col_double(),
      Longitude = col_double()
    ),
    # quotes are already escaped, so don't try to escape again
    quote = "")
  str <- read_lines(f)
  if (nrow(events)!=(length(str)-1)) {
    cat(basename(f), "\n")
  }
}


#
#   Check records in DB
#   ___________________

events_by_ym <- query("select yearmonth, count(*) as n from events group by yearmonth;")
events_by_ym %>%
  mutate(date = as.Date(paste0(yearmonth, "01"), "%Y%m%d")) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  labs(x = "Date", y = "Events") +
  ggtitle("ICEWS events per month") +
  theme_minimal()





