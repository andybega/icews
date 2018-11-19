
library("dataverse")
library("stringr")
library("tools")

icews_doi <- "doi:10.7910/DVN/28075"

dvn_files <- get_dataset(icews_doi)

data_files <- dvn_files$files$label[str_detect(dvn_files$files$label, ".tab")]

outdir <- "~/Dropbox/Work/VForecast/Data/icews_data/raw"

for (file_label in data_files) {
  f <- get_file(file_label, icews_doi)

  if (tools::file_ext(file_label)=="zip") {
    tmp <- tempfile(fileext = ".tab.zip")
    writeBin(as.vector(f), tmp)
    con <- unzip(tmp, exdir = outdir)
  } else {
    fname <- gsub(".zip", "", file_label)
    con <- file.path(outdir, fname)
    writeBin(as.vector(f), con)
  }
}


flights_db <- tbl(con, "flights")

events <- read_tsv(con,
                   col_types = cols(
                     .default = col_character(),
                     `Event ID` = col_integer(),
                     `Event Date` = col_date(format = ""),
                     Intensity = col_double(),
                     `Story ID` = col_integer(),
                     `Sentence Number` = col_integer(),
                     Latitude = col_double(),
                     Longitude = col_double()
                   ))

library(RSQLite)

dbWriteTable(conn=db, name='my_table', value='my_file.csv', sep='\t')

#
#   Figure out which indices to build
#   ____________________________

library("tictoc")

tic(); foo = dbGetQuery(con, "select distinct(source_file) from events;"); toc()


################

# Before syncing to Github
devtools::document()
devtools::load_all()
devtools::test()
devtools::check()
pkgdown::build_site()



"
db_path Path to SQLite database file.
raw_file_dir Directory containing the raw event TSV files.
docs_dir Directory containing the documentation files.
use_db Store events in a SQLite database?
keep_files If using a database, retain raw data TSV files?
"


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





#
#   Playing around with normalization
#   _____________

dir(icews:::find_raw())

events <- icews:::read_events_tsv(file.path(icews:::find_raw(), "events.2017.20180710093300.tab"))
# Add year and yearmonth since these will be useful for getting counts over time
events$year      <- as.integer(format(events$event_date, "%Y"))
events$yearmonth <- as.integer(format(events$event_date, "%Y%m"))
# SQLite does not have date data type, use ISO text instead
events$event_date <- as.integer(format(events$event_date, "%Y%m%d"))
events$source_file  <- "events.2017.20180710093300.tab"

sapply(events, function(x) sum(is.na(x)))

con <- connect()
query_icews("
create table if not exists `events2017` (
    event_id INTEGER NOT NULL,
    event_date TEXT NOT NULL,
    source_name TEXT,
    source_sectors TEXT,
    source_country TEXT,
    event_text TEXT,
    cameo_code TEXT,
    intensity REAL,
    target_name TEXT,
    target_sectors TEXT,
    target_country TEXT,
    story_id INTEGER NOT NULL,
    sentence_number INTEGER,
    publisher TEXT,
    city TEXT,
    district TEXT,
    province TEXT,
    country TEXT,
    latitude REAL,
    longitude REAL,
    year INTEGER,
    yearmonth INTEGER,
    source_file TEXT NOT NULL,
    PRIMARY KEY (event_id, event_date)
    );")

dbListTables(con)
dbWriteTable(con, "events2017", events, append = TRUE)

# table with indices
query_icews("
create table if not exists events2017idx (
            event_id INTEGER NOT NULL,
            event_date TEXT NOT NULL,
            source_name TEXT,
            source_sectors TEXT,
            source_country TEXT,
            event_text TEXT,
            cameo_code TEXT,
            intensity REAL,
            target_name TEXT,
            target_sectors TEXT,
            target_country TEXT,
            story_id INTEGER NOT NULL,
            sentence_number INTEGER,
            publisher TEXT,
            city TEXT,
            district TEXT,
            province TEXT,
            country TEXT,
            latitude REAL,
            longitude REAL,
            year INTEGER,
            yearmonth INTEGER,
            source_file TEXT NOT NULL,
            PRIMARY KEY (event_id, event_date)
);")

dbWriteTable(con, "events2017idx", events, append = TRUE)

idx_cols <- c("source_file", "cameo_code", "country", "year", "yearmonth")
for (x in idx_cols) {
  dbSendQuery(con, sprintf("create index %s on events2017idx(%s)",
                           paste0("events2017_", x, "_idx"), x))
}



# semi-normalized table
query_icews("
create table if not exists events2017norm (
            event_id INTEGER NOT NULL,
            event_date TEXT NOT NULL,
            source_name TEXT,
            source_sectors TEXT,
            source_country TEXT,
            event_text TEXT,
            cameo_code TEXT,
            intensity REAL,
            target_name TEXT,
            target_sectors TEXT,
            target_country TEXT,
            story_id INTEGER NOT NULL,
            sentence_number INTEGER,
            city TEXT,
            district TEXT,
            province TEXT,
            country TEXT,
            latitude REAL,
            longitude REAL,
            year INTEGER,
            yearmonth INTEGER,
            source_file TEXT NOT NULL,
            PRIMARY KEY (event_id, event_date),
            FOREIGN KEY (story_id) REFERENCES stories2017norm(story_id)
);")

query_icews("
create table if not exists stories2017norm (
            story_id INTEGER NOT NULL PRIMARY KEY,
            publisher TEXT);")

dbWriteTable(con, "events2017idx", events, append = TRUE)

idx_cols <- c("source_file", "cameo_code", "country", "year", "yearmonth")
for (x in idx_cols) {
  dbSendQuery(con, sprintf("create index %s on events2017idx(%s)",
                           paste0("events2017_", x, "_idx"), x))
}



microbenchmark(
  dbGetQuery(con, "select count(*) from events2017"),
  dbGetQuery(con, "select count(*) from events2017idx"),
  times = 100
)


#
#   Semi-normalized version
#   _______________________
#
#   Take some of the text columns out, location info, etc.
#
