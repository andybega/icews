
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
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()



"
db_path Path to SQLite database file
raw_file_dir Directory containing the raw event TSV files
use_db Store events in a SQLite database?
keep_files If using a database, retain raw data TSV files?
"
