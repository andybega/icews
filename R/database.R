
#' Create a local ICEWS database
#'
#' Initialize a local ICEWS database.
#'
#' @param db_path If NULL and the ICEWS_DATA_DIR environment variable is set,
#'   the database will be created at ICEWS_DATA_DIR/db/icews.sqlite3. Otherwise,
#'   an alternative path for the database file.
#'
#' @export
#' @import dplyr
create_db <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db")
    if (!dir.exists(db_dir)) {
      dir.create(db_dir)
    }
    db_path <- file.path(db_dir, "icews.sqlite3")
  }

  dplyr::src_sqlite(db_path, create = TRUE)
  invisible(TRUE)
}

#' Connect to local ICEWS database
#'
#' Create a DB connection for the local ICEWS database.
#'
#' @param db_path Path to SQLite database
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
connect_to_db <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db")
    if (!dir.exists(db_dir)) {
      dir.create(db_dir)
    }
    db_path <- file.path(db_dir, "icews.sqlite3")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  con
}

#' Purge ICEWS database
#'
#' Delete the events table
#'
#' @param db_path Path to SQLite database
#'
#' @export
purge_db <- function(db_path = NULL) {
  con <- connect_to_db(db_path)
  on.exit(DBI::dbDisconnect(con))

  res <- DBI::dbSendQuery(con, "DROP TABLE events;")
  DBI::dbClearResult(res)
  res <- DBI::dbSendQuery(con, "VACUUM;")
  DBI::dbClearResult(res)
}

#' Sync database and raw files
#'
#' Synchronize database contents with local raw data files.
#'
#' @param db_path Path to SQLite database
#' @param raw_file_dir Directory containing the raw event TSV files.
#' @param dryrun List file changes to be performed without taking any action.
#'
#' @export
#' @importFrom DBI dbGetQuery dbListTables
sync_db_with_files <- function(db_path = NULL, raw_file_dir = NULL,
                               dryrun = FALSE) {

  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }
  if (is.null(db_path)) {
    db_path <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db/icews.sqlite3")
  }

  con <- connect_to_db(db_path)
  events_exists <- ("events") %in% DBI::dbListTables(con)
  if (events_exists) {
    query <- "SELECT DISTINCT(source_file) FROM events;"
    ingested_files <- DBI::dbGetQuery(con, query)$source_file
  } else {
    ingested_files <- vector("character", 0L)
  }
  dbDisconnect(con)

  local_files <- dir(raw_file_dir, pattern = "events[0-9\\.]+.tab")
  need_to_ingest <- local_files[!local_files %in% ingested_files]

  if (isTRUE(dryrun)) {
    cat(sprintf("Need to ingest %s files\n", length(need_to_ingest)))
    return(invisible(NULL))
  }

  for (file_name in need_to_ingest) {
    cat(sprintf("Ingesting %s\n", file_name))
    raw_file_path <- file.path(raw_file_dir, file_name)
    ingest_raw_data_file(raw_file_path, db_path)
  }

  # DB maintenance
  con <- connect_to_db(db_path)
  on.exit(DBI::dbDisconnect(con))

  # Create/update indices
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS event_id_idx    ON events(`Event ID`);")
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS source_file_idx ON events(source_file);")
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS cameo_code_idx  ON events(`CAMEO Code`);")
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS country_idx     ON events(Country);")
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS year_idx        ON events(year);")
  DBI::dbSendQuery(con, "CREATE INDEX IF NOT EXISTS yearmonth_idx   ON events(yearmonth);")

  # Housekeeping
  DBI::dbSendQuery(con, "VACUUM;")
  DBI::dbSendQuery(con, "PRAGMA optimize;")

  invisible(NULL)
}

#' Ingest a raw events file
#'
#' Ingest a raw data file into the database
#'
#' @param raw_file_path Directory containing the raw event TSV files.
#' @param db_path Path to SQLite database
#'
#' @import readr
#' @importFrom DBI dbDisconnect
ingest_raw_data_file <- function(raw_file_path, db_path) {
  con <- connect_to_db(db_path)
  on.exit(DBI::dbDisconnect(con))

  events <- readr::read_tsv(raw_file_path,
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
  # Add year and yearmonth since these will be useful for getting counts over time
  events$year      <- as.integer(format(events$`Event Date`, "%Y"))
  events$yearmonth <- as.integer(format(events$`Event Date`, "%Y%m"))
  # SQLite does not have date data type, use ISO text instead
  events$`Event Date` <- as.character(events$`Event Date`)
  events$source_file <- basename(raw_file_path)
  DBI::dbWriteTable(con, "events", events, append = TRUE)
  invisible(TRUE)
}

#' Synchronize/update local ICEWS database
#'
#' Maintain a current set of ICEWS events in the local database that match
#' the latest versions on DVN. If needed, create the database and download data
#' files.
#'
#' @param db_path Path to SQLite database
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @export
sync_db <- function(db_path = NULL, raw_file_dir = NULL) {

  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }
  if (is.null(db_path)) {
    db_path <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db/icews.sqlite3")
  }

  cat("Checking whether files need to be downloaded/updated\n\n")
  Sys.sleep(.5)

  download_icews(raw_file_dir)

  cat("Checking whether DB is set up\n\n")
  Sys.sleep(.5)

  if (!file.exists(db_path)) {
    cat("Creating DB")
    create_db(db_path)
  }

  cat("Checking whether files need to be ingested to DB")
  Sys.sleep(.5)

  # Workaround until proper sync
  con <- connect_to_db()
  if ("events" %in% DBI::dbListTables(con)) {
    purge_db(db_path)
  }
  DBI::dbDisconnect(con)
  sync_db_with_files(db_path, raw_file_dir)

}


