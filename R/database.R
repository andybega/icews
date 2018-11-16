
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
create_db <- function(db_path = find_db()) {
  # Make sure we are not overwriting existing database
  if (file.exists(db_path)) {
    stop(sprintf("Database already exists at '%s'", db_path))
  }

  if (!dir.exists(dirname(db_path))) {
    dir.create(dirname(db_path))
  }
  con <- dplyr::src_sqlite(db_path, create = TRUE)
  invisible(con)
}

#' Connect to local ICEWS database
#'
#' Create a DB connection for the local ICEWS database.
#'
#' @param db_path Path to SQLite database file
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
connect <- function(db_path = find_db()) {
  if (!file.exists(db_path)) {
    stop(sprintf("Could not find database file at '%s'", db_path))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  con
}

#' Query
#'
#' Get results from a query to the database
#'
#' @param query SQL query string
#' @param db_path Path to SQLite database file
#'
#' @export
query_icews <- function(query, db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  res <- DBI::dbGetQuery(con, query)
  res
}

#' Create event table and indices
#'
#' @param db_path Path to SQLite database file
create_event_table <- function(db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  sql <- "CREATE TABLE IF NOT EXISTS `events` (
    event_id INTEGER,
    event_date TEXT,
    source_name TEXT,
    source_sectors TEXT,
    source_country TEXT,
    event_text TEXT,
    cameo_code TEXT,
    intensity REAL,
    target_name TEXT,
    target_sectors TEXT,
    target_country TEXT,
    story_id INTEGER,
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
    source_file TEXT
  );"
  res <- DBI::dbSendQuery(con, sql)
  DBI::dbClearResult(res)

  # Create indices
  idx_columns <- c("event_id", "source_file", "cameo_code", "country", "year",
                   "yearmonth")
  idx_names <- paste0(gsub(" ", "_", tolower(idx_columns)), "_idx")
  existing_indices <- list_indices(db_path)$name
  need_cols <- idx_columns[!idx_names %in% existing_indices]
  if (length(need_cols) > 0) {
    #cat("Building indices\n")
    need_names <- idx_names[!idx_names %in% existing_indices]
    for (i in 1:length(need_cols)) {
      #cat(sprintf("Creating index for '%s'\n", need_cols[i]))
      sql <- sprintf("CREATE INDEX IF NOT EXISTS %s ON events(`%s`);", need_names[i], need_cols[i])
      res <- DBI::dbSendQuery(con, sql)
      DBI::dbClearResult(res)
    }
  }
  invisible(NULL)
}


#' Check and if needed setup database
#'
#' @param db_path Path to SQLite database file
check_db_exists <- function(db_path) {
  if (file.exists(db_path)) {
    return(TRUE)
  }
  create_db(db_path)
  create_event_table(db_path)
  invisible(TRUE)
}


#' Augment and write events to DB
#'
#' Adds some columns to events before writing to DB.
#'
#' @param events data.frame containing ICEWS events
#' @param file Name of the TSV source file from which events came
#' @param db_path Path to SQLite database file
write_data_to_db <- function(events, file, db_path = find_db()) {
  con = connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  # Add year and yearmonth since these will be useful for getting counts over time
  events$year      <- as.integer(format(events$event_date, "%Y"))
  events$yearmonth <- as.integer(format(events$event_date, "%Y%m"))
  # SQLite does not have date data type, use ISO text instead
  events$event_date <- as.character(events$event_date)
  events$source_file  <- file

  DBI::dbWriteTable(con, "events", events, append = TRUE)
  invisible(TRUE)
}


#' Ingest a raw events file
#'
#' Ingest a raw data file into the database
#'
#' @param raw_file_path Directory containing the raw event TSV files.
#' @param db_path Path to SQLite database
#'
#' @importFrom DBI dbDisconnect
ingest_from_file <- function(raw_file_path = find_raw(), db_path = find_db()) {
  events <- read_events_tsv(raw_file_path, fix_names = TRUE)
  write_data_to_db(events, basename(raw_file_path), db_path)
  invisible(TRUE)
}


#' Ingest a file without retaining
#'
#' The "in memory" part is not working because I haven't figured out how to
#' unzip a raw vector in memory.
#'
#' @param file The normalized filename, e.g. "events.1995.[...].tab"
#' @param db_path Path to SQLite database file
ingest_from_memory <- function(file, db_path) {
  file <- download_file(file, to_dir = tempdir())
  ingest_from_file(file, db_path)
  invisible(TRUE)
}



#' Delete events associated with a file
#'
#' @param file The normalized filename, e.g. "events.1995.[...].tab", without
#' ".zip" ending.
#' @param db_path Path to SQLite database file
#'
#' @seealso [purge_db()], [delete_events()]
#'
#' @md
delete_events <- function(file, db_path) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  sql <- sprintf("DELETE FROM events WHERE source_file=='%s';", file)
  res <- DBI::dbSendQuery(con, sql)
  DBI::dbClearResult(res)
  invisible(TRUE)
}



#' Purge ICEWS database
#'
#' Delete the events table. For a complete rebuild it is quicker to delete
#' the whole database file.
#'
#' @param db_path Path to SQLite database
#'
#' @seealso [remove_db()]
#'
#' @export
#' @md
purge_db <- function(db_path = NULL) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  res <- DBI::dbSendQuery(con, "DROP TABLE events;")
  DBI::dbClearResult(res)
  res <- DBI::dbSendQuery(con, "VACUUM;")
  DBI::dbClearResult(res)
}


#' Remove database file
#'
#' Removes the SQLite database file. This is quicker than deleting the events
#' table.
#'
#' @param db_path Path to SQLite database file.
#' @param directory Should the directory be removed also? This will delete any
#' other files that are in it.
#'
#' @seealso [purge_db()], [delete_events()]
#'
#' @export
#' @md
remove_db <- function(db_path, directory = FALSE) {
  unlink(db_path)
  if (directory) {
    unlink(dirname(db_path), recursive = TRUE)
  }
  invisible(NULL)
}


#' Synchronize DB with raw files
#'
#' Synchronize DB with any local files found, without downloading new files.
#'
#' @param db_path Path to SQLite database file
#' @param raw_file_dir Directory containing the raw event TSV files.
#' @param dryrun List changes to be performed without taking any action.
sync_db_with_files <- function(raw_file_dir = find_raw(), db_path = find_db(),
                               dryrun = FALSE) {

  plan <- plan_database_sync(db_path, raw_file_dir)

  if (isTRUE(dryrun)) {
    print_plan(plan)
    return(invisible(plan))
  }

  check_db_exists(db_path)
  execute_plan(plan, raw_file_dir = raw_file_dir, db_path = db_path)

  cat("File and/or database update done\n")
  invisible(TRUE)
}


#' Update database and files
#'
#' Maintain a current set of ICEWS events in the local database that match
#' the latest versions on DVN. If needed, create the database and download data
#' files.
#'
#' @param dryrun Just list changes that would be made, without making them.
#' @param use_db Store events in a SQLite database?
#' @param keep_files If using a database, retain raw data TSV files?
#' @param db_path Path to SQLite database file
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @export
update_icews <- function(dryrun = FALSE,
                   use_db     = getOption("icews.use_db"),
                   keep_files = getOption("icews.keep_files"),
                   db_path = find_db(), raw_file_dir = find_raw()) {

  # Check input
  # dryrun
  if (!is.logical(dryrun) | is.na(dryrun)) {
    stop("dryrun argument should be TRUE or FALSE")
  }

  # use_db
  if (!is.logical(use_db) | is.na(use_db)) {
    stop("use_db argument should be TRUE or FALSE")
  }
  if (is.null(use_db)) {
    stop("Option \"icews.use_db\" is not set, consider running `setup_icews()`\n?setup_icews")
  }

  # keep_files
  if (!is.logical(keep_files) | is.na(keep_files)) {
    stop("keep_files argument should be TRUE or FALSE")
  }
  if (is.null(keep_files)) {
    stop("Option \"icews.keep_files\" is not set, consider running `setup_icews()`\n?setup_icews")
  }

  # Check if "/raw" should/does exist
  if (!dir.exists(raw_file_dir) & (keep_files | !use_db)) {
    dir.create(raw_file_dir)
  }


  # Determine action plan based on DB and file options
  if (!use_db) {
    plan <- plan_file_changes(raw_file_dir)
  } else {
    # use a database
    check_db_exists(db_path)
    plan <- plan_database_changes(db_path, raw_file_dir, keep_files, use_local = TRUE)
  }

  if (isTRUE(dryrun)) {
    print_plan(plan)
    return(invisible(plan))
  }

  execute_plan(plan, raw_file_dir = raw_file_dir, db_path = db_path)

  cat("File and/or database update done\n")
  invisible(TRUE)
}

#' Optimize database
#'
#' Call SQLite's database optimizer and vacuum
#'
#' @param db_path Path to database file
#' @param vacuum Call "VACUUM" command?
#' @param optimize Call "PRAGMA optimize"?
optimize_db <- function(db_path, vacuum = TRUE, optimize = TRUE) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  # Housekeeping
  if (vacuum) {
    res <- DBI::dbSendQuery(con, "VACUUM;")
    DBI::dbClearResult(res)
  }
  if (optimize) {
    res <- DBI::dbSendQuery(con, "PRAGMA optimize;")
    DBI::dbClearResult(res)
  }
  invisible(NULL)
}


#' List of ingested source files
#'
#' List the source files from which events have been ingested to the DB already.
#'
#' @param db_path Path to SQLite database file
#'
#' @return A character vector of source file names.
#'
#' @export
list_source_files <- function(db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  source_files <- DBI::dbGetQuery(con, "SELECT DISTINCT(source_file) FROM events;")
  source_files$source_file
}


#' List indices in database
#'
#' List the indices currently in the "event" table in the database.
#'
#' @param db_path Path to SQLite database file
list_indices <- function(db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "PRAGMA index_list(events);")
}

