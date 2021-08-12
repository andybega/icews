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
  # check for temporary databases
  temp_db <- db_path %in% c("", ":memory:", "file::memory:")
  if (!temp_db & !file.exists(db_path)) {
    stop(sprintf("Could not find database file at '%s'", db_path))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  con
}

#' Query ICEWS
#'
#' Get results from a query to the database
#'
#' @param query SQL query string
#' @param db_path Path to SQLite database file]
#'
#' @details `query_icews` is a wrapper around [DBI::dbGetQuery()] that will
#' open a connection to the database, submit and return the query results, and
#' then close the database connection.
#'
#' @examples
#' \dontrun{
#' query_icews("SELECT count(*) FROM events;")
#' }
#'
#' @export
#' @md
query_icews <- function(query, db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  res <- DBI::dbGetQuery(con, query)
  res
}

#' Check and if needed setup database
#'
#' @param db_path Path to SQLite database file
#' @keywords internal
check_db_exists <- function(db_path) {
  if (file.exists(db_path)) {
    return(TRUE)
  }
  message(sprintf("Did not find existing database; initialized database at '%s'", db_path))
  create_db(db_path)
  invisible(TRUE)
}


#' Augment and write events to DB
#'
#' Adds some columns to events before writing to DB.
#'
#' @param events data.frame containing ICEWS events
#' @param file Name of the TSV source file from which events came
#' @param db_path Path to SQLite database file
#'
#' @keywords internal
write_data_to_db <- function(events, file, db_path = find_db()) {
  con = connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  # Add year and yearmonth since these will be useful for getting counts over time
  events$year      <- as.integer(format(events$event_date, "%Y"))
  events$yearmonth <- as.integer(format(events$event_date, "%Y%m"))
  # SQLite does not have date data type, use ISO text instead
  events$event_date <- as.integer(format(events$event_date, "%Y%m%d"))
  events$source_file  <- file

  # Weekly file check
  # To ingest weekly files, check and potentially subset event set to avoid
  # adding duplicate events to DB
  # In case there are events in the to-be-added set with a date before the
  # last event date in DB, explicitly check all event IDs to eliminate
  # duplicates
  if (isTRUE(is_weekly_file(file))) {
    max_date_in_db <- query_icews(
      "select max(event_date) from events;",
      db_path = get("db_path", envir = parent.frame()))[[1]]
    # if fresh DB and first time setup, set arbitrary large int date instead of NA
    if (is.na(max_date_in_db)) max_date_in_db <- 21001231

    if (min(events$event_date) <= max_date_in_db) {

      DBI::dbWriteTable(con, "temp", events[, "event_id"], temporary = TRUE)
      safe_ids <- DBI::dbGetQuery(con, "SELECT event_id FROM temp EXCEPT SELECT event_id FROM events;")[[1]]

      # it can be the case that all events are already in DB, if so then
      # update the source file stats table so we don't look at this file again
      # in future updates, then exit the function
      # (updates in 'null_source_files' trigger a 'source_files' table update)
      if (length(safe_ids)==0) {
        addition <- data.frame(name = file)
        DBI::dbWriteTable(con, "null_source_files", addition, append = TRUE)
        return(invisible(TRUE))
      }

      events <- events[events$event_id %in% safe_ids, ]
    }
  }

  DBI::dbWriteTable(con, "events", events, append = TRUE)
  update_stats(db_path)
  invisible(TRUE)
}


#' Ingest a raw events file
#'
#' Ingest a raw data file into the database
#'
#' @param raw_file_path Path to a TSV file with events.
#' @param db_path Path to SQLite database
#'
#' @importFrom DBI dbDisconnect
#'
#' @keywords internal
ingest_from_file <- function(raw_file_path = NULL, db_path = find_db()) {
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
#'
#' @keywords internal
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
#' @keywords internal
delete_events <- function(file, db_path) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  sql <- sprintf("DELETE FROM events WHERE source_file=='%s';", file)
  res <- DBI::dbSendQuery(con, sql)
  DBI::dbClearResult(res)

  update_stats(db_path)
  invisible(TRUE)
}



#' Purge or delete ICEWS database
#'
#' `purge_db` drops the events table in the database. For a complete rebuild
#' it is usually quicker to remove the database file itself with `remove_db`,
#' but this will also delete any user-created tables.
#'
#' @param db_path Path to SQLite database
#'
#' @seealso [purge_data_files()], [burn_it_down()]
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


#' @rdname purge_db
#'
#' @param directory Should the directory be removed also? This will delete any
#' other files that are in it.
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
#' @template quiet
#'
#' @export
sync_db_with_files <- function(raw_file_dir = find_raw(), db_path = find_db(),
                               dryrun = FALSE, quiet = FALSE) {

  plan <- plan_database_sync(db_path, raw_file_dir)

  if (isTRUE(dryrun)) {
    print(plan)
    return(invisible(plan))
  }

  check_db_exists(db_path)
  execute_plan(plan, raw_file_dir = raw_file_dir, db_path = db_path, quiet)

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
#'
#' @keywords internal
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


#' List indices in database
#'
#' List the indices currently in the "event" table in the database.
#'
#' @param db_path Path to SQLite database file
#'
#' @keywords internal
list_indices <- function(db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, "PRAGMA index_list(events);")

  invisible(NULL)
}


#' @keywords internal
update_stats <- function(db_path = find_db()) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  # Update source_files table
  rs <- DBI::dbSendQuery(con, "DELETE FROM source_files;")
  DBI::dbClearResult(rs)
  rs <- DBI::dbSendQuery(con, "INSERT INTO source_files (name) SELECT DISTINCT(source_file) AS name FROM events;")
  DBI::dbClearResult(rs)

  # Update stats table
  rs <- DBI::dbSendQuery(con, "UPDATE stats SET value = ( SELECT count(*) FROM events ) WHERE name=='events_n';")
  DBI::dbClearResult(rs)

  invisible(NULL)
}
