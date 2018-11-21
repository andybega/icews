#' Create a local ICEWS database
#'
#' Initialize a local ICEWS database.
#'
#' @param db_path If NULL and the ICEWS_DATA_DIR environment variable is set,
#'   the database will be created at ICEWS_DATA_DIR/db/icews.sqlite3. Otherwise,
#'   an alternative path for the database file.
#'
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

  # Create events table and indices
  create_event_table(db_path)

  # Create other tables
  execute_sql("stats.sql", db_path)
  execute_sql("source_files.sql", db_path)

  # return SQLiteConnection from RSQLite
  invisible(con$con)
}

#' Create event table and indices
#'
#' @template dbp
create_event_table <- function(db_path) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  execute_sql("events.sql", db_path)

  # Create indices
  idx_columns <- c("source_file", "cameo_code", "country", "year",
                   "yearmonth")
  idx_names <- paste0("events_", gsub(" ", "_", tolower(idx_columns)), "_idx")
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
