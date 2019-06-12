#' @rdname state
#'
#' @param db_path Path to SQLite database files.
#'
#' @export
get_db_state <- function(db_path = find_db()) {
  db_files <- list_source_files(db_path)
  state <- parse_label(db_files)
  state$label <- state$is_data <- NULL
  colnames(state) <- paste0("db_", colnames(state))
  state
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
  source_files <- DBI::dbGetQuery(con, "SELECT name FROM source_files;")
  source_files$name
}
