#' Read ICEWS events into memory
#'
#' Read the entire ICEWS event data into memory. This takes up several (2-3 in 2018) GB.
#'
#' @param path Either path to SQLite database file or raw file directory. If
#'   NULL (default), the global options will be used instead.
#'
#' @seealso [query_icews()], [read_events_tsv()]
#'
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @md
read_icews <- function(path = NULL) {
  # data directory is specified
  if (tools::file_ext(path)==".sqlite3") {
    return(read_icews_db(path))
  }
  if (any(grepl("^events.[0-9]{4}", dir(path)))) {
    return(read_icews_raw(path))
  }
  if (get_icews_opts()$use_db) {
    return(read_icews_db(find_db()))
  }
  read_icews_raw(find_raw())
}


#' Read data from raw files
#'
#' @param raw_file_dir Directory containing the raw event TSV files.
#' @param ... Options passed to [read_events_tsv()].
#'
#' @md
read_icews_raw <- function(raw_file_dir, ...) {
  data_files <- list_local_files()
  events <- data_files %>%
    purrr::map(read_events_tsv, ...) %>%
    dplyr::bind_rows()
  # Add year and yearmonth since these will be useful for getting counts over time
  events$year      <- as.integer(format(events$`Event Date`, "%Y"))
  events$yearmonth <- as.integer(format(events$`Event Date`, "%Y%m"))
  events
}

#' Read data from DB
#'
#' @param db_path Path to SQLite database file.
read_icews_db <- function(db_path) {
  query_icews("SELECT * FROM events;", db_path)
}


