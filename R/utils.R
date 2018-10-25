
#' Setup ICEWS environment variables
#'
#' Checks for and creates several ICEWS environment variables.
#'
#' @param data_dir Where should the raw TSV data be kept?
#' @param use_db Use a SQLite database to store and work with the data?
#' @param r_environ If TRUE, this will write config parameters to a .Renviron file.
#'
#' @export
setup_icews <- function(data_dir, use_db, r_environ = FALSE) {
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist.")
  }
  if (!use_db %in% c(TRUE, FALSE)) {
    stop("use_db should be TRUE or FALSE")
  }
  Sys.setenv("ICEWS_DATA_DIR" = data_dir)
  Sys.setenv("ICEWS_USE_DB" = use_db)

  if(isTRUE(r_environ)) {
    if (!requireNamespace("usethis", quietly = TRUE)) {
      stop("Package \"usethis\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    cat("Add these lines to the .Renviron file:\n\n")
    cat(sprintf("ICEWS_DATA_DIR=\"%s\"\n", data_dir))
    cat(sprintf("ICEWS_USE_DB=%s\n", use_db))
    cat("\n")
    usethis::edit_r_environ()
  }
  invisible(NULL)
}

#' What's the current state of things?
#'
#' Information on the current setup and any data that is present locally.
#'
#' @param db_path Path to SQLite database
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @export
dr_icews <- function(db_path = NULL, raw_file_dir = NULL) {

  env_is_set <- !is.null(Sys.getenv("ICEWS_DATA_DIR"))

  # Make sure there are enough inputs
  if (!env_is_set & is.null(db_path) & is.null(raw_file_dir)) {
    stop("Environment variables are not set, nor were paths passed. The doctor is **not** in.")
  } else if (!env_is_set & any(is.null(db_path), is.null(raw_file_dir))) {
    stop("Environment variables are not set, but one of the paths was not specified. The doctor is **not** in.")
  } else if (env_is_set & any(!is.null(db_path), !is.null(raw_file_dir))) {
    cat("Found environment variables; disregarding path arguments\n")
  }

  # Check if environment vars are set up
  if (!env_is_set) {
    cat("Did not find ICEWS environment variables, consider running \"?setup_icews\"\n")
    cat(sprintf("Looking for data at:\n  db:  %s\n  raw: %s\n",
                db_path, raw_file_dir))
  } else {
    cat(sprintf("Environment variables are set; looking for data at:\n  db:  %s\n  raw: %s\n",
                file.path(Sys.getenv("ICEWS_DATA_DIR"), "db/icews.sqlite3"),
                file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")))
  }

  if (env_is_set) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
    db_path      <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db/icews.sqlite3")
  }

  Sys.sleep(0.5)
  local_files <- dir(raw_file_dir, pattern = "events[0-9\\.]+.tab")
  cat(sprintf("Found %s local data file(s)\n", length(local_files)))

  db_exists <- file.exists(db_path)
  if (!db_exists) {
    Sys.sleep(0.5)
    cat("Did not find database file; consider running \"sync_db()\" to set up and sync database.\n")
  } else {
    Sys.sleep(0.5)
    cat("Checking database\n")
    con <- connect_to_db(db_path)
    res <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM events;")
    DBI::dbDisconnect(con)
    cat(sprintf("Found %s events", formatC(res[[1]], format="d", big.mark=",")))
  }
  invisible(TRUE)
}

#' Read and combine raw data files
#'
#' Read the entire ICEWS event data into memory. This takes up several (2-3 in 2018) GB.
#'
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @export
#' @import dplyr
#' @importFrom readr read_tsv
#' @importFrom purrr map
read_icews <- function(raw_file_dir = NULL) {
  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }

  data_files <- dir(raw_file_dir, pattern = ".tab", full.names = TRUE)
  col_fmt <- readr::cols(
    .default = col_character(),
    `Event ID` = col_integer(),
    `Event Date` = col_date(format = ""),
    Intensity = col_double(),
    `Story ID` = col_integer(),
    `Sentence Number` = col_integer(),
    Latitude = col_double(),
    Longitude = col_double()
  )
  events <- data_files %>%
    purrr::map(readr::read_tsv, col_types = col_fmt) %>%
    dplyr::bind_rows()
  events
}


#' Query
#'
#' Get results from a query to the database
#'
#' @param query SQL query string
#'
#' @export
query <- function(query) {
  con <- connect_to_db()
  on.exit(DBI::dbDisconnect(con))
  res <- DBI::dbGetQuery(con, query)
  res
}
