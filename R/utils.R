
#' Contains the DVN DOI for ICEWS
get_doi <- function() {
  "doi:10.7910/DVN/28075"
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

  opts_are_set <- !is.null(getOption("icews.data_dir"))

  # Make sure there are enough inputs
  if (!opts_are_set & is.null(db_path) & is.null(raw_file_dir)) {
    stop("Path variables are not set, nor were paths passed. The doctor is **not** in.")
  } else if (!opts_are_set & any(is.null(db_path), is.null(raw_file_dir))) {
    stop("Path variables are not set, but one of the paths was not specified. The doctor is **not** in.")
  } else if (opts_are_set & any(!is.null(db_path), !is.null(raw_file_dir))) {
    cat("Found path variables in options; disregarding path arguments\n")
  }

  # Check if option vars are set up
  if (!opts_are_set) {
    cat("Did not find ICEWS path variables, consider running \"?setup_icews\"\n")
    cat(sprintf("Looking for data at:\n  db:  %s\n  raw: %s\n",
                db_path, raw_file_dir))
  } else {
    str <- "Path variables are set;"
    if (!getOption("icews.use_db")) {
      str <- paste0(str, "\nFile-based option\nLooking for data at:\n  ", find_raw())
    }
    if (getOption("icews.use_db") & !getOption("icews.keep_files")) {
      str <- paste0(str, "\nDatabase option\nLooking for data at:\n  ", find_db())
    }
    if (getOption("icews.use_db") & getOption("icews.keep_files")) {
      str <- paste0(
        str,
        sprintf("\nDatabase option, but keep files as well\nLooking for data at:\n  db:  %s\n  raw: %s",
                find_db(), find_raw()))
    }
  }

  if (opts_are_set) {
    raw_file_dir <- find_raw()
    db_path      <- find_db()
  }

  Sys.sleep(0.5)
  local_files <- dir(raw_file_dir, pattern = "events[0-9\\.]+.tab")
  cat(sprintf("Found %s local data file(s)\n", length(local_files)))

  db_exists <- file.exists(db_path)
  if (!db_exists) {
    Sys.sleep(0.5)
    cat("Did not find database file; consider running \"update()\" to set up and sync database.\n")
  } else {
    Sys.sleep(0.5)
    cat("Checking database\n")

    # Check if uningested local files
    if (length(local_files) > 0) {
      db_source_files <- list_source_files(db_path)
      if (any(!local_files %in% db_source_files)) {
        cat("There are local files that have not been ingested into the database,\nconsider running `update()` or `sync_db_with_files()`\n")
      } else {
        cat("Looks good")
      }
    }

    res <- query_icews("SELECT COUNT(*) FROM events;")
    cat(sprintf("Found %s events in database", formatC(res[[1]], format="d", big.mark=",")))
  }
  invisible(TRUE)
}






#' Remove all data
#'
#' Remove all ICEWS data
#'
#' @param db_path Path to SQLite database
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @importFrom utils menu
#' @export
burn_it_down <- function(db_path = find_db(), raw_file_dir = find_raw()) {

  a <- menu(title = "Are you crazy?", choices = c("Yes", "Trust me, I know what I'm doing"))
  if (a!=2) {
    cat("I can't let you do this, bye.\n")
    return(invisible(NULL))
  }
  b <- menu(title = "This will delete all local .tab files, and delete the database. Are you sure?",
            choices = c("Hmm, actually, nope.", "Do it. Just do it."))
  if (b!=2) {
    cat("You had me worried.\n")
    return(invisible(NULL))
  }

  cat("Deleting database\n")
  remove_db(db_path)
  cat("Purging raw data files\n")
  purge_data_files(raw_file_dir)
  unlink(raw_file_dir, recursive = TRUE)
  cat("Unsetting option variables\n")
  options(icews.data_dir   = NULL)
  options(icews.use_db     = NULL)
  options(icews.keep_files = NULL)
  cat("If you added them to .Rprofile, remove there as well\n")

  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("Package \"usethis\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    cat("Remove all \"icews.\" options if they are there.\n")
    usethis::edit_r_profile()
  }

  cat("It is done\n")
}



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
  if (any(grepl("^events.[0-9]{4}"))) {
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
  data_files <- list_raw_files()
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


