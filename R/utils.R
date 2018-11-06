
#' Setup ICEWS environment variables
#'
#' Checks for and creates several ICEWS environment variables.
#'
#' @param data_dir Where should the raw TSV data be kept?
#' @param use_db Store events in a SQLite database?
#' @param keep_files keep_files If using a database, retain raw data TSV files?
#' @param r_profile If TRUE, this will write config parameters to a .Renviron file.
#'
#' @export
setup_icews <- function(data_dir, use_db = TRUE, keep_files = FALSE, r_profile = TRUE) {
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist.")
  }
  if (!use_db %in% c(TRUE, FALSE)) {
    stop("use_db should be TRUE or FALSE")
  }

  options(icews.data_dir   = data_dir)
  options(icews.use_db     = use_db)
  options(icews.keep_files = keep_files)

  # Create folders as neccessary
  if (use_db & !dir.exists(find_db())) {
    dir.create(find_db())
  }
  if ((!use_db | keep_files) & !dir.exists(find_raw())) {
    dir.create(find_raw())
  }

  if(isTRUE(r_profile)) {
    if (!requireNamespace("usethis", quietly = TRUE)) {
      stop("Package \"usethis\" needed for this function to work. Please install it (recommended) or set 'r_environ = FALSE'.",
           call. = FALSE)
    }
    cat("Add these lines to the .Rprofile file:\n\n")
    cat("# ICEWS data location and options\n")
    cat(sprintf("options(icews.data_dir   = \"%s\")\n", data_dir))
    cat(sprintf("options(icews.use_db     = %s)\n", use_db))
    cat(sprintf("options(icews.keep_files = %s)\n", keep_files))
    cat("\n")
    usethis::edit_r_profile()
  }
  cat("Path options are set\n")
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

  opts_are_set <- !is.null(getOption("icews.data_dir"))

  # Make sure there are enough inputs
  if (!opts_are_set & is.null(db_path) & is.null(raw_file_dir)) {
    stop("Path variables are not set, nor were paths passed. The doctor is **not** in.")
  } else if (!opts_are_set & any(is.null(db_path), is.null(raw_file_dir))) {
    stop("Path variables are not set, but one of the paths was not specified. The doctor is **not** in.")
  } else if (opts_are_set & any(!is.null(db_path), !is.null(raw_file_dir))) {
    cat("Found path variables in options; disregarding path arguments\n")
  }

  # Check if environment vars are set up
  if (!opts_are_set) {
    cat("Did not find ICEWS path variables, consider running \"?setup_icews\"\n")
    cat(sprintf("Looking for data at:\n  db:  %s\n  raw: %s\n",
                db_path, raw_file_dir))
  } else {
    cat(sprintf("Path variables are set; looking for data at:\n  db:  %s\n  raw: %s\n",
                file.path(find_db()),
                file.path(find_raw())))
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
    res <- query("SELECT COUNT(*) FROM events;")
    cat(sprintf("Found %s events", formatC(res[[1]], format="d", big.mark=",")))
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

  stop("needs to be checked after refactor")

  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }
  if (is.null(db_path)) {
    db_path <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "db/icews.sqlite3")
  }

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

  cat("Puring database\n")
  purge_db(db_path)
  cat("Deleting database\n")
  unlink(db_path)
  cat("Purging raw data files\n")
  purge_data_files(raw_file_dir)
  cat("Unsetting environment variables\n")
  Sys.unsetenv("ICEWS_DATA_DIR")
  Sys.unsetenv("ICEWS_DATA_DIR")
  cat("If you added them to .Renviron, remove there as well\n")

  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("Package \"usethis\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    cat("Remove ICEWS_DATA_DIR and ICEWS_USE_DB if they are there.\n")
    usethis::edit_r_environ()
  }

  cat("It is done\n")
}


#' Normalize paths
#'
#' This takes care of finding paths when the environment variable is set and
#' paths are at the default NULL values.
#'
#' @param db_path Location of database SQLite file
#' @param raw_file_dir Directory for raw data files
#'
normalize_paths <- function(db_path, raw_file_dir) {
  # Check that both are NULL or not, but not a mix
  if (xor(is.null(db_path), is.null(raw_file_dir))) {

  }
  # Use user-supplied paths
  if (!is.null(db_path) & !is.null(raw_file_dir)) {

  }
  # Use environment settings
  if (is.null(db_path) & is.null(raw_file_dir)) {

  }
}
