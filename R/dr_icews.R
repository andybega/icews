#' What's the current state of things?
#'
#' Information on the current setup and any data that is present locally.
#'
#' @template dbp
#' @template rfd
#'
#' @export
dr_icews <- function(db_path = NULL, raw_file_dir = NULL) {

  opts_are_set <- !is.null(getOption("icews.data_dir"))

  # Make sure there are enough inputs
  if (!opts_are_set & is.null(db_path) & is.null(raw_file_dir)) {
    stop("Path variables are not set, nor were paths passed. The doctor is **not** in.")
  }

  # check option status
  cat(sprintf("Checking ICEWS options...%s\n",
              ifelse(opts_are_set, "options are set", "")))
  opts <- get_icews_opts()
  msg <- format(opts)
  cat(paste0(c(msg, ""), collapse = "\n"))

  # regardless of if options are set, use one or both paths if they are set
  if (opts_are_set & any(!is.null(db_path), !is.null(raw_file_dir))) {
    cat("One or both path arguments are not NULL, disregarding option values\n")
  }

  # flag for running with user-specified path arguments
  man_mode <- any(!is.null(db_path), !is.null(raw_file_dir))

  # if not man_mode fall back on default opts settings
  if (!man_mode) {
    db_path      <- find_db()
    raw_file_dir <- find_raw()
  }

  # Check local files
  cat("Check local file status...\n")
  if (is.null(raw_file_dir)) {

    cat("Skip: options are not set and/or raw_file_dir = NULL\n")

  } else if (!man_mode & isTRUE(opts$use_db) & isFALSE(opts$keep_files)) {

    cat("Skip: using database-only backend\n")

  } else {

    cat(sprintf("Looking for data at: '%s'\n", raw_file_dir))
    local_state <- get_local_state(raw_file_dir)
    cat(sprintf("Found %s local data file(s)\n", nrow(local_state)))

  }

  # Check database
  cat("Check database status...\n")
  if (is.null(db_path)) {

    cat("Skip: options are not set and/or db_path = NULL\n")

  } else if (!man_mode & isFALSE(opts$use_db)) {

    cat("Skip: using database-only backend\n")

  } else {

    cat(sprintf("Looking for database at: '%s'\n", db_path))

    # Check if DB file exists, if so check status
    if (!file.exists(db_path)) {

      cat("Did not find database file; consider running \"update_icews()\" to set up and sync database.\n")

    } else {

      # Check if uningested local files
      # first, make sure local_files exists to avoid error
      if (!exists("local_state")) {
        local_state <- data.frame(NULL)
      }
      if (nrow(local_state) > 0) {
        db_source_files <- list_source_files(db_path)
        file_diffs <- setdiff(db_source_files, local_state$file_name)
        if (length(file_diffs) > 0) {
          cat("Local files and database source files don't match, consider running\n`update()` or `sync_db_with_files()`\n")
        }
      }

      res <- query_icews("SELECT value FROM stats WHERE name = 'events_n';")$value
      cat(sprintf("Found %s events in database", formatC(res[[1]], format="d", big.mark=",")))

    }

  }

  invisible(NULL)
}
