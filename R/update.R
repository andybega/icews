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
#' @details The behavior of `update_icews` depends on the ICEWS option values,
#' as set via `setup_icews`, or alternatively manually specified function
#' arguments. The intended use is with options set in .Rprofile.
#'
#' If "use_db" and "keep_files" are both true, data files will be downloaded to
#' the "raw" directory in the root ICEWS directory
#' (`get_icews_opts()$data_dir`), then ingested into a SQLite database in the
#' "db" directory. Each data file is ingested before the next one is downloaded,
#' so that if the process is interrupted there will be at least some data in the
#' database.
#'
#' If "use_db" is set with "keep_files" as false, data is downloaded to
#' temporary files before ingestion. This saves about 5GB of space as of late
#' 2018.
#'
#' If "use_db" is false, then regardless of the "keep_files" value, only the
#' raw data files will be downloaded. This is equivalent to [download_data()].
#'
#' You can manually replicate what `update_icews()` does by calling
#' [download_data()], followed by [sync_db_with_files()].
#'
#' @examples
#' \dontrun{
#' # assuming this is the first time the package is used:
#' setup_icews("path/to/icews_data", use_db = TRUE, keep_files = TRUE,
#'             r_profile = TRUE)
#' }
#'
#' # see the plan for the initial download
#' # update_icews(dryrun = TRUE)
#'
#' # do the initial download; this will take a while (1hr or so)
#' # update_icews(dryrun = FALSE)
#'
#' # call it again in the future to check if updates are needed
#'
#' @export
#' @md
update_icews <- function(dryrun = TRUE,
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
    print(plan)
    return(invisible(plan))
  }

  execute_plan(plan, raw_file_dir = raw_file_dir, db_path = db_path)

  cat("File and/or database update done\n")
  invisible(plan)
}
