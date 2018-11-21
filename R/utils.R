
#' Remove all data
#'
#' Remove all local ICEWS data artifacts, i.e. local data files and/or database
#' file.
#'
#' @param db_path Path to SQLite database
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @seealso [purge_data_files()], [purge_db()], [remove_db()]
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

