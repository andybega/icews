#' Download ICEWS data files
#'
#' Download the ICEWS event data from Dataverse
#'
#' @param to_dir Path to directory where data files will be downloaded to.
#' @param update Update files for which a newer version if available? This will
#'   delete the old file version(s). If FALSE, it will download the new version
#'   but leave the old version in place. There thus will be duplicate event sets.
#' @param dryrun Conducts a dry run listing proposed changes, without actually
#'   downloading or deleting anything.
#'
#' @details
#' `download_data()` will check both ICEWS dataverse repos (both the weekly and
#' yearly data repos) and download the data files it finds to the location of
#' the raw data directory ([find_raw()]). By default, with "update = TRUE", it
#' will replace existing files with updated versions if they are available, e.g.
#' if in the raw data directory there is an "events.2017.\[date1\].tab" file
#' but on dataverse there is a "events.2017.\[date2\].tab" file, it will
#' download the new version and delete the old version once that is done.
#'
#' @export
#' @import dataverse
#' @import dplyr
download_data <- function(to_dir = find_raw(), update = TRUE, dryrun = FALSE) {

  cat("Checking dataverse\n")
  plan <- plan_file_changes(to_dir)

  if (!isTRUE(update)) {
    state$action <- ifelse(plan$action=="remove", "none", plan$action)
    state$action <- factor(plan$action, levels = c("none", "download", "remove"))
  }

  if (isTRUE(dryrun)) {
    print(plan)
    return(invisible(plan))
  }

  execute_plan(plan, raw_file_dir = to_dir, db_path = NULL)

  cat("File download/sync done\n")
  invisible(TRUE)
}

#' @rdname download_data
#'
#' @details
#' Use `download_data()` instead of `download_icews()``
#' @export
download_icews <- function(to_dir = find_raw(), update = TRUE, dryrun = FALSE) {
  lifecycle::deprecate_soft("0.2.0", "download_icews()", "download_data()")
  download_data(to_dir, update, dryrun)
}
