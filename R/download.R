


#' Download a single file
#'
#' Download a single file from the ICEWS DVN repository and unzip if neccessary
#'
#' @param file Name of the file on DVN to download. If zipped.
#' @param to_dir Destination directory.
#'
#' @details
#' To get a list of files available for download, see [get_dvn_manifest].
#'
#' @return
#' Silently returns the downloaded file name.
#'
#' @import dataverse
#' @importFrom utils unzip
#' @export
#' @md
download_file <- function(file, to_dir) {

  icews_doi <- get_doi()
  f <- get_file(file, icews_doi)

  # Decide how to handle based on whether extraction is needed
  if (tools::file_ext(file)=="zip") {
    tmp <- tempfile(fileext = ".tab.zip")
    writeBin(as.vector(f), tmp)
    con <- utils::unzip(tmp, exdir = to_dir)
  } else {
    fname <- gsub(".zip", "", file)
    con <- file.path(to_dir, fname)
    writeBin(as.vector(f), con)
  }
  return(invisible(con))
}

#' Remove a data file
#'
#' @param raw_file_path Data file to be removed
remove_file <- function(raw_file_path) {
  unlink(raw_file_path)
}

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
#' @export
#' @import dataverse
#' @import dplyr
#' @importFrom rlang .data
download_data <- function(to_dir = find_raw(), update = TRUE, dryrun = FALSE) {


  plan <- plan_file_changes(to_dir)
  path  <- to_dir

  if (!isTRUE(update)) {
    state$action <- ifelse(plan$action=="remove", "none", plan$action)
    state$action <- factor(plan$action, levels = c("none", "download", "remove"))
  }

  if (isTRUE(dryrun)) {
    print_plan(plan)
    return(invisible(plan))
  }

  execute_plan(plan, raw_file_dir = to_dir, db_path = NULL)

  cat("File download/sync done\n")
  invisible(TRUE)
}

#' @rdname download_data
#' @export
download_icews <- function(to_dir = find_raw(), update = TRUE, dryrun = FALSE) {
  .Deprecated("download_data")
  download_data(to_dir, update, dryrun)
}


#' Purge file downloads
#'
#' Removes the downloaded raw event TSV files
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#'
#' @export
purge_data_files <- function(raw_file_dir = find_raw()) {

  data_files <- dir(raw_file_dir, pattern = "events[\\.0-9]+.tab", full.names = TRUE)
  unlink(data_files)
  invisible(NULL)
}

