


#' Download a single file from DVN
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
#' @seealso [purge_db()], [remove_db()], [burn_it_down()]
#'
#' @export
purge_data_files <- function(raw_file_dir = find_raw()) {

  data_files <- dir(raw_file_dir, pattern = "events[\\.0-9]+.tab", full.names = TRUE)
  unlink(data_files)
  invisible(NULL)
}

#' Read events TSV
#'
#' Wrapper around [readr::read_tsv()] to read in the raw events tab-delimited
#' files. It provides the correct column type specification and disables the
#' default quote escape option, as the files already have escaped quotes.
#'
#' @param file Path to a raw events tab-delimited file (".tab").
#' @param fix_names Normalize column names? Default is FALSE; see details. Names
#'   in the database are always normalized.
#' @param ... Other options passed to [readr::read_tsv()].
#'
#' @details The raw data file column names are capitalized and contain spaces.
#'   To make it easier to work with them in R and SQL, they are by default
#'   normalized by lower casing all and replacing spaces with underscores, e.g.
#'   "Event ID" becomes "event_id".
#'
#' @export
#' @importFrom readr read_tsv cols col_character col_integer col_date col_double
read_events_tsv <- function(file, fix_names = TRUE, ...) {
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
  x <- readr::read_tsv(file, col_types = col_fmt, quote = "", ...)
  if (isTRUE(fix_names)) {
    x <- normalize_column_names(x)
  }
  x
}


#' Normalize column names
#'
#' @param x Tibble containing raw events
normalize_column_names <- function(x) {
  cnames <- colnames(x)
  cnames <- tolower(cnames)
  cnames <- gsub(" ", "_", cnames)
  colnames(x) <- cnames
  x
}


#' List the raw data file paths
#'
#' Get a list of the paths for any local raw data ".tab" files.
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#' @param full_names Return the full path or only file name?
#'
#' @export
list_raw_files <- function(raw_file_dir = find_raw(), full_names = TRUE) {
  dir(raw_file_dir, pattern = ".tab", full.names = full_names)
}




