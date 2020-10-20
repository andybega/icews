# Stuff that touches dataverse

check_dataverse_version <- function() {
  # Check that the installed dataverse version has the bug fix references in
  # #51, #58, and lastly #72
  if (utils::packageVersion("dataverse") < "0.2.1.9001") {
    msg <- paste0(c(
      strwrap("There is bug in the dataverse R package prior to version 0.2.1.9001 that breaks file downloading. Please check on CRAN if a newer version is available or install the development version from GitHub using:"),
      "  remotes::install_github(\"IQSS/dataverse-client-r\")"
    ), collapse = "\n")
    return(msg)
  }
  invisible(TRUE)
}

#' Download a single file from Dataverse
#'
#' Download a single file from the ICEWS Dataverse repository and unzip if neccessary
#'
#' @param file Name (label) of the file on DVN to download. If zipped, it will
#'   automatically be unzipped.
#' @param to_dir Destination directory.
#' @param repo Which repo is the file in? ("historic" or "weekly")
#' @param file_id Optionally, integer file ID. If this is specified, it will
#'   preferentially be used over the file name to download the file. This is
#'   needed for duplicate file names.
#' @param new_name Optionally, a new file name for the file. Useful for
#'   duplicate files.
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
download_file <- function(file, to_dir, repo = "historic", file_id = NULL, new_name = NULL) {

  # see #72
  msg <- check_dataverse_version()
  if (!isTRUE(msg)) stop(msg)

  if (length(file) > 1) stop("I'm not vectorized")

  # override default repo if certain filename is detected
  if (repo=="historic" & grepl("[0-9]{8}\\-icews\\-events\\.zip", file)) {
    repo = "weekly"
  }

  file_ref <- if (!is.null(file_id)) file_id else file

  # There is a bug in the dataverse CRAN release that is fixed in the dev
  # version from github. See #51 and #58.
  f <- dataverse::get_file(file = file_ref, dataset = get_doi()[[repo]],
                           format = NULL)

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

  if (!is.null(new_name)) {
    if (new_name != basename(con)) {
      file.rename(con, file.path(to_dir, new_name))
    }
  }

  return(invisible(con))
}
