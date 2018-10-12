#' Download ICEWS data files
#'
#' Download the ICEWS event data from Dataverse
#'
#' @param path Path to directory where data files will be downloaded to.
#' @param overwrite Should duplicate files that are already at path be
#'   re-downloaded?
#'
#' @export
#' @import dataverse
download_icews <- function(path = Sys.getenv("ICEWS_DATA_DIR"), overwrite = FALSE) {
  icews_doi <- "doi:10.7910/DVN/28075"

  if (path==Sys.getenv("ICEWS_DATA_DIR")) {
    path_raw <- file.path(path, "raw")
  } else {
    path_raw <- path
  }

  manifest <- get_dvn_manifest()

  if (overwrite==FALSE) {
    files_at_path <- dir(path)

    # Check for any zip files that match DVN zip file; unzip if find any
    # Don't want to do this in the loop below so the loop can download/unzip
    # one file at a time without checking each time if zip version is already
    # there
    local_zip <- files_at_path[grepl(".zip$", files_at_path) &
                                      files_at_path %in% manifest$data_files$file]
    if (length(local_zip) > 0) {
      sapply(local_zip, unzip, exdir = path)

      # Update with new unzipped files
      files_at_path <- dir(path)
    }

    # Check which files we don't need to download again
    already_here <- manifest$data_files$file %in% files_at_path
    manifest$data_files <- manifest$data_files[!already_here, ]
    if (any(already_here)) {
      cat(sprintf("Found %s existing data files at path, not overwriting\n", sum(already_here)))
    }
  }

  for (dvn_file in manifest$data_files$dvn_file) {
    # Get binary blob
    cat(sprintf("Downloading %s\n", dvn_file))
    f <- get_file(dvn_file, icews_doi)

    # Decide how to handle based on whether extraction is needed
    if (tools::file_ext(dvn_file)=="zip") {
      tmp <- tempfile(fileext = ".tab.zip")
      writeBin(as.vector(f), tmp)
      con <- unzip(tmp, exdir = path)
    } else {
      fname <- gsub(".zip", "", dvn_file)
      con <- file.path(path, fname)
      writeBin(as.vector(f), con)
    }
  }
  invisible(NULL)
}

#' Get Dataverse file list
#'
#' Get information on current ICEWS files on Dataverse
#'
#' @export
#' @import dataverse
get_dvn_manifest <- function() {
  icews_doi <- "doi:10.7910/DVN/28075"
  dvn_files  <- get_dataset(icews_doi)
  data_files_dvn <- dvn_files$files$label[str_detect(dvn_files$files$label, ".tab")]
  data_files_tsv <- gsub(".zip", "", data_files_dvn)

  list(
    files = dvn_files$files$label,
    data_files = data.frame(
      dvn_file = data_files_dvn,
      zipped = grepl(".zip", data_files_dvn),
      file = data_files_tsv,
      stringsAsFactors = FALSE
    )
  )
}

#' Read and combine raw data files
#'
#' Read the entire ICEWS event data into memory. This takes up several (2-3 in 2018) GB.
#'
#' @param path Path to directory where data files will be downloaded to.
#'
#' @export
#' @import dplyr
#' @import readr
#' @import purrr
read_icews <- function(path = Sys.getenv("ICEWS_DATA_DIR")) {
  if (path==Sys.getenv("ICEWS_DATA_DIR")) {
    path_raw <- file.path(path, "raw")
  } else {
    path_raw <- path
  }

  data_files <- dir(path_raw, pattern = ".tab", full.names = TRUE)
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
