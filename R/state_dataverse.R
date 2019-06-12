#
#   Functions related to determining the local vs DVN state
#

#' Parse a data file label
#'
#' Parse the Dataverse file label to identify whether it contains data and if so which
#' event set and file version.
#'
#' @param label character string
#'
#' @return A data frame with components:
#' - label
#' - file
#' - is_date: TRUE or FALSE
#' - dataset
#' - version
#'
#' @md
#' @import tibble
#' @keywords internal
parse_label <- function(label) {
  out <- tibble::tibble(
    label   = as.character(label),
    file    = ifelse(substring(label, nchar(label) - 7)==".tab.zip",
                     gsub(".zip", "", label),
                     gsub(".zip", ".tab", label)),
    is_data = grepl("(\\.tab)|(icews-events\\.zip)", label),
    data_set = NA,
    version  = NA
  )
  # if label is empty, file turns to logical, which causes test errors when
  # trying to merge character to logical down the line
  out$file <- as.character(out$file)
  out$data_set[out$is_data] <- gsub("(.[0-9]{14})|(.tab)|(.sample)", "", out$file[out$is_data])
  out$version[out$is_data]  <- gsub("(events.[0-9]{4}.)|(.tab)", "", out$file[out$is_data])
  out
}

#' Get DVN/local file/database state
#'
#' Determine what data files, event sets, and version are currently on dataverse,
#' in the local files, or in the local database.
#'
#' @rdname state
#'
#' @param icews_doi DOI of the main ICEWS repo on Dataverse, see [get_doi()]
#' @param server For unit tests only; default is set to [dataverse::get_dataset()] default.
#'
#' @details The data files (tab-separated files, ".tab") on dataverse that
#'   contain the raw event data follow a common format denoting the set of
#'   events contained in a file and which version of the event data and/or file
#'   dump they correspond to. For example, "events.1995.20150313082510.tab"
#'   contains events for 1995 and the version is denoted by the timestamp,
#'   "20150313082510".
#'
#'   The download and update functions
#'   ([update_icews()], [download_data()]) will recognize which event sets
#'   are locally available or still need to be downloaded, and whether any
#'   local even sets have been superseded by a new version in dataverse, by
#'   using
#'
#' @return For `get_local_state` and `get_db_state`, a tibble with columns:
#'   - db/local_file: the full source data file name, e.g. "events.1995.20150313082510.tab".
#'   - db/local_data_set: the event set contained in the file, e.g. "events.1995".
#'   - db/local_version: the version of the file/event set, e.g. "20150313082510"
#'
#'   For `get_dvn_manifest`, a list of length 3, containing:
#'   - data_files: a tibble similar to those returned by `get_local_state` and
#'     `get_db_state`, with an additional column for the file label on dataverse
#'     since some files are zipped, i.e. ending with ".tab.zip" instead of ".tab".
#'   - files: a summary list of all files on DVN, consisting of the data files
#'     but also documentation and metadata files.
#'   - dataverse_dataset: an object of class "dataverse_dataset", returned by
#'     [dataverse::get_dataset()].
#'
#' @examples
#' \dontrun{
#' # Remote (DVN) state
#' get_dvn_manifest()
#' # Local file state
#' get_local_state()
#' # Database state
#' get_db_state()
#' }
#'
#' @md
#' @export
#' @import dataverse
#' @import tibble
get_dvn_manifest <- function(icews_doi = get_doi(), server = Sys.getenv("DATAVERSE_SERVER")) {
  dvn_files  <- tryCatch(
    tibble(repo = c("historic", "daily"),
           content = list(
             dataverse::get_dataset(icews_doi$historic, server = server),
             dataverse::get_dataset(icews_doi$daily, server = server)
           )),
    error = function(e) {
      stop("Something went wrong in 'dataverse' or the Dataverse API, try again. Original error message:\n", e$message)
    })

  file_list <- bind_rows(
    tibble::tibble(
      repo = "historic",
      label = dvn_files$content[[1]]$files$label,
      category = unlist(dvn_files$content[[1]]$files$categories),
      description = dvn_files$content[[1]]$files$description
    ),
    tibble::tibble(
      repo = "daily",
      label = dvn_files$content[[2]]$files$label,
      category = "Data",
      description = dvn_files$content[[2]]$files$description
    )
  )

  # filter out known corrupt file
  file_list <- file_list[!file_list$label=="20181006-icews-events.zip", ]

  data_file_list <- parse_label(file_list$label)
  data_file_list <- subset(data_file_list, is_data)
  data_file_list$is_data <- NULL

  list(
    files             = file_list,
    data_files        = data_file_list,
    dataverse_dataset = dvn_files
  )
}









