#' @rdname state
#'
#' @param raw_file_dir Directory containing raw data files
#'
#' @export
get_local_state <- function(raw_file_dir = find_raw()) {
  files <- basename(list_local_files(raw_file_dir))
  state <- parse_label(files)
  state$label <- state$is_data <- NULL
  colnames(state) <- paste0("local_", colnames(state))
  state
}

#' List the raw data file paths
#'
#' Get a list of the paths for any local raw data ".tab" files.
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#' @param full_names Return the full path or only file name?
#'
#' @export
list_local_files <- function(raw_file_dir = find_raw(), full_names = TRUE) {
  o <- dir(raw_file_dir, full.names = full_names)
  # check it's all events.YYYY....tab or YYYYMMDD-icews-events.zip files
  good1 <- grepl("^events\\.[0-9]{4}\\.[0-9a-z]+\\.tab$", basename(o))
  good2 <- grepl("^[0-9]{8}[0-9a-z\\-]+\\.tab$", basename(o))
  offending <- !good1 & !good2
  if (any(offending)) {
    ff <- paste0("  ", basename(o)[offending], collapse = "  \n")
    msg <- sprintf("unexpected non-data file(s) found in '%s':", raw_file_dir)
    msg <- paste0(c(msg, ff), collapse = "\n")
    stop(msg)
  }
  o
}

