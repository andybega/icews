#' @rdname state
#'
#' @param raw_file_dir Directory containing raw data files
#'
#' @export
get_local_state <- function(raw_file_dir = find_raw()) {
  files <- basename(list_local_files(raw_file_dir))

  state <- tibble(
    file_name = files
  )
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
  is_df <- is_data_file(o)
  if (any(!is_df)) {
    ff <- paste0("  ", basename(o)[!is_df], collapse = "  \n")
    msg <- sprintf("unexpected non-data file(s) found in '%s':", raw_file_dir)
    msg <- paste0(c(msg, ff), collapse = "\n")
    stop(msg)
  }
  o
}

#' Daily data file?
#'
#' @keywords internal
is_daily_file <- function(x) {
  grepl("^[0-9]{8}[0-9a-z\\-]+\\.tab$", basename(x))
}


is_data_file <- function(x) {
  # check it's all events.YYYY....tab or YYYYMMDD-icews-events.zip files
  good1 <- grepl("^events\\.[0-9]{4}\\.[0-9a-z]+\\.tab$", basename(x))
  good2 <- is_daily_file(x)

  good1 | good2
}





