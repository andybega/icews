
#' Weekly data file?
#'
#' @keywords internal
is_weekly_file <- function(x) {
  grepl("^[0-9]{8}[0-9a-z\\-]+\\.(tab|zip)$", basename(x))
}


is_data_file <- function(x) {
  # check it's all events.YYYY....tab or YYYYMMDD-icews-events.zip files
  good1 <- grepl("^[Ee]{1}vents\\.[0-9]{4}\\.[0-9a-z]+\\.(tab|zip|tab.zip)$", basename(x))
  good2 <- is_weekly_file(x)

  good1 | good2
}

#' Identify dataset contained in file
#'
#' Identify which time period is nominally covered by a file. This is kept
#' around from prior version of the package, in case it becomes useful again.
#' E.g. to allow for time range specific downloading.
#'
#' @param x a normalized file name
#'
#' @keywords internal
parse_dataset <- function(x) {
  data_set <- rep(NA_character_, length(x))
  is_data_mask <- is_data_file(x)
  out <- gsub("(.[0-9]{8,})|([Ee]vents.)|(-icews-events)|(.tab|.zip|.tab.zip)|(.sample)", "", x[is_data_mask])
  data_set[is_data_mask] <- out
  data_set
}



