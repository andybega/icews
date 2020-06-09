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
#' @md
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

  # hack fix for m/d/y format in Events.2017 (#57)
  x <- readLines(file)[2]
  x <- strsplit(x, "\t")[[1]][2]
  if (grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)) {
    col_fmt <- readr::cols(
      .default = col_character(),
      `Event ID` = col_integer(),
      `Event Date` = col_date(format = "%m/%d/%Y"),
      Intensity = col_double(),
      `Story ID` = col_integer(),
      `Sentence Number` = col_integer(),
      Latitude = col_double(),
      Longitude = col_double()
    )
  }

  x <- readr::read_tsv(file, col_types = col_fmt, quote = "", ...)
  if (isTRUE(fix_names)) {
    x <- normalize_column_names(x)
  }
  x
}


#' Normalize column names
#'
#' @param x Tibble containing raw events
#'
#' @keywords internal
normalize_column_names <- function(x) {
  cnames <- colnames(x)
  cnames <- tolower(cnames)
  cnames <- gsub(" ", "_", cnames)
  colnames(x) <- cnames
  x
}
