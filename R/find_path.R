#' Generic path finder
#'
#' @param x The kind of path to find; "db", "raw", or "docs".
#' @param ... additional elements to be combined with the file path using
#'   [base::file.path()]
#'
#' @keywords internal
find_path <- function(x, ...) {
  data_dir <- getOption("icews.data_dir")
  if (!is.null(data_dir)) {
    suffix <- switch(x,
                     db   = "db/icews.sqlite3",
                     raw  = "raw",
                     docs = "docs")
    path <- file.path(data_dir, suffix, ...)
    return(path)
  }
  lines <- c(
    "Path argument is missing.",
    "Consider setting the paths up globally with `setup_icews()`.",
    "Ideally in your .Rprofile file; try running `dr_icews()` for help."
  )
  stop(paste(lines, collapse = "\n"))
}

#' Find locations
#'
#' If the options have been set (see [setup_icews()]), `find_raw` will return
#' the path to the raw data file directory, `find_db` the database file path,
#' and `find_docs` the directory containing documentation files.
#'
#' @param ... additional elements that will be combined with the base path
#'   using [base::file.path()]
#'
#' @examples
#' \dontrun{
#' find_raw()
#' find_docs()
#' find_db()
#'
#' # additional elements will be combined with the base path, which makes it
#' # easier to get the full path for the contents of the data directory:
#' file_list <- dir(find_raw())
#' find_raw(file_list[1])
#' events <- read_events_tsv(find_raw(file_list[1]))
#' }
#'
#' @export
#' @md
find_raw <- function(...) {
  find_path("raw", ...)
}

#' @rdname find_raw
#' @export
find_db <- function(...) {
  find_path("db", ...)
}

#' @rdname find_raw
#' @export
find_docs <- function(...) {
  find_path("docs", ...)
}
