
#' Generic path finder
#'
#' @param x The kind of path to find; "db", "raw", or "docs".
find_path <- function(x) {
  data_dir <- getOption("icews.data_dir")
  if (!is.null(data_dir)) {
    suffix <- switch(x,
                     db   = "db/icews.sqlite3",
                     raw  = "raw",
                     docs = "docs")
    path <- file.path(data_dir, suffix)
    return(path)
  }
  lines <- c(
    "Path argument is missing.",
    "Consider setting the paths up globally with `options(icews.data_dir = \"path/to/dir\")`.",
    "Ideally in your .Rprofile file; try running `dr_icews()` for help."
  )
  stop(paste(lines, collapse = "\n"))
}

#' Find database path
find_db <- function() {
  find_path("db")
}

#' Find raw file directory
find_raw <- function() {
  find_path("raw")
}

#' Find docs directory
find_docs <- function() {
  find_path("docs")
}
