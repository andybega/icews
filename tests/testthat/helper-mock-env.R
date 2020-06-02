
# Creates both local an db structure
setup_mock_environment <- function(pop_raw = FALSE, init_db = FALSE, pop_db = FALSE) {
  dd <- tempdir()
  o <- sapply(file.path(dd, c("raw", "db", "docs")), dir.create, showWarnings = FALSE)
  raw_file_dir <- file.path(dd, "raw")
  db_path <- tempfile(fileext = ".sqlite3", tmpdir = file.path(dd, "db"))
  if (file.exists(db_path)) {
    unlink(db_path, force = TRUE)
  }
  if (pop_raw) {
    populate_mock_raw(raw_file_dir)
  }
  if (init_db | pop_db) {
    init_mock_db(db_path)
  }
  if (pop_db) {
    populate_mock_db(db_path)
  }
  list(db_path = db_path, raw_file_dir = raw_file_dir)
}

# initialize and populate a temporary database
setup_mock_db <- function(db_path = NULL, init = TRUE, populate = TRUE) {
  if (is.null(db_path)) {
    db_path <- tempfile(fileext = ".sqlite3")
  }
  if (file.exists(db_path)) {
    unlink(db_path)
  }
  if (init) {
    init_mock_db(db_path)
  }
  if (populate) {
    populate_mock_db(db_path)
  }
  invisible(db_path)
}

setup_mock_raw <- function(raw_file_dir = NULL, populate = TRUE) {
  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(tempdir(), "raw")
  }
  dir.create(raw_file_dir, showWarnings=FALSE)
  if (populate) {
    populate_mock_raw(raw_file_dir)
  }
  invisible(raw_file_dir)
}

tsv_sample_path <- function() {
  system.file("extdata", "events.2018.sample.tab", package = "icews")
}

# populate mock raw dir with TSV sample file
populate_mock_raw <- function(raw_file_dir) {
  sample_tsv <- tsv_sample_path()
  file.copy(sample_tsv, raw_file_dir)
  invisible(raw_file_dir)
}

# initialize mock DB
init_mock_db <- function(db_path = NULL) {
  create_db(db_path)
  invisible(db_path)
}

# populate mock DB with events
populate_mock_db <- function(db_path) {
  ingest_from_file(tsv_sample_path(), db_path)
  invisible(db_path)
}

clean_mock_environment <- function(p) {
  unlink(dirname(p$db_path), recursive = TRUE)
  unlink(file.path(p$raw_file_dir, "events.2018.sample.tab"))
}
