#' Read ICEWS events into memory
#'
#' Read the entire ICEWS event data into memory. This takes up several (2-3 in 2018) GB.
#'
#' @param path Either path to SQLite database file or raw file directory. If
#'   NULL (default), the global options will be used instead.
#' @template n_max
#'
#' @seealso [query_icews()], [read_events_tsv()]
#'
#' @export
#' @import dplyr
#' @md
read_icews <- function(path = NULL, n_max = NULL) {
  # check n_max is positive integer
  if (!is.null(n_max)) {
    n_max <- as.integer(n_max)
    if (!is.integer(n_max) | n_max < 1) {
      stop("n_max must be a positive integer")
    }
  } else {
    n_max <- ifelse(is.null(n_max), Inf, n_max)
  }

  # throw error if path is null but options are not set
  opts <- get_icews_opts()
  null_path <- is.null(path)
  if (null_path & is.null(opts$data_dir)) {
    stop("Options are not set and path is NULL; set a path or call `setup_icews()`.")
  }
  # opts set; preferentially read from raw files
  if (null_path & isTRUE(opts$keep_files)) {
    return(read_icews_raw(find_raw(), n_max))
  }
  # opts set; but db only
  if (null_path & isFALSE(opts$keep_files)) {
    return(read_icews_db(find_db(), n_max))
  }
  # path is set, figure out to what it goes
  if (tools::file_ext(path)=="sqlite3") {
    return(read_icews_db(path, n_max))
  }
  if (any(grepl("^events.[0-9]{4}", dir(path)))) {
    return(read_icews_raw(path, n_max))
  }
}


#' Read data from raw files
#'
#' @template rfd
#' @template n_max
#' @param ... Options passed to [read_events_tsv()].
#'
#' @md
read_icews_raw <- function(raw_file_dir, n_max = NULL, ...) {
  data_files <- list_local_files(raw_file_dir)
  event_list <- list(NULL)
  n <- 0L
  for (i in seq_along(data_files)) {
    event_list[[i]] <- read_events_tsv(data_files[i], n_max = n_max, ...)
    n <- n + nrow(event_list[[i]])
    if (n >= n_max) break
  }
  events <- dplyr::bind_rows(event_list)

  # Add year and yearmonth since these will be useful for getting counts over time
  # un-normalized names
  if ("Event Date" %in% names(events)) {
    events$year      <- as.integer(format(events$`Event Date`, "%Y"))
    events$yearmonth <- as.integer(format(events$`Event Date`, "%Y%m"))
  } else {
    # normalized names
    events$year      <- as.integer(format(events$event_date, "%Y"))
    events$yearmonth <- as.integer(format(events$event_date, "%Y%m"))
  }

  events
}

#' Read data from DB
#'
#' @template dbp
#' @template n_max
read_icews_db <- function(db_path, n_max = NULL) {
  limit <- ifelse(is.infinite(n_max), "", paste0(" LIMIT ", n_max))
  sql   <- sprintf("SELECT * FROM events%s;", limit)
  events <- query_icews(sql, db_path)
  events$event_date <- as.Date(as.character(events$event_date), format = "%Y%m%d")
  events <- as_tibble(events)
  events
}


