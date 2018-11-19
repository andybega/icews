#' Get ICEWS Event Data
#'
#' @description Download utilities and other helpers for getting the ICEWS event
#'   data from Dataverse.
#'
#' @details The package is meant to be used in a semi-automated fashion to
#' maintain an updated version of the ICEWS event data in a local database. The
#' workflow for using it like this would be to first add relevant options to
#' your `.Rprofile` file, and then synchronizing with DVN. The code to do so
#' looks like this:
#'
#' ```
#' library("icews")
#' library("usethis")
#' setup_icews("/path/to/icews_data", use_db = TRUE, keep_files = FALSE,
#'             r_profile = TRUE)
#' update_icews()
#'
#' con <- connect()
#' DBI::dbGetQuery(con, "SELECT count(*) AS n FROM events;")
#' # or
#' query("SELECT count(*) AS n FROM events;")
#' # or
#' tbl(con, "events") %>% summarize(n = n())
#' ```
#'
#' This will initialize a database and download the ICEWS events from dataverse.
#' The data will be stored at `/path/to/icews_data`, with the following
#' organization (as needed):
#' - `db`: contains the database file, named `icews.sqlite3`
#' - `raw`: contains raw TSV data files (unzipped if neccessary)
#' - `docs`: contains copies of the ICEWS documentation (not implemented yet)
#'
#' Use the `dryrun = TRUE` argument to only list proposed change, without
#' taking any actual actions. The other two top-level sync functions discussed
#' below also have this argument.
#'
#' A more minimalistic workflow that doesn't require any options to be set would
#' be to only download the current set of data files. This can be done with:
#'
#' ```
#' download_data(to_dir = "~/Downloads/icews_data/raw", update = FALSE)
#'
#' # ro read the data files
#' events <- read_icews(raw_file_dir = "~/Downloads/icews_data/raw")
#' ```
#'
#' This only downloads the current data files from DVN, thus obviating the need
#' to have to interact with dataverse. Optionally it will also update any
#' outdated local data files in the same directory when the "update" argument
#' is set to TRUE:
#'
#' ```
#' download_data(to_dir = "~/Downloads/icews_data/raw", update = TRUE)
#' ```
#'
#' Lastly, to sync a database with local data files, without downloading any
#' new files, use this function:
#'
#' ```
#' sync_db_with_files(raw_file_dir = "~/Downloads/icews_data/raw",
#'                    db_path = "~/Downloads/icews_data/db/icews.sqlite3")
#' ```
#'
#' See [https://github.com/andybega/icews](https://github.com/andybega/icews) and
#' [https://www.andybeger.com/icews/](https://www.andybeger.com/icews/) for
#' more information.
#'
#' @references
#'   Boschee, Elizabeth; Lautenschlager, Jennifer; O'Brien, Sean;
#'   Shellman, Steve; Starz, James; Ward, Michael, 2015, ``ICEWS Coded
#'   Event Data'', [https://doi.org/10.7910/DVN/28075](https://doi.org/10.7910/DVN/28075), Harvard Dataverse.
#'
#' @name icews
#' @docType package
#' @md
"_PACKAGE"
globalVariables(c("cameo_codes", "goldstein_mappings"))
globalVariables(c("action", "data_set", "db_data_set", "db_version",
                  "dvn_data_set", "dvn_file", "dvn_label", "dvn_version",
                  "in_db", "in_local", "is_data", "local_data_set",
                  "local_file", "local_version", "on_dvn", "where"))





