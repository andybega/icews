#' Get ICEWS Event Data
#'
#' @description Download utilities and other helpers for getting the ICEWS event
#'   data from Dataverse.
#'
#' @section Package options:
#'
#' Icews uses the following [options()] to configure behavior:
#'
#' \itemize{
#'   \item `icews.debug`: set to TRUE to print additional diagnostic messages
#'     in various functions.
#' }
#'
#' Options that control file and database paths, and which should be set
#' with [setup_icews()]:
#'
#' \itemize{
#'   \item `icews.data_dir`: Path to the directory in which data will be kept
#'     locally. Depending on the other settings, subdirectories like "raw" and
#'     "db" will be created within.
#'   \item `icews.use_db`: Use a SQLite database to store the data?
#'   \item `icews.keep_files`: If "use_db" is TRUE, retain the raw data TSV
#'     files or delete after ingesting to the database?
#' }
#'
#' See [https://github.com/andybega/icews](https://github.com/andybega/icews) and
#' [https://www.andybeger.com/icews/](https://www.andybeger.com/icews/) for
#' more information.
#'
#' @references
#'   Boschee, Elizabeth; Lautenschlager, Jennifer; O'Brien, Sean;
#'   Shellman, Steve; Starz, James; Ward, Michael, 2015, ``ICEWS Coded
#'   Event Data'', \doi{10.7910/DVN/28075}, Harvard Dataverse.
#'
#'   Boschee, Elizabeth; Lautenschlager, Jennifer; O'Brien, Sean; Shellman,
#'   Steve; Starz, James, 2018, ``ICEWS Weekly Event Data'',
#'   \doi{10.7910/DVN/QI2T9A}, Harvard Dataverse.
#'
#'
#' @name icews
#' @docType package
#' @md
"_PACKAGE"
globalVariables(c("cameo_codes", "goldstein_mappings"))

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("icews.debug")))  {
    options("icews.debug" = FALSE)
  }
}

.onAttach <- function(...) {
  opts <- get_icews_opts()
  msg  <- format(opts)
  packageStartupMessage(paste0(msg, collapse = "\n"))

  # see #72
  msg <- check_dataverse_version()
  if (!isTRUE(msg)) packageStartupMessage(msg)

  # Make sure the DATAVERSE_SERVER env variable is set
  if (Sys.getenv("DATAVERSE_SERVER")!="dataverse.harvard.edu") {
    msg <- paste0(c(
      strwrap("The R dataverse client requires the DATAVERSE_SERVER environment variable to be set. See <https://github.com/IQSS/dataverse-client-r>. Please run:"),
      "Sys.setenv(DATAVERSE_SERVER = \"dataverse.harvard.edu\")"
    ), collapse = "\n")
    packageStartupMessage(msg)
  }
}

## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL
