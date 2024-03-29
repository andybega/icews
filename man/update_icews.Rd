% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{update_icews}
\alias{update_icews}
\title{Update database and files}
\usage{
update_icews(
  dryrun = TRUE,
  use_db = getOption("icews.use_db"),
  keep_files = getOption("icews.keep_files"),
  db_path = find_db(),
  raw_file_dir = find_raw(),
  quiet = FALSE,
  plan = NULL
)
}
\arguments{
\item{dryrun}{Just list changes that would be made, without making them.}

\item{use_db}{Store events in a SQLite database?}

\item{keep_files}{If using a database, retain raw data TSV files?}

\item{db_path}{Path to SQLite database file}

\item{raw_file_dir}{Directory containing the raw event TSV files.}

\item{quiet}{Don't print progress messages}

\item{plan}{Shortcut to avoid talking to dataverse twice, see examples
below.}
}
\description{
Maintain a current set of ICEWS events in the local database that match
the latest versions on DVN. If needed, create the database and download data
files.
}
\details{
The behavior of \code{update_icews} depends on the ICEWS option values,
as set via \code{setup_icews}, or alternatively manually specified function
arguments. The intended use is with options set in .Rprofile.

If "use_db" and "keep_files" are both true, data files will be downloaded to
the "raw" directory in the root ICEWS directory
(\code{get_icews_opts()$data_dir}), then ingested into a SQLite database in the
"db" directory. Each data file is ingested before the next one is downloaded,
so that if the process is interrupted there will be at least some data in the
database.

If "use_db" is set with "keep_files" as false, data is downloaded to
temporary files before ingestion. This saves about 5GB of space as of late
2018.

If "use_db" is false, then regardless of the "keep_files" value, only the
raw data files will be downloaded. This is equivalent to \code{\link[=download_data]{download_data()}}.

You can manually replicate what \code{update_icews()} does by calling
\code{\link[=download_data]{download_data()}}, followed by \code{\link[=sync_db_with_files]{sync_db_with_files()}}.
}
\examples{
\dontrun{
# assuming this is the first time the package is used:
setup_icews("path/to/icews_data", use_db = TRUE, keep_files = TRUE,
            r_profile = TRUE)
}

# see the plan for the initial download
# update_icews(dryrun = TRUE)

# do the initial download; this will take a while (1hr or so)
# update_icews(dryrun = FALSE)

# talking to dataverse takes takes up some time and doing it twice
# can be avoided by doing:
# plan <- update_icews(dryrun = TRUE)
# update_icews(dryrun = FALSE, plan = plan)

# call it again in the future to check if updates are needed

}
