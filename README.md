<!-- README.md is generated from README.Rmd. Please edit that file -->

icews
=====

[![CRAN
status](https://www.r-pkg.org/badges/version/icews)](https://cran.r-project.org/package=icews)
[![R build
status](https://github.com/andybega/icews/workflows/R-CMD-check/badge.svg)](https://github.com/andybega/icews/actions)
[![Codecov test
coverage](https://codecov.io/gh/andybega/icews/branch/master/graph/badge.svg)](https://codecov.io/gh/andybega/icews?branch=master)

Get the ICEWS event data from the Harvard Dataverse repos at
[https://doi.org/10.7910/DVN/28075](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075)
(historic data) and
[https://doi.org/10.7910/DVN/QI2T9A](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QI2T9A)
(daily updates from late 2018 on).

The icews package provides these major features:

-   get the ICEWS event data without having to deal with Dataverse
-   use raw data files (tab-separated variables, .tsv) or a database
    (SQLite3) or both as the storage backend
-   set options so that in future R sessions icews knows where your data
    lives
-   icews keeps the local data in sync with the latest versions on
    Dataverse

*Note about stability:*

*This package is young. The core functionality of the package involves
downloading and keeping track of potentially 10+GB of files and data. I
have not found a good way to test that thoroughly. There are probably
bugs and errors that pop up on a fresh run without any downloaded data
already in place. Please file bug reports or email me
(<a href="mailto:adbeger@gmail.com" class="email">adbeger@gmail.com</a>)!*

Installation
------------

Not on CRAN, so you will have to install via remotes or devtools:

``` r
library("remotes")
remotes::install_github("andybega/icews")
```

The **icews** package relies on the R [**dataverse**
client](https://github.com/IQSS/dataverse-client-r). Note that this
package requires a dataverse API token to correctly work. See the
package README.

Usage
-----

**tl;dr**: get a SQLite database with the current events on Dataverse
with this code; otherwise read below for more details.

``` r
Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")
library("icews")
library("DBI")
library("dplyr")

setup_icews(data_dir = "/where/should/data/be", use_db = TRUE, keep_files = TRUE,
            r_profile = TRUE)
# this will give instructions for what to add to .Rprofile so that settings
# persist between R sessions

update_icews(dryrun = TRUE)
# Should list proposed downloads, ingests, etc.
update_icews(dryrun = FALSE)
# Wait until all is done; like 45 minutes or more the first time around

# To get the data:
con <- connect()
DBI::dbGetQuery(con, "SELECT count(*) AS n FROM events;")
# or
query_icews("SELECT count(*) AS n FROM events;")
# or
con <- connect()
dplyr::tbl(con, "events") %>% summarize(n = n())
# or, 
# read all 17+ million rows into memory
events <- read_icews()
```

### Files only

In the most simple use case, you can use the package to download the
ICEWS event data, which comes in several tab-serparated value (TSV)
files, without having to deal with Dataverse.

``` r
Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")
library("icews")

dir.create("~/Downloads/icews")
download_data("~/Downloads/icews")
```

This will conventiently also re-use and update any files already in the
same directory. Zipped files will be unzipped. E.g. the monthly data
updates currently come in a file with the pattern
“events.2018.yyyymmddhhmmss.tab”, where the “yyyymmddhhmmss” part
changes. The downloader can identify this and will replace the old with
the new file.

Just in case, you can do a dry run that will not actually make any
changes:

``` r
download_data("~/Downloads/icews", dryrun = TRUE)
```

``` bash
Found 25 local data file(s)
Downloading 2 new file(s)
Removing 1 old local file(s)

Plan:
Download 'events.1995.20150313082510.tab.zip'
Download 'events.1996.20150313082528.tab.zip'
Remove   'events.1996.20140313082528.tab'
```

The events come in (zipped) tab-seperated files. To load all of these
into memory in a big combined data frame with about 16 million rows
(\~2.5Gb):

``` r
events <- read_icews("~/Downloads/icews")
```

Beyond this basic usage, the goal is to abstract as many little pains
away as possible. To that end:

### Persist the data directory location

The package can keep track of the data location via variables stored in
the package options. The easiest way is to add these to an “.Rprofile”
file so that they are available each time R starts up.

``` r
setup_icews(data_dir = "/where/should/data/be", use_db = TRUE, 
            keep_files = TRUE, r_profile = TRUE)
```

This will open the “.Rprofile” file and tell you what to add to it
(requires [usethis](https://cran.r-project.org/package=usethis) to be
installed). From now on the package knows where your data lives, and
most of the functions can be called without specifying any directory or
path arguments.

This sets three variables:

``` r
# ICEWS data location and options
options(icews.data_dir   = "~/path/to/icews_data")
options(icews.use_db     = TRUE)
options(icews.keep_files = TRUE)
```

### Use a SQLite database that keeps in sync with Dataverse

To setup and populate a database with the current version on Dataverse,
use this command:

``` r
# assumes setup_icews with use_db = TRUE has already been called
update_icews(dryrun = FALSE)
```

This will download any data files needed from Dataverse, and create and
populate a SQLite database with them. The events will be in a table
called “events”. To connect, use `connect()`; this returns a RSQLite
database connection. From then on, it can be used like this:

``` r
library("DBI")
library("dplyr")

con <- connect()

dbGetQuery(con, "SELECT count(*) FROM events;")
# or
tbl(con, "events") %>% summarize(n = n())
```

When done, it is good etiquette to close to the database connection:

``` r
DBI::dbDisconnect(con)
```

### Bonus: CAMEO codes

Also included is a dictionary of the CAMEO code for event types. This
includes quad and penta category mappings as well.

``` r
data("cameo_codes")
str(cameo_codes)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    312 obs. of  11 variables:
#>  $ cameo_code    : chr  "01" "010" "011" "012" ...
#>  $ name          : chr  "MAKE PUBLIC STATEMENT" "Make statement" "Decline comment" "Make pessimistic comment" ...
#>  $ level         : num  0 1 1 1 1 1 1 1 1 1 ...
#>  $ lvl0          : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lvl1          : int  NA 10 11 12 13 14 15 16 17 18 ...
#>  $ description   : chr  NA "All public statements expressed verbally or in action not otherwise specified." "Explicitly decline or refuse to comment on a situation." "Express pessimism, negative outlook." ...
#>  $ usage_notes   : chr  NA "This residual category is not coded except when distinctions among 011 to 017 cannot be made. Note that statements are typicall "This event form is a verbal act. The target could be who the source actor declines to make a comment to or about." "This event form is a verbal act. Only statements with explicit pessimistic components should be coded as 012; otherwise, defaul ...
#>  $ example       : chr  NA "U.S. military chief General Colin Powell said on Wednesday NATO would need to remain strong." "NATO on Monday declined to comment on an estimate that Yugoslav army and special police troops in Kosovo were losing 90 to 100  "Former West Germany Chancellor Willy Brandt said in a radio interview broadcast today he was skeptical over Moscow\u0082\xc4\xf ...
#>  $ order         : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ quad_category : chr  "verbal cooperation" "verbal cooperation" "verbal cooperation" "verbal cooperation" ...
#>  $ penta_category: chr  "statement" "statement" "statement" "statement" ...
```

And, a dictionary mapping Goldstein scores to CAMEO codes.

``` r
data("goldstein_mappings")
str(goldstein_mappings)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    312 obs. of  5 variables:
#>  $ code     : chr  "01" "010" "011" "012" ...
#>  $ name     : chr  "MAKE PUBLIC STATEMENT" "Make statement" "Decline comment" "Make pessimistic comment" ...
#>  $ goldstein: num  0 0 -0.1 -0.4 0.4 0 0 -5 0 3.4 ...
#>  $ nsLeft   : int  1 2 4 6 8 10 12 14 16 18 ...
#>  $ nsRight  : int  22 3 5 7 9 11 13 15 17 19 ...
```
