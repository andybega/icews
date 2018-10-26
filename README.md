<!-- README.md is generated from README.Rmd. Please edit that file -->
icews
=====

Get the ICEWS event data from the Dataverse repo at [https://doi.org/10.7910/DVN/28075](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075).

The goal is to eventually develop this into a package that can (a) keep a single local copy of the ICEWS event data in sync with the latest data on Dataverse and (b) be re-used between projects without the need to duplicate the 2-3Gb of data each time.

Installation
------------

Not on CRAN, so you will have to install via devtools:

``` r
library("devtools")
install_github("andybega/icews")
```

Usage
-----

**tl;dr**: get a SQLite database with the current events on DVN with this code; otherwise read below for more details.

``` r
library("icews")
library("DBI")
library("dplyr")
library("usethis")

setup_icews(data_dir = "/where/should/data/be", use_db = "TRUE", r_environ = TRUE)

sync_db()
# Wait until all is done; like 45 minutes or more the first time around

con <- connect_to_db()
DBI::dbGetQuery(con, "SELECT count(*) FROM events;")
# or
query("SELECT count(*) FROM events;")
# or
tbl(con, "events") %>% summarize(n = n())
```

### Files only

In the most simple use case, you can use the package to download the ICEWS event data, which comes in several tab-serparated value (TSV) files, without having to deal with Dataverse.

``` r
dir.create("~/Downloads/icews")
download_icews("~/Downloads/icews")
```

This will conventiently also re-use and update any files already in the same directory. E.g. the monthly data updates currently come in a file with the pattern "events.2018.yyyymmddhhmmss.tab", where the "yyyymmddhhmmss" part changes. The downloader can identify this and will replace the old with the new file.

Just in case, you can do a dry run that will not actually make any changes:

``` r
download_icews("~/Downloads/icews", dryrun = TRUE)
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

The events come in (zipped) tab-seperated files. To load all of these into memory in a big combined data frame with about 16 million rows (~2.5Gb):

``` r
events <- read_icews("~/Downloads/icews")
```

Beyond this basic usage, the goal is to abstract as many little pains away as possible. To that end:

### Persist the data directory location

The package can keep track of the data location via an environment variable. The easiest way is to add these to an ".Renviron" file so that they are available each time R starts up.

``` r
setup_icews(data_dir = "/where/should/data/be", use_db = "TRUE", r_environ = TRUE)
```

This will open the ".Renviron" file and tell you what to add to it (requires [usethis](https://cran.r-project.org/package=usethis) to be installed). From now on the package knows where your data lives, and most of the functions can be called without specifying any directory or path arguments.

This sets two environment variables, although the second one is not used right now.

``` r
Sys.getenv("ICEWS_DATA_DIR")
Sys.getenv("ICEWS_USE_DB")
```

### Use a SQLite database that keeps in sync with DVN

To setup and populate a database with the current version on DVN, use this command:

``` r
sync_db()
```

This will download any data files needed from DVN, and create and populate a SQLite database with them. The events will be in a table called "events". To connect, use `connect_to_db`; this returns a RSQLite database connection. From then on, it can be used like this:

``` r
library("DBI")
library("dplyr")

con <- connect_to_db()

dbGetQuery(con, "SELECT count(*) FROM events;")
# or
tbl(con, "events") %>% summarize(n = n())
```

When done, it is good etiquette to close to the database connection:

``` r
DBI::dbDisconnect(con)
```

### Bonus: CAMEO codes

Also included is a dictionary of the CAMEO code for event types.

``` r
data("cameo_codes")
head(cameo_codes[, 1:5])
#>   code                     name level lvl0 lvl1
#> 1   01    MAKE PUBLIC STATEMENT     0   01 <NA>
#> 2  010           Make statement     1   01  010
#> 3  011          Decline comment     1   01  011
#> 4  012 Make pessimistic comment     1   01  012
#> 5  013  Make optimistic comment     1   01  013
#> 6  014   Consider policy option     1   01  014
```

And, a dictionary mapping Goldstein scores to CAMEO codes.

``` r
data("goldstein_mappings")
head(goldstein_mappings[, 1:5])
#>   code                     name goldstein nsLeft nsRight
#> 1   01    MAKE PUBLIC STATEMENT       0.0      1      22
#> 2  010           Make statement       0.0      2       3
#> 3  011          Decline comment      -0.1      4       5
#> 4  012 Make pessimistic comment      -0.4      6       7
#> 5  013  Make optimistic comment       0.4      8       9
#> 6  014   Consider policy option       0.0     10      11
```
