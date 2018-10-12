<!-- README.md is generated from README.Rmd. Please edit that file -->
icews
=====

Get the ICEWS event data from the Dataverse repo at <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075>.

The goal is to eventually develop this into a package that can (a) keep a single local copy of the ICEWS event data in sync with the latest data on Dataverse and (b) be re-used between projects without the need to duplicate the 2-3Gb of data each time.

Installation
------------

``` r
library("devtools")
install_github("andybega/icews")
```

Usage
-----

To download the current data files from Dataverse, use `download_icews`. It accepts a path argument for the download destination. Alternatively, set an environment variable for the destination path. The plan is that in the future, the package will work by keeping a persistent local copy of some sort in sync with the version on Dataverse, including any more recent data releases. But for now, it's just a convenient shortcut to avoid manually downloading from Dataverse.

(BTW, this takes a while.)

``` r
library("icews")

Sys.setenv(ICEWS_DATA_DIR = "~/Dropbox/Work/VForecast/Data/icews_data")

download_icews()
```

The events come in (zipped) tab-seperated files. To load all of these into memory in a big combined data frame with about 16 million rows (~2.5Gb):

``` r
events <- read_icews()
```

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
