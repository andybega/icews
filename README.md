<!-- README.md is generated from README.Rmd. Please edit that file -->
icews
=====

Get the ICEWS event data from the Dataverse repo at <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075>.

Example
-------

To download the current data files from Dataverse:

``` r
library("icews")

Sys.setenv(ICEWS_DATA_DIR = "~/Dropbox/Work/VForecast/Data/icews_data")

download_icews()
events <- read_icews()
```

``` r
data("cameo_codes")
head(cameo_codes)
#>   code                     name level lvl0 lvl1
#> 1   01    MAKE PUBLIC STATEMENT     0   01 <NA>
#> 2  010           Make statement     1   01  010
#> 3  011          Decline comment     1   01  011
#> 4  012 Make pessimistic comment     1   01  012
#> 5  013  Make optimistic comment     1   01  013
#> 6  014   Consider policy option     1   01  014
#>                                                                      description
#> 1                                                                           <NA>
#> 2 All public statements expressed verbally or in action not otherwise specified.
#> 3                        Explicitly decline or refuse to comment on a situation.
#> 4                                           Express pessimism, negative outlook.
#> 5                                       Express optimism, assurance, confidence.
#> 6                                  Review, reflect upon, or study policy option.
#>                                                                                                                                                                                                                                                                                                                               usage_notes
#> 1                                                                                                                                                                                                                                                                                                                                    <NA>
#> 2 This residual category is not coded except when distinctions among 011 to 017 cannot be made. Note that statements are typically subordinate events; events such as comments are coded as mere statements only when they do not further imply appeals, agreements, support, apologies, demands, disapprovals, rejections, threats, etc.
#> 3                                                                                                                                                                                                                       This event form is a verbal act. The target could be who the source actor declines to make a comment to or about.
#> 4                                                                                                                                                                                                This event form is a verbal act. Only statements with explicit pessimistic components should be coded as 012; otherwise, default to 010.
#> 5                                                                                                                                                                                                 This event form is a verbal act. Only statements with explicit optimistic components should be coded as 013; otherwise, default to 010.
#> 6                                                                                                                                                                                    This event form is typically, although not exclusively, a verbal act. There is no limitation on types of policies that could be under consideration.
#>                                                                                                                                                                                                            example
#> 1                                                                                                                                                                                                             <NA>
#> 2                                                                                                                     U.S. military chief General Colin Powell said on Wednesday NATO would need to remain strong.
#> 3                                                 NATO on Monday declined to comment on an estimate that Yugoslav army and special police troops in Kosovo were losing 90 to 100 dead per day in NATO air strikes.
#> 4                        Former West Germany Chancellor Willy Brandt said in a radio interview broadcast today he was skeptical over Moscow\x82\xc4\xf4s will to agree on limiting European-based nuclear weapons.
#> 5                                                   Turkish President Turgut Ozal said on Wednesday he was confident that the United States would remove irritants damaging relations between the two NATO allies.
#> 6 Europe\x82\xc4\xf4s leading security forum is exploring the possibility of international patrols to monitor the former Yugoslav republic of Macedonia\x82\xc4\xf4s border with Serbia, its envoy said on Friday.
#>   order
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
#> 6     6
```
