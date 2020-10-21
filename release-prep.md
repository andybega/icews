Notes to prep for release
=========================

```r
devtools::check_win_devel()
devtools::check_win_release()
ch <- rhub::check_for_cran()

# monitor progress
list_package_checks()

# eventually:
ch$cran_summary()
```
