# Save a DVN manifest so we can do unit tests with mock substitute

dontrun <- function() {

  foo <- get_dvn_manifest()
  saveRDS(foo, "inst/testdata/dvn_manifest.rds")

}
