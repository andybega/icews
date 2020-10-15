# Save a DVN manifest so we can do unit tests with mock substitute

dontrun <- function() {

  #devtools::load_all()
  manifest <- get_dvn_manifest()
  saveRDS(manifest, "inst/testdata/dvn_manifest.rds")

}
