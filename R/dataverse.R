# Stuff that touches dataverse

check_dataverse_version <- function() {
  # Check that the installed dataverse version has the bug fix references in
  # #51, #58, and lastly #72
  if (utils::packageVersion("dataverse") < "0.2.1.9001") {
    msg <- paste0(c(
      strwrap("There is bug in the dataverse R package prior to version 0.2.1.9001 that breaks file downloading. Please check on CRAN if a newer version is available or install the development version from GitHub using:"),
      "  remotes::install_github(\"IQSS/dataverse-client-r\")"
    ), collapse = "\n")
    return(msg)
  }
  invisible(TRUE)
}
