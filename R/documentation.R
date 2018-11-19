
#' Download documentation
#'
#' Download documentation files
#'
#' @param docs_dir Directory containing the documentation files.
#'
#' @export
download_docs <- function(docs_dir = find_docs()) {
  remote_state <- get_dvn_manifest()
  remote_docs  <- remote_state$files[remote_state$files$category=="Documentation", ]
  for (f in remote_docs$label) {
    download_file(f, to_dir = docs_dir)
  }
  invisible(NULL)
}
