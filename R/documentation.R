
#' Download documentation
#'
#' Download documentation files. This is not done by [update_icews()], i.e. it
#' has to be done manually if you want the documentation files.
#'
#' @template dd
#' @template quiet
#'
#' @details `download_docs` will always download the current set of documentation
#' files on dataverse. There is no update functionality, i.e. existing files
#' will always be silently overwritten.
#'
#' @return Invisibly returns the "docs_dir" argument value.
#'
#' @examples
#' \dontrun{
#' docs_path <- download_docs(quiet = FALSE)
#' dir(docs_path)
#' }
#'
#' @export
#' @md
download_docs <- function(docs_dir = find_docs(), quiet = TRUE) {
  manifest <- get_dvn_manifest()
  remote_docs  <- manifest$file_list[manifest$file_list$category=="Documentation", ]
  if (!quiet) {
    cat(sprintf("Downloading %s file(s) to '%s'\n", nrow(remote_docs), docs_dir))
  }
  for (f in remote_docs$label) {
    if (!quiet) {
      cat(sprintf("Downloading '%s'\n", f))
    }
    download_file(f, to_dir = docs_dir)
  }
  invisible(docs_dir)
}
