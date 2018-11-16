.onLoad <- function(...) {
  opts <- get_icews_opts()
  msg <- format(opts)
  packageStartupMessage(paste0(msg, collapse = "\n"))
}
