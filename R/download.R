#' Download ICEWS data files
#'
#' Download the ICEWS event data from Dataverse
#'
#' @param path Path to directory where data files will be downloaded to.
#' @param overwrite Should duplicate files that are already at path be
#'   re-downloaded?
#' @param dryrun Conducts a dry run listing proposed changes, without actually
#'   downloading or deleting anything.
#'
#' @export
#' @import dataverse
#' @import dplyr
#' @importFrom utils unzip
#' @importFrom rlang .data
download_icews <- function(path = NULL, overwrite = FALSE,
                           dryrun = FALSE) {
  icews_doi <- "doi:10.7910/DVN/28075"
  nothing_to_do_msg <- "All files are current with the DVN versions, nothing to download\n"

  if (is.null(path)) {
    path <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }

  state <- reconcile_state(path)

  if (isTRUE(overwrite)) {
    state$files$action <- ifelse(state$files$action=="none", "download", state$files$action)
    state$files$action <- factor(state$files$action, levels = c("none", "download", "remove"))
  }

  if (isTRUE(dryrun)) {
    actions <- table(state$files$action)
    cat(sprintf("Found %s local data file(s)\n", sum(actions)))
    if (actions[["download"]] > 0) {
      suffix <- ifelse(isTRUE(overwrite), ", because overwrite was set to TRUE", "")
      msg <- sprintf("Downloading %s new file(s)%s\n", sum(actions[["download"]]), suffix)
      cat(msg)
    }
    if (actions[["remove"]] > 0) {
      cat(sprintf("Removing %s old local file(s)\n", sum(actions[["remove"]])))
    }
    if (actions[["remove"]] + actions[["download"]] > 0) {
      msgs <- state$files %>%
        dplyr::filter(action!="none") %>%
        dplyr::rowwise() %>%
        dplyr::do({
          action <- ifelse(.data$action=="download", "Download", "Remove")
          file   <- ifelse(.data$action=="download", .data$dvn_file, .data$file)
          data.frame(sprintf("%-9s'%s'\n", action, file), stringsAsFactors = FALSE)
        }) %>%
        `[[`(1)
      cat("\nPlan:\n", msgs, sep = "")
    } else {
      cat(nothing_to_do_msg)
    }
    return(invisible(TRUE))
  }

  need_action <- state$files[state$files$action!="none", ]

  # Exit if no actions are needed
  if (nrow(need_action)==0) {
    cat(nothing_to_do_msg)
    return(invisible(TRUE))
  }

  # Otherwise, proceed through the action items
  for (i in 1:nrow(need_action)) {
    task <- need_action[i, ]

    if (task$action=="download") {

      # Get binary blob
      cat(sprintf("Downloading %s\n", task$dvn_file))
      f <- get_file(task$dvn_file, icews_doi)

      # Decide how to handle based on whether extraction is needed
      if (tools::file_ext(task$dvn_file)=="zip") {
        cat("Unzipping\n")
        tmp <- tempfile(fileext = ".tab.zip")
        writeBin(as.vector(f), tmp)
        con <- utils::unzip(tmp, exdir = path)
      } else {
        fname <- gsub(".zip", "", task$dvn_file)
        con <- file.path(path, fname)
        writeBin(as.vector(f), con)
      }

    } else if (task$action=="remove") {

      cat(sprintf("Removing %s\n", task$file))
      unlink(file.path(path, task$file))

    } else {

      stop("I should not be here")

    }
  }

  cat("File download/sync done\n")
  invisible(TRUE)
}

#' Reconcile state
#'
#' Reconcile the local and dataverse file state
#'
#' @param path Directory containing the raw event TSV files.
#'
#' @import tibble
#' @import dplyr
reconcile_state <- function(path = NULL) {

  action <- NULL

  if (is.null(path)) {
    path <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }

  manifest <- get_dvn_manifest()

  # Determine local state
  # File-based
  files_at_path <- dir(path)

  # Check for any zip files that match DVN zip file; unzip if find any
  # Don't want to do this in the loop below so the loop can download/unzip
  # one file at a time without checking each time if zip version is already
  # there
  local_zip <- files_at_path[grepl(".zip$", files_at_path) &
                               files_at_path %in% manifest$data_files$file]
  if (length(local_zip) > 0) {
    sapply(local_zip, unzip, exdir = path)

    # Update with new unzipped files
    files_at_path <- dir(path)
  }

  local_state <- tibble::tibble(
    local_file = files_at_path,
    local_data_set  = gsub(".[0-9]{14}.tab", "", files_at_path),
    local_version   = gsub("(events.[0-9]{4}.)|(.tab)", "", files_at_path)
  )

  data_state <- dplyr::full_join(manifest$data_files, local_state,
                                 by = c("data_set" = "local_data_set"))
  data_state <- data_state %>%
    dplyr::mutate(
      action = dplyr::case_when(
        version==local_version ~ "none",
        version!=local_version ~ "update",
        is.na(local_version)   ~ "download",
        TRUE ~ "none"),
      action = factor(action, levels = c("none", "update", "download")))

  file_state <- dplyr::full_join(manifest$data_files, local_state,
                                 by = c("file" = "local_file"))
  file_state <- file_state %>%
    dplyr::mutate(
      action = dplyr::case_when(
        is.na(local_version) ~ "download",
        is.na(version)       ~ "remove",
        TRUE ~ "none"),
      action = factor(action, levels =  c("none", "download", "remove")))

  list(data = data_state, files = file_state)
}

#' Get Dataverse file list
#'
#' Get information on current ICEWS files on Dataverse
#'
#' @export
#' @import dataverse
get_dvn_manifest <- function() {
  icews_doi <- "doi:10.7910/DVN/28075"
  dvn_files  <- get_dataset(icews_doi)
  data_files_dvn <- dvn_files$files$label[grepl(".tab", dvn_files$files$label)]
  data_files_tsv <- gsub(".zip", "", data_files_dvn)

  list(
    files = dvn_files$files$label,
    data_files = data.frame(
      dvn_file = data_files_dvn,
      zipped = grepl(".zip", data_files_dvn),
      file = data_files_tsv,
      data_set = gsub(".[0-9]{14}.tab", "", data_files_tsv),
      version = gsub("(events.[0-9]{4}.)|(.tab)", "", data_files_tsv),
      stringsAsFactors = FALSE
    )
  )
}

#' Purge file downloads
#'
#' Removes the downloaded raw event TSV files
#'
#' @param raw_file_dir Directory containing the raw event TSV files.
#'
#' @export
purge_data_files <- function(raw_file_dir = NULL) {
  if (is.null(raw_file_dir)) {
    raw_file_dir <- file.path(Sys.getenv("ICEWS_DATA_DIR"), "raw")
  }

  data_files <- dir(raw_file_dir, pattern = "events[\\.0-9]+.tab", full.names = TRUE)
  unlink(data_files)
  invisible(NULL)
}

