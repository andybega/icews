#' Parse a data file label
#'
#' Parse the DVN file label to identify whether it contains data and if so which
#' event set and file version.
#'
#' @param label character string
#'
#' @return A data frame with components:
#' - label
#' - file
#' - is_date: TRUE or FALSE
#' - dataset
#' - version
#'
#' @md
#' @import tibble
parse_label <- function(label) {
  out <- tibble::tibble(
    label   = label,
    file    = gsub(".zip", "", label),
    is_data = grepl(".tab", label),
    data_set = NA,
    version  = NA
  )
  out$data_set[out$is_data] <- gsub(".[0-9]{14}.tab", "", out$file[out$is_data])
  out$version[out$is_data]  <- gsub("(events.[0-9]{4}.)|(.tab)", "", out$file[out$is_data])
  out
}


#' Get Dataverse file list
#'
#' Get information on current ICEWS files on Dataverse
#'
#' @export
#' @import dataverse
#' @import tibble
get_dvn_manifest <- function() {
  icews_doi  <- get_doi()
  dvn_files  <- dataverse::get_dataset(icews_doi)

  file_list <- tibble::tibble(
    label = dvn_files$files$label,
    category = unlist(dvn_files$files$categories),
    description = dvn_files$files$description
  )

  data_file_list <- parse_label(file_list$label)
  data_file_list <- subset(data_file_list, is_data)
  data_file_list$is_data <- NULL

  list(
    files             = file_list,
    data_files        = data_file_list,
    dataverse_dataset = dvn_files
  )
}


#' Get local file state
#'
#' Determine what data files are present locally, and which event sets and
#' versions they contain.
#'
#' @param raw_file_dir Directory containing raw data files
get_local_state <- function(raw_file_dir = find_raw()) {
  files <- dir(raw_file_dir)
  state <- parse_label(files)
  if (!isTRUE(all(state$is_data))) {
    stop(sprintf("unexpected non-data files found in '%s'", raw_file_dir))
  }
  state$label <- state$is_data <- NULL
  colnames(state) <- paste0("local_", colnames(state))
  state
}


#' Get database state
#'
#' Determine what data files, event sets, and version are currently in the
#' database.
#'
#' @param db_path Path to SQLite database file
get_db_state <- function(db_path = find_db()) {
  db_files <- list_source_files(db_path)
  state <- parse_label(db_files)
  state$label <- state$is_data <- NULL
  colnames(state) <- paste0("db_", colnames(state))
  state
}


#' Plan file changes
#'
#' Plan file changes related to download/updating
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#'
plan_file_changes <- function(raw_file_dir) {

  action <- NULL

  dvn_state <- get_dvn_manifest()$data_files
  colnames(dvn_state) <- paste0("dvn_", colnames(dvn_state))
  local_state <- get_local_state(raw_file_dir)

  file_state <- dplyr::full_join(dvn_state, local_state,
                                 by = c("dvn_file" = "local_file")) %>%
    dplyr::mutate(data_set = ifelse(is.na(dvn_data_set), local_data_set, dvn_data_set)) %>%
    dplyr::rename(file = dvn_file) %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "file system",
                  on_dvn = !is.na(dvn_version),
                  in_local = !is.na(local_version)) %>%
    dplyr::select(file, action, where, data_set, on_dvn, in_local, dvn_label) %>%
    dplyr::arrange(data_set, file)

  # Determine any actions that need to be performed
  full_plan <- file_state %>%
    dplyr::mutate(
      action = dplyr::case_when(
        in_local==FALSE ~ "download",
        on_dvn==FALSE   ~ "remove",
        TRUE ~ "none"),
      action = factor(action, levels =  c("none", "download", "remove")))

  # Clean up file state so it is easier to read
  lvls <- c("none", "download", "delete", "ingest_from_file",
            "ingest_from_memory", "remove")
  full_plan$action = factor(full_plan$action, levels = lvls)
  full_plan <- full_plan[order(full_plan$data_set, full_plan$action), ]

  #class(full_plan) <- "plan"
  return(full_plan)
}


#' Plan database sync
#'
#' Plan database synchronization with local files, without downloading any
#' new files
#'
#' @param db_path Path to SQLite database file
#' @param raw_file_dir Path to raw data file directory.
plan_database_sync <- function(db_path      = find_db(),
                               raw_file_dir = find_raw()) {

  local_state <- get_local_state(raw_file_dir)
  db_state    <- get_db_state(db_path)

  full_plan <- dplyr::full_join(local_state, db_state,
                                 by = c("local_file" = "db_file")) %>%
    dplyr::mutate(data_set = ifelse(is.na(local_data_set), db_data_set, local_data_set)) %>%
    dplyr::rename(file = local_file) %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "in database",
                  in_local = !is.na(local_version),
                  in_db    = !is.na(db_version)) %>%
    dplyr::select(file, action, where, data_set, in_local, in_db) %>%
    dplyr::arrange(data_set, file)

  # Determine DB actions that need to be performed;
  full_plan <- full_plan %>%
    dplyr::group_by(data_set) %>%
    dplyr::mutate(
      action = dplyr::case_when(
        where=="in database" & in_local  & in_db   ~ "none",
        where=="in database" & !in_local & in_db   ~ "delete",
        where=="in database" & in_local  & !in_db  ~ "ingest_from_file",
        TRUE ~ action)
    ) %>%
    dplyr::ungroup()

  # Clean up file state so it is easier to read
  lvls <- c("none", "download", "delete", "ingest_from_file",
            "ingest_from_memory", "remove")
  full_plan$action = factor(full_plan$action, levels = lvls)
  full_plan <- full_plan[order(full_plan$data_set, full_plan$action), ]

  #class(full_plan) <- "plan"
  full_plan
}


#' Plan database changes
#'
#' Plan database changes related to download/updating
#'
#' @param db_path Path to SQLite database file
#' @param raw_file_dir Path to raw data file directory.
#' @param keep_files Retain raw data files?
#' @param use_local Use local files if available?
#'
#' @md
plan_database_changes <- function(db_path      = find_db(),
                                  raw_file_dir = find_raw(),
                                  keep_files   = getOption("icews.keep_files"),
                                  use_local    = TRUE) {

  dvn_state <- get_dvn_manifest()$data_files
  colnames(dvn_state) <- paste0("dvn_", colnames(dvn_state))
  db_state  <- get_db_state(db_path)

  file_state <- dplyr::full_join(dvn_state, db_state,
                                 by = c("dvn_file" = "db_file")) %>%
    dplyr::mutate(data_set = ifelse(is.na(dvn_data_set), db_data_set, dvn_data_set)) %>%
    dplyr::rename(file = dvn_file) %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "in database",
                  on_dvn = !is.na(dvn_version),
                  in_db = !is.na(db_version)) %>%
    dplyr::select(file, action, where, data_set, on_dvn, in_db, dvn_label) %>%
    dplyr::arrange(data_set, file)

  if (isTRUE(use_local) | isTRUE(keep_files)) {
    local_plan <- plan_file_changes(raw_file_dir) %>%
      # to avoid factor/char merge warning
      mutate(action = as.character(action))
    full_plan  <- dplyr::bind_rows(local_plan, file_state) %>%
      # fill in missing in_db, in_local values from row binding
      dplyr::group_by(file) %>%
      dplyr::mutate(in_db    = TRUE %in% in_db,
                    in_local = TRUE %in% in_local) %>%
      ungroup() %>%
      select(file, action, where, data_set, on_dvn, in_db, everything()) %>%
      arrange(data_set, action)
  } else {
    # use a dummy plan with no planned actions as stand in when not using
    # local files
    full_plan <- file_state %>%
      mutate(action   = NA_character_,
             where    = "file system",
             in_local = FALSE) %>%
      select(file, action, data_set, everything())
  }

  # adjust for discard files option
  if (!keep_files) {
    # Never download so that all ingests are from memory
    full_plan$action[full_plan$where=="file system" & full_plan$action=="download"] <- "none"
    # remove all local files
    full_plan$action[full_plan$where=="file system" & full_plan$in_local==TRUE] <- "remove"
  }

  # Determine DB actions that need to be performed;
  full_plan <- full_plan %>%
    dplyr::group_by(data_set) %>%
    dplyr::mutate(
      action = dplyr::case_when(
        is.na(action) & on_dvn  & in_db  ~ "none",
        is.na(action) & !on_dvn & in_db  ~ "delete",
        is.na(action) & on_dvn  & !in_db & ("download" %in% action | in_local) ~ "ingest_from_file",
        is.na(action) & on_dvn  & !in_db & !("download" %in% action | in_local) ~ "ingest_from_memory",
        TRUE ~ action)
    ) %>%
    dplyr::ungroup()

  # Clean up file state so it is easier to read
  lvls <- c("none", "download", "delete", "ingest_from_file",
            "ingest_from_memory", "remove")
  full_plan$action = factor(full_plan$action, levels = lvls)
  full_plan <- full_plan[order(full_plan$data_set, full_plan$action), ]

  #class(full_plan) <- "plan"
  full_plan
}

#' Print a summary of a plan
#'
#' Summarize the actions to be performed in a plan
#'
#' @param plan A tibble containing the plan output
print_plan <- function(plan) {

  nothing_to_do_msg <- "All files are current with the DVN versions, nothing to download\n"

  local_files    <- length(unique(plan$file[plan$in_local]))
  download_files <- sum(plan$where=="file system" & plan$action=="download")
  remove_files   <- sum(plan$where=="file system" & plan$action=="remove")

  db_ingest_file <- sum(plan$where=="in database" & plan$action=="ingest_from_file")
  db_ingest_mem  <- sum(plan$where=="in database" & plan$action=="ingest_from_memory")
  db_delete      <- sum(plan$where=="in database" & plan$action=="delete")

  summary_string <- list(
    "File system changes:\n",
    sprintf("Found %s local data file(s)\n", local_files),
    sprintf("Downloading %s file(s)\n",      download_files),
    sprintf("Removing %s old file(s)\n",     remove_files)
  )

  if ("in database" %in% plan$where) {
    summary_string <- c(
      summary_string,
      list(
        "\nDatabase changes:\n",
        sprintf("Deleting old records for %s file(s)\n", db_delete),
        sprintf("Ingesting records from %s file(s)\n", db_ingest_file + db_ingest_mem)
      ))
  }

  transaction_string <- list(
    NULL
  )
  if (any(plan$action!="none")) {
    msgs <- plan %>%
      dplyr::filter(action!="none") %>%
      dplyr::mutate(action = as.character(action)) %>%
      dplyr::rowwise() %>%
      dplyr::do({
        action = switch(.data$action,
                         download = "Download", remove = "Remove",
                         delete = "Delete records from",
                         ingest_from_file = "Ingest records from",
                         ingest_from_memory = "Ingest records from")
        file   = ifelse(.data$action=="download", .data$dvn_label, .data$file)
        data.frame(msg = sprintf("%-19s '%s'\n", action, file), stringsAsFactors = FALSE)
      })
    plan_msg <- c("\nPlan:\n", msgs$msg)
  } else {
    plan_msg <- nothing_to_do_msg
  }
  cat(paste0(summary_string, sep = "", collapse = ""))
  cat(plan_msg, sep = "")
  invisible(plan)
}

execute_plan <- function(plan, raw_file_dir, db_path) {

  need_action <- plan[plan$action!="none", ]

  # Exit if no actions are needed
  if (nrow(need_action)==0) {
    cat("No changes required\n")
    return(invisible(TRUE))
  }

  # Otherwise, proceed through the action items
  for (i in 1:nrow(need_action)) {
    task <- need_action[i, ]

    if (task$action=="download") {
      cat(sprintf("Downloading '%s'\n", task$dvn_label))
      f <- download_file(task$dvn_label, raw_file_dir)
      next
    }

    if (task$action=="remove") {
      cat(sprintf("Removing '%s'\n", task$file))
      remove_file(file.path(raw_file_dir, task$file))
      next
    }

    if (task$action=="delete") {
      cat(sprintf("Deleting DB records from '%s'\n", task$file))
      delete_events(task$file)
      next
    }

    if (task$action=="ingest_from_file") {
      cat(sprintf("Ingesting records from '%s'\n", task$file))
      ingest_from_file(file.path(raw_file_dir, task$file))
      next
    }

    if (task$action=="ingest_from_memory") {
      cat(sprintf("Ingesting records from '%s'\n", task$file))
      ingest_from_memory(task$file)
      next
    }

    stop(sprintf("Unknown action '%s', I should not be here", task$action))
  }

  cat("\nComplete\n")
  invisible(TRUE)
}
