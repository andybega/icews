#
#   Functions related to planning state changes, e.g. download files, delete
#   database records, etc.
#

#' Plan class maker
#'
#' @param x a tibble
#' @keywords internal
create_plan <- function(x) {
  class(x) <- c("icews_plan", class(x))
  x
}

#' Format method for plan
#'
#' @param x An object of class "icews_plan"
#' @param ... Not used
#'
#' @importFrom rlang .data
#' @method format icews_plan
#' @keywords internal
format.icews_plan <- function(x, ...) {
  plan <- x

  nothing_to_do_msg <- "All files are current with the DVN versions, nothing to download\n"

  local_files    <- length(unique(plan$file_name[plan$in_local]))
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
        file   = ifelse(.data$action=="download", .data$dvn_file_label, .data$file_name)
        data.frame(msg = sprintf("%-19s '%s'\n", action, file), stringsAsFactors = FALSE)
      })
    plan_msg <- c("\nPlan:\n", msgs$msg)
  } else {
    plan_msg <- nothing_to_do_msg
  }

  str <- c(paste0(summary_string, sep = "", collapse = ""), plan_msg)
  str
}

#' Print method for plan
#'
#' @param x A tibble containing the plan output
#' @param ... Not used
#'
#' @method print icews_plan
#' @keywords internal
print.icews_plan <- function(x, ...) {
  str <- format(x)
  cat(paste0(str, collapse = ""))
}


# Applied functions -------------------------------------------------------


#' Identify dataset contained in file
#'
#' Identify which time period is nominally covered by a file. This is kept
#' around from prior version of the package, in case it becomes useful again.
#' E.g. to allow for time range specific downloading.
#'
#' @param x a normalized file name
#'
#' @keywords internal
parse_dataset <- function(x) {
  data_set <- rep(NA_character_, length(x))
  is_data_mask <- is_data_file(x)
  out <- gsub("(.[0-9]{14})|(events.)|(-icews-events)|(.tab)|(.sample)", "", x[is_data_mask])
  data_set[is_data_mask] <- out
  data_set
}


#' Plan file changes
#'
#' Plan file changes related to download/updating
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#' @keywords internal
plan_file_changes <- function(raw_file_dir = find_raw()) {

  action <- NULL

  dvn_state   <- get_dvn_state()
  local_state <- get_local_state(raw_file_dir) %>%
    # add a dummy indicator to ID which is local available
    mutate(in_local = TRUE)

  file_state <- dplyr::full_join(dvn_state, local_state,
                                 by = "file_name") %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "file system",
                  on_dvn = !is.na(dvn_file_id),
                  in_local = ifelse(is.na(in_local), FALSE, in_local),
                  data_set = parse_dataset(file_name)) %>%
    dplyr::select(file_name, action, where, on_dvn, in_local,
                  dvn_repo, dvn_file_label, dvn_file_id,
                  data_set) %>%
    dplyr::arrange(data_set, file_name)

  # Determine any actions that need to be performed
  full_plan <- file_state %>%
    dplyr::mutate(
      action = dplyr::case_when(
        in_local==FALSE ~ "download",
        on_dvn==FALSE   ~ "remove",
        TRUE ~ "none"),
      action = factor(action, levels =  c("none", "download", "remove")))

  # ad hoc fix for missing events.2017 file on dataverse (#57)
  missing_on_dvn <- !any(stringr::str_detect(dvn_state$dvn_file_label, "events\\.2017"))
  in_local <- 2017 %in% full_plan$data_set
  if (missing_on_dvn & in_local) {
    full_plan[full_plan$data_set==2017, ][["action"]] <- "none"
  }

  # Clean up file state so it is easier to read
  lvls <- c("none", "download", "delete", "ingest_from_file",
            "ingest_from_memory", "remove")
  full_plan$action = factor(full_plan$action, levels = lvls)
  full_plan <- full_plan[order(full_plan$data_set, full_plan$action), ]

  full_plan <- create_plan(full_plan)
  return(full_plan)
}


#' Plan database sync
#'
#' Plan database synchronization with local files, without downloading any
#' new files
#'
#' @param db_path Path to SQLite database file
#' @param raw_file_dir Path to raw data file directory.
#' @keywords internal
plan_database_sync <- function(db_path      = find_db(),
                               raw_file_dir = find_raw()) {

  local_state <- get_local_state(raw_file_dir) %>%
    mutate(in_local = TRUE)
  # if there were any errros on a previous update, the DB state may be old
  update_stats(db_path)
  db_state    <- get_db_state(db_path) %>%
    mutate(in_db = TRUE)

  full_plan <- dplyr::full_join(local_state, db_state,
                                by = "file_name") %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "in database",
                  in_local = ifelse(is.na(in_local), FALSE, in_local),
                  in_db    = ifelse(is.na(in_db), FALSE, in_db),
                  data_set = parse_dataset(file_name)) %>%
    dplyr::select(file_name, action, where, in_local, in_db,
                  data_set) %>%
    dplyr::arrange(data_set, file_name)

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

  # Sort the plan so that we remove DB records before trying to ingest new
  # records. Because each file can contain events prior to it's nominal coverage
  # (e.g. the old daily event files sometimes contained events prior to the
  # day they were for) delete all DB records before trying to ingest new files.
  full_plan <- full_plan %>%
    arrange(action, where, file_name)

  full_plan <- create_plan(full_plan)
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
#' @keywords internal
plan_database_changes <- function(db_path      = find_db(),
                                  raw_file_dir = find_raw(),
                                  keep_files   = getOption("icews.keep_files"),
                                  use_local    = TRUE) {

  dvn_state <- get_dvn_state() %>%
    mutate(on_dvn = TRUE)
  # if there were any errros on a previous update, the DB state may be old
  update_stats(db_path)
  db_state  <- get_db_state(db_path) %>%
    mutate(in_db = TRUE)

  file_state <- dplyr::full_join(dvn_state, db_state,
                                 by = "file_name") %>%
    dplyr::mutate(action = NA_character_,
                  # Note where the action is to be performed
                  where  = "in database",
                  on_dvn = ifelse(is.na(on_dvn), FALSE, on_dvn),
                  in_db  = ifelse(is.na(in_db), FALSE, in_db),
                  data_set = parse_dataset(file_name)) %>%
    dplyr::select(file_name, action, where, on_dvn, in_db,
                  data_set,
                  dvn_repo, dvn_file_label, dvn_file_id,) %>%
    dplyr::arrange(data_set, file_name)

  if (isTRUE(use_local) | isTRUE(keep_files)) {
    local_plan <- plan_file_changes(raw_file_dir) %>%
      # to avoid factor/char merge warning
      mutate(action = as.character(action))
    full_plan  <- dplyr::bind_rows(local_plan, file_state) %>%
      # fill in missing in_db, in_local values from row binding
      dplyr::group_by(file_name) %>%
      dplyr::mutate(in_db    = TRUE %in% in_db,
                    in_local = TRUE %in% in_local) %>%
      ungroup() %>%
      select(file_name, action, where, data_set, on_dvn, in_db, everything()) %>%
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

  # ad hoc fix for missing events.2017 file on dataverse (#57)
  missing_on_dvn <- !any(stringr::str_detect(dvn_state$dvn_file_label, "events\\.2017"))
  in_local <- 2017 %in% full_plan$data_set
  if (missing_on_dvn & in_local) {
    fix <- full_plan$data_set==2017 & full_plan$where=="in database" & full_plan$action=="delete"
    if (any(fix)) full_plan[fix, ][["action"]] <- "none"
  }

  # Clean up file state so it is easier to read
  lvls <- c("none", "download", "delete", "ingest_from_file",
            "ingest_from_memory", "remove")
  full_plan$action = factor(full_plan$action, levels = lvls)
  full_plan <- full_plan[order(full_plan$data_set, full_plan$action), ]

  # Sort the plan so that we remove DB records before trying to ingest new
  # records. Because each file can contain events prior to it's nominal coverage
  # (e.g. the old daily event files sometimes contained events prior to the
  # day they were for) delete all DB records before trying to ingest new files.
  full_plan <- full_plan %>%
    arrange(action, where, file_name)

  full_plan <- create_plan(full_plan)
  full_plan
}

#' @keywords internal
execute_plan <- function(plan, raw_file_dir, db_path) {

  need_action <- plan[plan$action!="none", ]

  # Exit if no actions are needed
  if (nrow(need_action)==0) {
    cat("No changes required\n")
    return(invisible(TRUE))
  }

  # Createa raw directory if needed
  if (any(need_action$action=="download")) {
    if (!dir.exists(raw_file_dir)) {
      dir.create(raw_file_dir)
    }
  }

  # Otherwise, proceed through the action items
  for (i in 1:nrow(need_action)) {
    task <- need_action[i, ]

    if (task$action=="download") {
      cat(sprintf("Downloading '%s'\n", task$dvn_file_label))
      f <- download_file(file = task$dvn_file_label, to_dir = raw_file_dir,
                         repo = task$dvn_repo, file_id = task$dvn_file_id,
                         new_name = task$file_name)
      next
    }

    if (task$action=="remove") {
      cat(sprintf("Removing '%s'\n", task$file_name))
      remove_file(file.path(raw_file_dir, task$file_name))
      next
    }

    if (task$action=="delete") {
      cat(sprintf("Deleting DB records from '%s'\n", task$file_name))
      delete_events(task$file_name, db_path)
      next
    }

    if (task$action=="ingest_from_file") {
      cat(sprintf("Ingesting records from '%s'\n", task$file_name))
      ingest_from_file(file.path(raw_file_dir, task$file_name), db_path)
      next
    }

    if (task$action=="ingest_from_memory") {
      cat(sprintf("Ingesting records from '%s'\n", task$file_name))
      ingest_from_memory(task$file_name, db_path)
      next
    }

    stop(sprintf("Unknown action '%s', I should not be here", task$action))
  }

  # Optimize database if any changes were made
  optimize <- vacuum <- FALSE
  if (any(need_action$action!="none" & need_action$where=="in database")) {
    optimize <- TRUE
  }
  if (any(need_action$action=="delete")) {
    vacuum <- TRUE
  }
  if (any(vacuum, optimize)) {
    cat("Cleaning up and optimizing database, this might take a while\n")
    optimize_db(db_path, vacuum = vacuum, optimize = optimize)
  }

  cat("\nComplete\n")
  invisible(TRUE)
}
