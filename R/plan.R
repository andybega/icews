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

#' Plan file changes
#'
#' Plan file changes related to download/updating
#'
#' @param raw_file_dir Directory containing the raw event TSV files
#' @keywords internal
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
      delete_events(task$file, db_path)
      next
    }

    if (task$action=="ingest_from_file") {
      cat(sprintf("Ingesting records from '%s'\n", task$file))
      ingest_from_file(file.path(raw_file_dir, task$file), db_path)
      next
    }

    if (task$action=="ingest_from_memory") {
      cat(sprintf("Ingesting records from '%s'\n", task$file))
      ingest_from_memory(task$file, db_path)
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
