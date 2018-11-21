#' Execute one of the included SQL definition files at inst/sql
#'
#' @param x Name of one of the SQL files at inst/sql
#' @template dbp
execute_sql <- function(x, db_path) {
  sql_str <- read_sql_statements(x)
  execute_sql_statements(sql_str, db_path)
  invisible(sql_str)
}

#' Execute a vector of SQL statements
#'
#' @param x A vector containing SQL statements
#' @template dbp
execute_sql_statements <- function(x, db_path) {
  con <- connect(db_path)
  on.exit(DBI::dbDisconnect(con))

  for (i in x) {
    res <- DBI::dbSendQuery(con, i)
    DBI::dbClearResult(res)
  }
  invisible(NULL)
}

#' Read one of the included SQL files at inst/sql and split it into a vector
#' of separate SQL statements
#'
#' @param x Name of one of the SQL files at inst/sql
#'
#' @importFrom readr read_file
read_sql_statements <- function(x) {
  path <- system.file("sql", x, package = "icews")
  sql_str <- read_file(path)
  sql_str <- strsplit(sql_str, "\n\n")[[1]]
  # eliminate comment lines
  comment <- grepl("(/\\*)|(\\*/)|(--)", sql_str)
  sql_str <- sql_str[!comment]
  sql_str
}
