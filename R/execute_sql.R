#' Execute one of the included SQL definition files at inst/sql
#'
#' @param x Name of one of the SQL files at inst/sql
#' @template dbp
#'
#' @importFrom readr read_file
execute_sql <- function(x, db_path) {
  # read from file
  path <- system.file("sql", x, package = "icews")
  str  <- read_file(path)

  # parse and execute statements
  sql_str <- read_sql_statements(str)
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
#' @param x Path to a .sql file containing SQL statements (not queries)
read_sql_statements <- function(x) {
  sql_str <- strsplit(x, "\n\n")[[1]]
  # eliminate comment lines
  comment <- grepl("(/\\*)|(\\*/)|(--)", sql_str)
  sql_str <- sql_str[!comment]
  sql_str
}
