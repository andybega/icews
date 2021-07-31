#' Dataverse API token
#'
#' Check that the Dataverse API token is set and if so show it
#'
#' @details An API token is required for the R dataverse client to work. The
#' token can be obtained by signing in to your Dataverse account and selecting
#' "API token" from the dropdown menu. The token should then be used to set
#' the DATAVERSE_KEY option, i.e. `Sys.setenv("DATAVERSE_KEY = '{api key}')`.
#' I do this in my R profile file so it's automatically done for every R
#' session (`usethis::edit_r_profile()`).
#'
#' \url{https://guides.dataverse.org/en/latest/user/account.html#api-token}
#'
#' \url{https://github.com/IQSS/dataverse-client-r#keys}
#'
#' @returns The token. If the token is not set, an error message will be thrown.
#'
#' @export
dataverse_api_token <- function() {
  token <- Sys.getenv("DATAVERSE_KEY")
  if (token=="") stop("API token is not set. See ?dataverse_api_token")
  token
}
