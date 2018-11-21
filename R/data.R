#' Get ICEWS dataverse DOI
#'
#' Contains the DVN DOI for ICEWS
#'
#' @examples
#' get_doi()
#'
#' @export
get_doi <- function() {
  "doi:10.7910/DVN/28075"
}

#' CAMEO code dictionary
#'
#' CAMEO event type code dictionary
#'
#' @format Data frame
#' \describe{
#'  \item{cameo_code}{CAMEO code; the codes need to remain as a character. Coercing
#'    to integer will create ambiguity, e.g. '010 / Make statement' and
#'    '10 / DEMAND' will both collapse to 10.}
#'  \item{name}{CAMEO code name}
#'  \item{level}{Hierarchy level of this code.}
#'  \item{lvl0}{Parent root code.}
#'  \item{lvl1}{Parent root code.}
#'  \item{description}{Description}
#'  \item{usage_notes}{Usage notes.}
#'  \item{example}{Example sentence from which this event type is parsed.}
#'  \item{order}{Sort order of codes}
#'  \item{quad_category}{Quad category grouping event types into verbal/material
#'    cooperation/conflict.}
#'  \item{penta_category}{Derived from quad category, but root code '01' statements
#'    in their own category, as these usually make up the bulk of events.}
#' }
#'
#' @source
#' \url{http://eventdata.parusanalytics.com/data.dir/cameo.html}
#'
#' \url{https://doi.org/10.7910/DVN/28075}
#'
"cameo_codes"


#' CAMEO event Goldstein scores
#'
#' List mapping CAMEO codes to Goldstein scores
#'
#' @source
#' \url{http://eventdata.parusanalytics.com/data.dir/cameo.html}
#'
#' \url{https://doi.org/10.7910/DVN/28075}
#'
#' @examples
#' data(goldstein_mappings)
#' head(goldstein_mappings)
"goldstein_mappings"

#' ICEWS event data sample
#'
#' A sample of the ICEWS event data consisting of the 4,993 events between 1 and
#' 3 June 2018.
#'
#' @source
#' \url{https://doi.org/10.7910/DVN/28075}
#'
#' @examples
#' data("icews_sample")
#' str(icews_sample)
"icews_sample"
