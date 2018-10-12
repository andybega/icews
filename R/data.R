#' CAMEO code dictionary
#'
#' CAMEO event type code dictionary
#'
#' @format Data frame
#' \describe{
#'  \item{code}{CAMEO code; the codes need to remain as a character. Coercing
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
