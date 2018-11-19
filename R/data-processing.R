
#' Translate country to G&W country codes
#'
#' Translate country names to Gleditsch & Ward country codes.
#'
#' @param country The character country name in ICEWS
#' @param date Event date; some countries like Yugoslavia/Serbia and Montenegro/Serbia
#'   change over time.
#'
#' @importFrom countrycode countrycode
#' @import dplyr
#' @export
#' @md
#' @examples
#' # Territories are correctly associated with metropole
#' df <- dplyr::tibble(
#'   country = c("United States", "Puerto Rico", "Guam"),
#'   event_date = rep(as.Date("2018-01-01")),
#'   stringsAsFactors = FALSE)
#' df$gwcode <- icews_to_gwcode(df$country, df$event_date)
#' df
#'
#' # Time-varying translation
#' df <- dplyr::tibble(
#'   country = c("Serbia", "Serbia"),
#'   event_date = as.Date(c("2006-06-04", "2006-06-05")),
#'   stringsAsFactors = FALSE)
#' df$gwcode <- icews_to_gwcode(df$country, df$event_date)
#' df
icews_to_gwcode <- function(country, date) {
  df <- tibble(country = as.character(country), date = date)
  # import constant mappings
  df <- dplyr::left_join(df, country_to_gwcode, by = "country")
  # code time-varying mappings
  df <- mutate(df, gwcode = case_when(
      country=="Serbia" & date < "2006-06-05" ~ 345L,
      country=="Serbia" & date > "2006-06-04" ~ 340L,
      country=="Hong Kong" & date < "1997-07-01" ~ 200L,
      country=="Hong Kong" & date > "1997-06-30" ~ 710L,
      country=="Macau" & date < "1999-12-20" ~ 235L,
      country=="Macau" & date > "1999-12-19" ~ 710L,
      TRUE ~ gwcode
    ))
  df[["gwcode"]]
}

# time varying Serbia, Hong Kong, Macau
