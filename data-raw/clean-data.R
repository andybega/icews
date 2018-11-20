
library("tidyverse")

cameo_codes <- read_csv("data-raw/cameo-codes.csv") %>%
  select(-X1, -eventtype_ID, -root_code, -nsLeft, -nsRight, -goldstein) %>%
  rename(cameo_code = code) %>%
  mutate(level = nchar(cameo_code) - 2,
         lvl0 = as.integer(substr(cameo_code, 1, 2)),
         lvl1 = as.integer(substr(cameo_code, 1, 3))) %>%
  mutate(lvl1 = ifelse(level==0, NA, lvl1)) %>%
  select(cameo_code, name, level, lvl0, lvl1, description, usage_notes, example, everything())

# Add in quad and penta codes
cameo_codes <- cameo_codes %>%
  mutate(quad_category = case_when(
    lvl0 %in% 1:5 ~ "verbal cooperation",
    lvl0 %in% 6:8 ~ "material cooperation",
    lvl0 %in% 9:13 ~ "verbal conflict",
    lvl0 %in% 14:20 ~ "material conflict",
    TRUE ~ NA_character_
  )) %>%
  mutate(penta_category = case_when(
    lvl0 %in% 1 ~ "statement",
    lvl0 %in% 2:5 ~ "verbal cooperation",
    lvl0 %in% 6:8 ~ "material cooperation",
    lvl0 %in% 9:13 ~ "verbal conflict",
    lvl0 %in% 14:20 ~ "material conflict",
    TRUE ~ NA_character_
  ))

cameo_codes <- cameo_codes %>%
  select(everything(), quad_category, penta_category, usage_notes, example, order)

goldstein_mappings <- read_csv("data-raw/cameo-codes.csv") %>%
  select(code, name, goldstein, nsLeft, nsRight)
attr(goldstein_mappings, "spec") <- NULL

usethis::use_data(cameo_codes, overwrite = TRUE)
usethis::use_data(goldstein_mappings, overwrite = TRUE)
