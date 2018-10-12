
library("tidyverse")

cameo_codes <- read_csv("data-raw/cameo-codes.csv") %>%
  select(-X1, -eventtype_ID, -root_code, -nsLeft, -nsRight, -goldstein) %>%
  mutate(level = nchar(code) - 2,
         lvl0 = substr(code, 1, 2),
         lvl1 = substr(code, 1, 3)) %>%
  mutate(lvl1 = ifelse(level==0, NA, lvl1)) %>%
  select(code, name, level, lvl0, lvl1, description, usage_notes, example, everything())

goldstein_mappings <- read_csv("data-raw/cameo-codes.csv") %>%
  select(code, name, goldstein, nsLeft, nsRight)

use_data(cameo_codes, overwrite = TRUE)
use_data(goldstein_mappings, overwrite = TRUE)
