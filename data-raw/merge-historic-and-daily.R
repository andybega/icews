

library("tidyverse")
library("icews")

last_historic_drop <- 20181119

events2018_fh <- tail(dir(find_raw(), full.names = TRUE), 1)
events2018 <- read_tsv(events2018_fh) %>%
  filter(`Event Date` > as.Date("2018-07-31"))
events2018$source_file <- basename(events2018_fh)

daily_files <- dir(find_raw(), pattern = "^[0-9]{8}", full.names = TRUE)
daily <- tibble(fh = daily_files) %>%
  mutate(events = map(fh, read_tsv, col_types = cols(
    .default = col_character(),
    `Event ID` = col_double(),
    `Event Date` = col_date(format = ""),
    Intensity = col_double(),
    `Story ID` = col_double(),
    `Sentence Number` = col_double(),
    Latitude = col_double(),
    Longitude = col_double()
  ))) %>%
  unnest(events) %>%
  rename(source_file = fh) %>%
  mutate(source_file = basename(source_file))

daily %>%
  filter(`Event ID` %in% events2018$`Event ID`) %>%
  ggplot(., aes(x = `Event Date`)) +
  geom_bar()

all_events <- bind_rows(events2018, daily)
all_events <- all_events %>%
  group_by(`Event ID`) %>%
  mutate(
    N_files = length(unique(source_file)),
    Set = case_when(
    N_files==1 & unique(source_file) == "events.2018.20181119132436.tab" ~ "Historic",
    N_files==1 & str_detect(unique(source_file), "^[0-9]{8}") ~ "Daily",
    N_files > 1 & all("events.2018.20181119132436.tab" %in% source_file,
                      any(str_detect(source_file, "^[0-9]{8}"))) ~ "Both",
    TRUE ~ NA_character_
  ))
all_events %>%
  group_by(Set) %>%
  summarize(N = n())

# Two different views of the set memberships
ggplot(all_events, aes(x = `Event Date`, fill = `Set`)) +
  geom_bar() +
  theme_light()

all_events %>%
  group_by(`Event Date`, Set) %>%
  summarize(N = n()) %>%
  ggplot(., aes(x = `Event Date`, y = N, color = Set)) +
  geom_line() +
  theme_light()

# All events are in either one or two files, not more
table(all_events$N_files)

# there is an event going back to 2018-09-14 that is in both files
all_events %>%
  ungroup() %>%
  filter(Set=="Both") %>%
  summarize(min(`Event Date`), max(`Event Date`))

all_events %>%
  ungroup() %>%
  filter(Set=="Historic") %>%
  summarize(max(`Event Date`))

all_events %>%
  ungroup() %>%
  filter(Set=="Daily") %>%
  summarize(min(`Event Date`))

# from 2018-11-01 new events are only in Daily
# before 2018-09-14 new events are only in Historic
# between 2018-09-14 and 2018-10-31 events are mixed: either in only one, or both
#
# -> need to check first set of daily files and only add events whose ID is not
#    already present
# -> after some date, dont' have to check anymore; max date in historic?
#
# ingest historic files as usual
# for daily files:
#   determine max historic event date in DB
#   if daily files has events before max historic date:
#     filter for event IDs that are not already in db
#     ingest
#   else:
#     ingest
#


