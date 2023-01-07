library(tidyverse)

# Import all the CSV files as one dataframe
csv_path <- "Raw Song CSVs/"

csv_file_names <-
  csv_path %>%
  paste0(list.files(csv_path))

songs_raw <- do.call(rbind,lapply(csv_file_names,read_csv))

# Clean up data--remove duplicates and remove blanks
songs_clean <- 
  songs_raw %>% 
  distinct(song, artist, .keep_all = TRUE) %>% 
  na.omit()

# Export full collection
write_csv(songs_clean,
          paste0("songs from channel 66 - ",
                 Sys.time(), ".csv"))

# check if there is a pattern of when the new songs are played

songs_raw %>% 
  group_by(song, artist) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ggplot(aes(time_stamp)) +
  geom_histogram()

songs_raw %>% 
  group_by(song, artist) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  mutate(day = as.Date(time_stamp)) %>% 
  count(day)

# Where there much more unqiue songs because it's Christmas or because they had "I am the Captain Now"?
# I can check this by seeing what time of day these new songs were played and comparing that to the air time for the show.

songs_raw %>% 
  group_by(song, artist) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  mutate(day = as.Date(time_stamp)) %>% 
  filter(day == as.Date("2022-12-25")) %>% 
  ggplot(aes(time_stamp)) +
  geom_histogram()

# Seems to be all day, let's just look at what the songs are.

songs_raw %>% 
  group_by(song, artist) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>% 
  mutate(day = as.Date(time_stamp)) %>% 
  filter(day <= as.Date("2022-12-25")) %>% View()

