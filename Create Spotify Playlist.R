library(tidyverse)
library(spotifyr)

# Set credentials
# I put my actual client id and client secret in the console
# Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

# Import track data
tracks <- 
  read_csv(list.files(pattern = "csv$") %>% tail(1))


# Get URIs of songs
search_input <- 
  tracks %>% 
  mutate(artist = str_replace_all(artist, "/", ", ")) %>% 
  mutate(query = paste0("track:", song, " ",
                        "artist:", artist))

#   This line will take some time to run since it is making as 
#   many requests as the number of tracks.
#   Wrapping the results in a list helps the code run even
#   when the query returns zero search results.
search_results <- 
  search_input %>% 
  rowwise() %>% 
  mutate(search_results = list(search_spotify(query,
                                         type = "track",
                                         limit = 1)))

# When the search_results column is unnested it will not
# create any rows for empty resutls thus filtering out 
# tracks that didn't return a match.


# Now create an empty playlist
# (I had a lot of issues getting the authentication to work here
# I ended up needing to set my Redirect URI to http://localhost:1410/
# in the Spotify developer dashboard.)
create_playlist(
  user_id = "philliphresh",
  name = "Yacht RockeR",
  description = "Songs played on Yacht Rock Radio",
  authorization = 
    get_spotify_authorization_code(scope = "playlist-modify-public")
)

# Figure out playlist id
my_playlists <- get_my_playlists()

playlist_id <- 
  my_playlists %>% 
  filter(name == "Yacht RockeR") %>% 
  pull(id)

# See what tracks are already on the playlist
#   The first time the playlist is made this should be empty 
#   but when I add more songs to the playlist it will not be.

get_current_playlist_tracks <- 
  function(playlist_id, ...) {
    
    current_playlist_tracks <- data.frame()
    
    for (i in 1:100000) {
      playlist_tracks_100 <- 
        get_playlist_tracks(playlist_id = playlist_id,
                            fields = ...,
                            limit = 100,
                            offset = nrow(current_playlist_tracks))
      
      if (playlist_tracks_100 %>% is_empty()) {break}
      
      current_playlist_tracks <- rbind(current_playlist_tracks,
                                       playlist_tracks_100)
    }
    
    return(current_playlist_tracks)
}

current_playlist_tracks <- 
  get_current_playlist_tracks(playlist_id = playlist_id,
                            c("track.name", 
                              "track.artists.name", 
                              "track.uri"))


# Remove tracks that are already on the playlist from the list to add
filtered_search_results <- 
  search_results %>% 
  unnest(search_results) %>% 
  anti_join(current_playlist_tracks, by = c("uri" = "track.uri"))

# Add tracks to playlist
#   since a maximum of 100 tracks can be added in one request, 
#   I am looping through tracks rather than adding all tracks in 
#   one call of the function.

# purrr::walk(search_results$search_results$uri, 
#             .f = function(x) add_tracks_to_playlist(playlist_id = playlist_id,
#                                                     uris = x))

# The above is not working, my guess is that it needs to add more than one track at a time

# The code below loops through 100 indices of tracks at a time
for (i in seq_along(1:(nrow(filtered_search_results) %/% 100 + 1))) {
  uris <- 
    filtered_search_results$uri[seq(1 + 100 * (i - 1), 100 * (i), 1)]
  
  uris <- uris[!is.na(uris)]
  
  add_tracks_to_playlist(playlist_id = playlist_id,
                         uris = uris)
}


# Now I can check for duplicate songs and remove them if necessary
current_playlist_tracks <- 
  get_current_playlist_tracks(playlist_id = playlist_id,
                              c("track.name", 
                                "track.artists.name", 
                                "track.uri"))


current_playlist_tracks %>% 
  tibble() %>% 
  count(track.uri, sort = TRUE) %>% 
  filter(n>1) %>% View()

# no duplicates for now so we're good to go!
