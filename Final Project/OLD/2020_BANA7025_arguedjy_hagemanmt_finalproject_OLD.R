library(tidyverse)
library(scales)
library(table1)
library(htmltools)

songs <- read.csv('spotify_songs.csv', stringsAsFactors = FALSE)

str(songs)
songs$track_album_release_date <- as.Date(songs$track_album_release_date)
songs$playlist_genre <- as.factor(songs$playlist_genre)
songs$playlist_subgenre <- as.factor(songs$playlist_subgenre)

nrow(songs[complete.cases(songs), ])
colSums(is.na(songs))

na_rows <- nrow(songs) - nrow(songs[complete.cases(songs), ])
pct_na_rows <- na_rows / nrow(songs)

songs <- na.omit(songs)

summary(songs)
songs %>%
  summarise_if(is.numeric, mean)

unused_cols <- c('track_id', 'track_album_id', 'track_album_name', 
                 'playlist_name', 'playlist_id', 'playlist_subgenre')
songs <- songs[ , -which(names(songs) %in% c(unused_cols))]

dim(songs)
head(songs)

# What is the makeup of songs in our data set, in terms of genre?
# What are the characteristics of each genre?
table1::table1(~ track_popularity + danceability + energy + key + loudness + 
                 mode + speechiness + acousticness + instrumentalness +
                 liveness + valence | playlist_genre, data = songs)

# Which artists with at least five tracks in the data set are more or less popular?
songs %>% 
  group_by(track_artist) %>%
  filter(n() >= 5) %>% 
  summarize(mean_popularity = mean(track_popularity)) %>% 
  arrange(desc(mean_popularity)) %>% 
  slice_head(n = 10)

songs %>% 
  group_by(track_artist) %>%
  filter(n() >= 5) %>%
  summarize(mean_popularity = mean(track_popularity)) %>% 
  arrange(mean_popularity)  %>% 
  slice_head(n = 10)

# How many cross-genre artists are there?
songs %>% 
  group_by(track_artist) %>% 
  summarize(n_unique = n_distinct(playlist_genre)) %>% 
  filter(n_unique >= 2) %>% 
  count()

# Which artists have tracks in the greatest variety of genres? 
songs %>% 
  group_by(track_artist) %>% 
  summarize(n_unique = n_distinct(playlist_genre)) %>% 
  filter(n_unique >= 2) %>% 
  arrange(desc(n_unique)) %>% 
  slice_head(n = 10)
 
# What dates are covered in the data set?
min(songs$track_album_release_date)
max(songs$track_album_release_date)


  