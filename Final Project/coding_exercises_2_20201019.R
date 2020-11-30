###############################################################################
# coding_exercises_1_20201019.R
# Mark Hageman & Johnny Arguedas
# BANA 7025-001
# Oct. 19, 2020
###############################################################################

library(tidyverse)
library(table1)
library(htmltools)

# Get the Data
# codebook: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md

songs <- read.csv('spotify_songs.csv', stringsAsFactors = F)

# get to know the data
dim(songs)
nrow(songs)
format(nrow(songs), nsmall=1, big.mark=",")  
ncol(songs)
names(songs)

str(songs)
songs$track_album_release_date <- as.Date(songs$track_album_release_date)
songs$playlist_genre <- as.factor(songs$playlist_genre)
songs$playlist_subgenre <- as.factor(songs$playlist_subgenre)
str(songs)

# Any missing data?
nrow(songs[complete.cases(songs), ])
colSums(is.na(songs))

# drop track_album_release_date
songs <- songs[ , !(names(songs) %in% c("track_album_release_date"))]
names(songs)
nrow(songs)

(na_rows <- nrow(songs) - nrow(songs[complete.cases(songs), ]))
(pct_na_rows <- na_rows / nrow(songs))
colSums(is.na(songs))

# omit rows with missing data
songs <- na.omit(songs)

# get summaries of numeric columns
songs %>%
  summarise_if(is.numeric, mean)
summary(songs)

# longest song
round((max(songs$duration_ms) / 1000 / 60), 2)

# drop unused columns
unused_cols <- c('track_id', 'track_album_id', 'track_album_name', 
                 'playlist_name', 'playlist_id', 'playlist_subgenre')
songs <- songs[ , -which(names(songs) %in% c(unused_cols))]
names(songs)

# What does the clean data look like?
dim(songs)
head(songs)

mean(songs$track_popularity)
format(n_distinct(songs$track_artist), nsmall=1, big.mark=",")

num_cols <- c("danceability", "energy", "key", "loudness", "mode", 
              "speechiness", "acousticness", "instrumentalness", 
              "liveness", "valence")
summary(songs[num_cols])

table1::table1(~ danceability + energy + key + loudness + mode + 
               speechiness + acousticness + instrumentalness +  
               liveness + valence | playlist_genre, data = songs)

hist(songs$duration_ms)
plot(songs$energy, songs$tempo)

# What are the genres?
unique(songs$playlist_genre)
ggplot(data = songs) + 
  geom_bar(mapping = aes(x = playlist_genre))
songs %>% count(playlist_genre)

# display mean of speechiness by genre
ggplot(data = songs, mapping = aes(x = playlist_genre, 
                                           y = speechiness)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")

# distribution of duration in milliseconds
ggplot(data = songs) +
  geom_histogram(aes(x = duration_ms))
# relatively normal with slight positive skew

# distribution of tempo in beats per minute (bpm)
ggplot(data = songs) +
  geom_histogram(aes(x = tempo))

# distribution of tempo by playlist_genre
ggplot(data = songs, aes(x = tempo, color = playlist_genre)) +
  geom_freqpoly(binwidth = 10)
# try a smaller bin width
ggplot(data = spotify_songs, aes(x = tempo, color = playlist_genre)) +
  geom_freqpoly(binwidth = 5)

# distribution of speechiness
ggplot(data = songs) +
  geom_histogram(aes(x = speechiness))

# examine covariation of playlist_genre and speechiness
ggplot(data = songs, mapping = aes(x = playlist_genre, 
                                           y = speechiness)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")
ggplot(data = songs, aes(x = playlist_genre, 
                                 y = speechiness)) +
  geom_boxplot(aes(x = reorder(playlist_genre, 
                               speechiness, 
                               FUN = median),
                   y = speechiness)) 

# examine mean of track_popularity by playlist_genre
ggplot(songs, aes(x = playlist_genre, 
                          y = track_popularity)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")



