###############################################################################
# coding_exercises_1_20201019.R
# Mark Hageman & Johnny Arguedas
# BANA 7025-001
# Oct. 19, 2020
###############################################################################

library(spotifyr)
library(tidytuesdayR)
library(tidyverse)

# Get the Data
# codebook: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md

spotify_songs <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv',
                  stringsAsFactors = F)
tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)
spotify_songs <- tuesdata$spotify_songs

# get to know the data
dim(spotify_songs)
str(spotify_songs)

# Any missing data?
colSums(is.na(spotify_songs)) 
# 5 missing values for track_artist 
# 5 missing values for track_album_name

summary(spotify_songs)

hist(spotify_songs$duration_ms)
plot(spotify_songs$energy, spotify_songs$tempo)

# What are the genres?
unique(spotify_songs$playlist_genre)
ggplot(data = spotify_songs) + 
  geom_bar(mapping = aes(x = playlist_genre))
spotify_songs %>% count(playlist_genre)

# display mean of speechiness by genre
ggplot(data = spotify_songs, mapping = aes(x = playlist_genre, 
                                           y = speechiness)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")

# distribution of duration in milliseconds
ggplot(data = spotify_songs) +
  geom_histogram(aes(x = duration_ms))
# relatively normal with slight positive skew

# distribution of tempo in beats per minute (bpm)
ggplot(data = spotify_songs) +
  geom_histogram(aes(x = tempo))

# distribution of tempo by playlist_genre
ggplot(data = spotify_songs, aes(x = tempo, color = playlist_genre)) +
  geom_freqpoly(binwidth = 10)
# try a smaller bin width
ggplot(data = spotify_songs, aes(x = tempo, color = playlist_genre)) +
  geom_freqpoly(binwidth = 5)

# distribution of speechiness
ggplot(data = spotify_songs) +
  geom_histogram(aes(x = speechiness))

# examine covariation of playlist_genre and speechiness
ggplot(data = spotify_songs, mapping = aes(x = playlist_genre, 
                                           y = speechiness)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")
ggplot(data = spotify_songs, aes(x = playlist_genre, 
                                 y = speechiness)) +
  geom_boxplot(aes(x = reorder(playlist_genre, 
                               speechiness, 
                               FUN = median),
                   y = speechiness)) 

# examine mean of track_popularity by playlist_genre
ggplot(spotify_songs, aes(x = playlist_genre, 
                          y = track_popularity)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar")



