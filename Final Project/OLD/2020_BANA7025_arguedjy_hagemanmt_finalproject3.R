library(tidyverse)
library(scales)
library(table1)
library(htmltools)
library(cowplot)
library(rlang)
library(Hmisc)
library(gridExtra)

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


songs %>% ggplot(aes(x = playlist_genre, fill = playlist_genre)) + 
  geom_bar() +
  ggtitle("Number of Songs Within Each Genre") +
  scale_fill_discrete(name = "Genre") +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(expand = c(0, 0), name = "Count of \n Songs") +
  theme(axis.title.y = element_text(angle = 0, vjust = .5)) + 
  theme(panel.grid.major.x = element_blank()) + 
  coord_cartesian(ylim = c(0, 7000))


# As we can see, the makeup of songs in our dataset is pretty even, 
# with EDM at the highest and rock at the lowest. 

# How do the genres rank in terms of popularity?
  
popularity_means <- songs %>% 
  group_by(playlist_genre) %>%
  dplyr::summarize(mean_popularity = mean(track_popularity))

popularity_means %>% ggplot(aes(x = playlist_genre, y = mean_popularity, fill = playlist_genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Popularity of Each Genre") +
  scale_fill_discrete(name = "Genre") +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(expand = c(0,0), name = "Average \n Popularity") +
  theme(axis.title.y = element_text(angle = 0, vjust = .5)) + 
  theme(panel.grid.major.x = element_blank()) + 
  coord_cartesian(ylim = c(0, 55))

# Which artists with at least five tracks in the data set are more or less popular?
songs %>% 
  group_by(track_artist) %>%
  filter(n() >= 5) %>% 
  dplyr::summarize(mean_popularity = mean(track_popularity)) %>% 
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
  dplyr::summarize(n_unique = n_distinct(playlist_genre)) %>% 
  filter(n_unique >= 2) %>% 
  count()

# Which artists have tracks in the greatest variety of genres? 
songs %>% 
  group_by(track_artist) %>% 
  dplyr::summarize(n_unique = n_distinct(playlist_genre)) %>% 
  filter(n_unique >= 2) %>% 
  arrange(desc(n_unique)) %>% 
  slice_head(n = 10)
 
# What dates are covered in the data set?
min(songs$track_album_release_date)
max(songs$track_album_release_date)

myplots <- 
  map(names(songs %>% select(where(is.numeric)) %>% select(-mode)), 
      function(colName) {
        songs %>% 
          ggplot(aes(x = playlist_genre,
                     y = !! sym(colName),
                     fill = playlist_genre)) +
          geom_boxplot() +
          theme(legend.position = "NONE") +
          labs(title = capitalize(colName), x = "", y = "")
    })
gridExtra::grid.arrange(grobs = myplots[c(1:4)])
gridExtra::grid.arrange(grobs = myplots[c(5:8)])
gridExtra::grid.arrange(grobs = myplots[c(9:12)])



  