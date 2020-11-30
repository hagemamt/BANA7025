library(tidyverse)
library(scales)
library(table1)
library(htmltools)
library(cowplot)
library(rlang)
library(Hmisc)
library(gridExtra)
library(ggcorrplot)

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
  dplyr::summarize(mean_popularity = mean(track_popularity)) %>% 
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

corr_songs <-cor(songs %>% select(where(is.numeric)) %>% select(-mode))
ggcorrplot(corr_songs, method = "circle", type ="lower")

# Multiple linear regression

# forward selection
add1(lm(track_popularity ~ 1, data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# instrumentalness has the highest significant F value, so add it first
add1(lm(track_popularity ~ instrumentalness, data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add energy
add1(lm(track_popularity ~ instrumentalness + energy, data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add loudness 
add1(lm(track_popularity ~ instrumentalness + energy + loudness, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add valence
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add liveness
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence
        + liveness, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add acousticness
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence
        + liveness + acousticness, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add danceability
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence
        + liveness + acousticness + danceability, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add tempo
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence
        + liveness + acousticness + danceability + tempo, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# add speechiness
add1(lm(track_popularity ~ instrumentalness + energy + loudness + valence
        + liveness + acousticness + danceability + tempo + speechiness, 
        data = songs),
     track_popularity ~ danceability + energy + key + loudness 
     + speechiness + acousticness + instrumentalness + liveness 
     + valence + tempo,
     test = "F")

# key is not found to be significant

# build the first model 
model1 <- lm(track_popularity ~ instrumentalness + energy + loudness + valence
             + liveness + acousticness + danceability + tempo + speechiness, 
             data = songs)
summary(model1)

# model adequacy checking
par(mfrow = c(1,2))
qqnorm(model1$residuals, main = "Q-Q Plot of Model 1 Residuals")
qqline(model1$residuals)
hist(model1$residuals, main = "Histogram of Model 1 Residuals")
par(mfrow = c(1,1))

# qq plot reveals non-normality, histogram shows light tails, 
# so try some transformations of track_popularity

# square root transformation
songs$sqrt_track_popularity <- sqrt(songs$track_popularity)
model2 <- lm(sqrt_track_popularity ~ instrumentalness + energy + loudness + 
               valence + liveness + acousticness + danceability + tempo + 
               speechiness, 
             data = songs)

# log transformation
songs$log_track_popularity <- log(songs$track_popularity)
model3 <- lm(log_track_popularity ~ instrumentalness + energy + loudness + 
               valence + liveness + acousticness + danceability + tempo + 
               speechiness, 
             data = songs[songs$log_track_popularity > -Inf,])

# reciprocal square root transformation
songs$recip_sqrt_track_popularity <- songs$track_popularity ^ (-.5)
model4 <- lm(recip_sqrt_track_popularity ~ instrumentalness + energy + loudness + 
               valence + liveness + acousticness + danceability + tempo + 
               speechiness, 
             data = songs[songs$log_track_popularity > -Inf,])

# reciprocal transformation
songs$recip_track_popularity <- songs$track_popularity ^ (-1)
model5 <- lm(recip_track_popularity ~ instrumentalness + energy + loudness + 
               valence + liveness + acousticness + danceability + tempo + 
               speechiness, 
             data = songs[songs$recip_sqrt_track_popularity < Inf,])

# arcsin transformation
songs$asin_track_popularity <- asin(songs$sqrt_track_popularity)
model6 <- lm(asin_track_popularity ~ instrumentalness + energy + loudness + 
               valence + liveness + acousticness + danceability + tempo + 
               speechiness, 
             data = songs[!(is.nan(songs$asin_track_popularity)),])

par(mfrow = c(2, 3))

qqnorm(model2$residuals, main = "Square root")
qqline(model2$residuals)

qqnorm(model3$residuals, main = "Log")
qqline(model3$residuals)

qqnorm(model4$residuals, main = "Reciprocal square root")
qqline(model4$residuals)

qqnorm(model5$residuals, main = "Reciprocal")
qqline(model5$residuals)

qqnorm(model6$residuals, main = "Arcsin")
qqline(model6$residuals)

par(mfrow = c(1,1))

df <- data.frame(Model = c(1:6),
                 Transformation = c("None",
                                    "Square root",
                                    "Log",
                                    "Reciprocal square root",
                                    "Reciprocal",
                                    "Arcsin"), 
                 AdjRsqr = c(summary(model1)$adj.r.squared,
                             summary(model2)$adj.r.squared,
                             summary(model3)$adj.r.squared,
                             summary(model4)$adj.r.squared,
                             summary(model5)$adj.r.squared,
                             summary(model6)$adj.r.squared))
df

