library(ggplot2movies)
library(tidyverse)

movies
summary(movies$length)

movies %>% 
  ggplot(aes(x = length)) +
  geom_bar() + coord_cartesian(xlim = c(0,200))

movies %>% 
  ggplot(aes(x = length)) +
  geom_histogram()

which.max(movies$length)
movies[11937, 'title']

movies %>% filter(length > 200) %>% arrange(desc(length)) %>% 
  select(title)

movies %>% filter(length < 50) %>% nrow

movies %>% filter(length <= 49) %>% 
  summarize(average_length = mean(length))

movies %>% 
  mutate(length_type = 
           ifelse(length <= 49, 'short', 
                  (ifelse(length > 150, 'long', 'regular'))
         )) -> movies
View(movies)

movies %>% filter(length_type == 'regular') %>% 
  summarize(average_length = mean(length))

movies %>% 
  ggplot(aes(x = length_type)) + 
  geom_histogram(stat = 'count')

movies %>% 
  group_by(length_type) %>% 
  summarize(average_rating = mean(rating))

movies %>% 
  ggplot(aes(x = length_type, y = rating)) +
  stat_summary(fun = 'mean', geom = 'bar')

movies %>% 
  group_by(length_type) %>% 
  summarize(average_rating = mean(rating),
            average_votes = mean(votes)) %>% 
  ggplot(aes(x = length_type, 
             y = average_rating, 
             alpha = average_votes)) +
  geom_col()

movies_long <- 
  movies %>% 
  filter(length > 150) %>% 
  arrange(desc(length))
View(movies_long)
