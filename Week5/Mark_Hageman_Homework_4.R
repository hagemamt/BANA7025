library(Lahman)
library(tidyverse)

data(Teams)

Teams %>% 
  filter(yearID >= 1960, teamID == 'CIN') %>% 
  group_by(yearID) %>% 
  ggplot(aes(x = yearID)) +
  geom_line(aes(y = W, color = 'blue')) +
  geom_line(aes(y = L, color = 'red')) +
  scale_y_continuous(name = 'Wins') +
  ggtitle('Cincinnati Reds',
          subtitle = 'Wins and losses by season (since 1960)') +
  scale_color_identity(breaks = c('blue', 'red'),
                       labels = c('Wins', 'Losses'),
                       guide = 'legend') +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())

