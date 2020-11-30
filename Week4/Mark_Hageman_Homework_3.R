###############################################################################
# Mark_Hageman_Homework_3.R
# Mark Hageman
# BANA 7025-001
# Nov. 8, 2020
###############################################################################

library(tidyverse)
library(car)
library(pillar)

# Importing Data and Data Cleaning

# 1.
data <- read_csv('week4/homework3/acs_2015_county_data_revised.csv',
                 col_names = T)
dim(data)

# 2. 
str(data)
glimpse(data)

# 3.
colSums(is.na(data))
# locate record with na income
data[is.na(data$income), c('state', 'county', 'income')] 
# impute median for income
(median_income <- median(data$income, na.rm = T))
data <- data %>% 
  mutate(income = replace_na(income, median_income))

# locate record with na child_poverty
data[is.na(data$child_poverty), c('state', 'county', 'child_poverty')]
# impute mean for child_poverty
(mean_child_poverty <- mean(data$child_poverty, na.rm = T))
data <- data %>% 
  mutate(child_poverty = replace_na(child_poverty, mean_child_poverty))
colSums(is.na(data))

# 4.
summary(data)
data[which.max(data$hispanic),]
Boxplot(data$transit)
data[which.max(data$transit), c('census_id', 'state', 'county', 'transit')]
data[which.max(data$child_poverty), 
     c('census_id', 'state', 'county', 'child_poverty')]
data[which.max(data$child_poverty), 
     c('census_id', 'state', 'county', 'poverty')]

# Data Manipulation and Insights

# 5.
nrow(data[data$men < data$women, ])

# 6.
nrow(data[data$unemployment < 0.1,])
nrow(data[data$unemployment < 10,])

# 7. 
data %>% 
  arrange(desc(mean_commute)) %>% 
  slice_head(n = 10) %>%
  select(census_id, county, state, mean_commute)

data %>% 
  top_n(10, 29) %>%
  select(census_id, county, state, mean_commute)

# 8.
data %>% 
  mutate(pct_women = women / total_pop) %>% 
  arrange(pct_women) %>% 
  select(census_id, county, state, pct_women)

# 9.
# 9.a
data <- data %>% 
  mutate(
    sum_race_pct = 
      round((hispanic + white + black + native + asian + pacific), 2)
  ) 
data %>% 
  arrange(sum_race_pct) %>% 
  slice_head(n = 10) %>% 
  select(county, state, sum_race_pct)

# 9.b
by_state <- group_by(data, state)
summarize(by_state, avg_race_pct = mean(sum_race_pct)) %>% 
  arrange(avg_race_pct) %>% 
  slice_head(n = 1)

# 9.c
nrow(subset(data, data$sum_race_pct > 100))
data %>% 
  filter(sum_race_pct > 100) %>% 
  select(state, county, sum_race_pct) %>% 
  arrange(county)

# 9.d
nrow(subset(data, data$sum_race_pct == 100.0))
data %>% 
  filter(sum_race_pct == 100.0) %>% 
  select(state, county, sum_race_pct) %>% 
  arrange(county)

# 10.

# 10.a 
data <- data %>% 
  mutate(carpool_rank = min_rank(desc(data$carpool))) 

# 10.b
data %>% 
  arrange(carpool_rank) %>% 
  slice_head(n = 10) %>% 
  select(census_id, county, state, carpool, carpool_rank)

# 10.c
data %>% 
  arrange(desc(carpool_rank)) %>% 
  slice_head(n = 10) %>% 
  select(census_id, county, state, carpool, carpool_rank)

# 10.d
data %>% 
  group_by(state) %>% 
  summarize(avg_carpool = mean(carpool)) %>% 
  arrange(desc(avg_carpool)) %>% 
  slice_head(n = 1)

# 10.e
data %>% 
  group_by(state) %>% 
  summarize(avg_carpool = mean(carpool)) %>% 
  arrange(desc(avg_carpool)) %>% 
  slice_head(n = 5)

