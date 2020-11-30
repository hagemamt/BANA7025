###############################################################################
# 04b_script.R
# Script to follow in-class examples
###############################################################################



# Prerequisites -----------------------------------------------------------

# load packages
library(nycflights13)
library(tidyverse)

# example data
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)



# Mutating joins ----------------------------------------------------------

# examples of mutating joins
x %>% inner_join(y, by = "key")
x %>% left_join(y, by = "key")
x %>% right_join(y, by = "key")
x %>% full_join(y, by = "key")

# example data for mismatching keys
x <- tribble(
  ~key1, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key2, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# how to join if keys don't match
x %>% inner_join(y, by = c("key1" = "key2"))


# your turn!
#1
flights %>% 
  left_join(airlines, by = "carrier") %>%
  filter(carrier == "Virgin America") %>%
  group_by(time_hour) %>%
  summarize(delay = mean(dep_delay, na.rm = T)) %>%
  top_n(10, wt = delay) %>%
  arrange(desc(delay))

#2
flights %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(dest,
         origin,
         origin_lat = lat.x,
         origin_lon = lon.x,
         dest_lat = lat.y,
         dest_lon = lon.y,
         arr_delay)



# Filtering joins ---------------------------------------------------------

# examples of filtering joins
x %>% semi_join(y, by = "key")
x %>% anti_join(y, by = "key")


# your turn!
#1
flights %>% 
  semi_join(planes, by = "tailnum") %>% 
  tally()
flights %>% 
  anti_join(planes, by="tailnum") %>% 
  tally()
#2
airports %>% 
  anti_join(flights, by=c("faa" = "dest")) %>% 
  distinct(faa) %>% 
  tally()


# Set operations ----------------------------------------------------------

# example data
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

# examples of set operations
intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)



# Challenge! --------------------------------------------------------------

