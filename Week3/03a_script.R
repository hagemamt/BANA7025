###############################################################################
# 03a_script.R
# Script to follow in-class examples
###############################################################################

##########################
# Intro to the Tidyverse #
##########################


# Installing/Loading the Tidyverse ----------------------------------------

# uncomment next line to install the Tidyverse package
# install.packages("tidyverse")
library(tidyverse)


# Installing/Loading non-core Tidyverse packages --------------------------

# uncomment next few lines to install various non-core Tidyverse packages
install.packages("readxl")
install.packages("lubridate")
install.packages("magrittr")
install.packages("glue")


# uncomment and load packages if needed
# library(readxl)
# library(lubridate)
# library(magrittr)
# library(glue)


# Prerequisites -----------------------------------------------------------

# load packages
library(nycflights13)
library(tidyverse)

vignette(package = "dplyr")

# your turn!
?flights


# The filter() function ---------------------------------------------------

# basic filtering
filter(flights, month == 1)
filter(flights, month == 1, day == 1)
filter(flights, month == 1, day == 1, dep_delay > 0)

# save a new data frame
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

# logical tests
12 == 12
12 <= c(12, 11)
12 %in% c(12, 11, 8)
x <- c(12, NA, 11, NA, 8)
is.na(x)

# multiple logical tests
12 == 12 & 12 < 14
12 == 12 & 12 < 10
12 == 12 | 12 < 10
any(12 == 12, 12 < 10)
all(12 == 12, 12 < 10)

# multiple comparisons
filter(flights, month == 12, day == 25)
filter(flights, month == 12 & day == 25)
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11, 12))
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)


# your turn!
#a
filter(flights, arr_delay > 120)
#b
filter(flights, dest %in% c('IAH', 'HOU'))
filter(flights, dest == 'IAH' | dest == 'HOU')
#c
filter(flights, arr_delay > 120, dep_delay <= 0)


# The arrange() function --------------------------------------------------

# ordering data
arrange(flights, dep_delay)
arrange(flights, dep_delay, arr_delay)
arrange(flights, desc(dep_delay))

# missing values always sorted at the end
df <- tibble(x = c(5, 2, 5, NA))
arrange(df, x)
arrange(df, desc(x))


# your turn!
#1
arrange(flights, desc(dep_delay))
#2
arrange(flights, dep_time)
#3
arrange(flights, desc(distance))
#4
arrange(flights, desc(distance))


# The select() function ---------------------------------------------------

# selecting variables
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
dim(flights)
# helper functions with the select function
select(flights, ends_with("time"))
select(flights, c(carrier, ends_with("time"), contains("delay")))
select(flights, time_hour, air_time, everything())

# renaming variables
rename(flights, ANNOYING = dep_delay)
names(flights$dep_delay) <- 'ANNOYING'
names(filghts)
# your turn!
#1
select(flights, year, year)
#2
vars <- c("MONTH", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
#3



# The mutate() function ---------------------------------------------------

# create a smaller data set to work with
flights_sml <- select(
  flights, 
  year:day, 
  ends_with("delay"), 
  distance, 
  air_time
)
flights_sml

# create new variables based on existing variables
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
flights_sml

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       gain_per_hour = gain / hours)

# using transmute() to only keep new variables
transmute(
  flights,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# using mathematical functions with mutate() and transmute()
transmute(
  flights,
  normalized_delay = dep_delay / (mean(dep_delay, na.rm = TRUE))
)
transmute(flights,
          log_air_time = log2(air_time),
          exp_delay = exp(dep_delay))
transmute(flights,
          dep_delay = dep_delay,
          lag_delay = lag(dep_delay),
          sum_delay = cumsum(dep_delay))
transmute(flights,
          arr_delay = arr_delay,
          bucket = ntile(arr_delay, 10))


# your turn!
#1
transmute(flights,
          distance_km = distance * 1.60934,
          time_per_km = air_time / distance_km)

#2



# The summarize() or summarise() function ---------------------------------

# summarizing data
summarise(flights, dep_delay_mean = mean(dep_delay, na.rm = TRUE))
summarise(flights, 
          dep_delay_mean = mean(dep_delay, na.rm = TRUE),
          dep_delay_sd = sd(dep_delay, na.rm = TRUE))
summarise(flights, 
          dep_delay_mean = mean(dep_delay, na.rm = TRUE),
          dep_delay_sd = sd(dep_delay, na.rm = TRUE),
          n = n())

# summarizing grouped data
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# your turn!
#1
by_carrier <- group_by(flights, carrier)
summarise(by_carrier, delay = mean(dep_delay, na.rm = TRUE))

#2
summarise(by_carrier, 
          max = max(dep_delay, na.rm = TRUE),
          min = min(dep_delay, na.rm = TRUE),
          delta = max - min)

#3
by_month <- group_by(flights, month)
summarise(by_month, delay = sd(arr_delay, na.rm = TRUE))



# The pipe operator %>% ---------------------------------------------------

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != 'HNL')

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

flights %>% 
  group_by(dest) %>%
  summarise(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL") %>%
  ggplot(aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

# library(magrittr)
# x <- 1:15
# sum(x)
# x %>% sum()



# your turn!


# another your turn!


