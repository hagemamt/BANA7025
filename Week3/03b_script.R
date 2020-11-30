###############################################################################
# 03a_script.R
# Script to follow in-class examples
###############################################################################

library(tidyverse)

################
# Tidying Data #
################


# The gather() function ---------------------------------------------------

## Note: The cases data set is NOT in RStudio.
## The code below is to demonstration purposes only.

# gather() function
cases %>% 
  gather(Year, n, 2:4)

# code alternatives to the aforementioned command
cases %>% gather(Year, n, `2011`:`2013`)
cases %>% gather(Year, n, `2011`, `2012`, `2013`)
cases %>% gather(Year, n, 2:4)
cases %>% gather(Year, n, -Country)


# your turn!
#1
data <- read_rds('week3/data/bomber_wide.rds')
#2
data %>%
  gather(Year, Value, -c(Type, MD))



# The spread() function ---------------------------------------------------

## Note: The cases data set is NOT in RStudio.
## The code below is to demonstration purposes only.

# spread() function
cases %>% 
  spread(Year, n)

# your turn!
#1
read_rds('week3/data/bomber_long.rds') %>%
  spread(Output, Value)

#2



# The separate() function -------------------------------------------------

## Note: The storms data set is NOT in RStudio.
## The code below is to demonstration purposes only.

# separate() function
storms %>% 
  separate(col = date,
           into = c("year", "month", "day"),
           sep = "-")

# code alternatives
storms %>% separate(date, c("year", "month", "day"))
storms %>% separate(date, c("year", "month", "day"), sep = "-")


# your turn!
#1
read_rds('week3/data/bomber_combined.rds') %>% 
  separate(AC, c("Type", "MD"), sep = ' ')
  
#2


# The unite() function ----------------------------------------------------

## Note: The storms data set is NOT in RStudio.
## The code below is to demonstration purposes only.

# unite() function
storms %>% 
  unite(col = date,
        year, month, day,
        sep = "-")

# code alternatives
storms %>% unite(date, year, month, day, sep = "_")
storms %>% unite(date, year, month, day)


# your turn!
#1
read_rds('week3/data/bomber_prefix.rds') %>% 
  unite(MD, prefix, number, sep = '-')
#2



# Challenge! --------------------------------------------------------------

#1
read_rds('week3/data/bomber_mess.rds') %>% 
  unite(MD, prefix, number, sep = '-') %>% 
  separate(Metric, c('FY', 'other'), sep = "_") %>% 
  spread(other, Value) %>% 
  as_tibble()
  
#2



##################
# MBTA Ridership #
##################

