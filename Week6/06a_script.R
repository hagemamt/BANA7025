###############################################################################
# 06a_script.R
# Script for control statements and iteration
###############################################################################


# Prerequisites -----------------------------------------------------------

# load packages
library(tidyverse)
library(purrr)


# Syntax of an if statement -----------------------------------------------

# What do you expect to happen?
x <- c(8, 3, -2, 5)
if(x < 0) {
  print("x contains a negative number")
}

# Now what do you expect to happen?
x <- c(8, 3, -2, 5)
if(any(x < 0)) {
  print("x contains a negative number")
}
if(sum(x < 0) > 0) {
  print("x contains a negative number")
}

# Extend with an else statement
if(any(x < 0)) {
  print("x contains a negative number")
} else {
  print("x contains all positive numbers")
}

# Expand to include multiple test expressions
x <- 7
if(x >= 10) {
  print("x exceeds acceptable tolerance levels")
} else if(x >= 0 & x < 10) {
  print("x is within acceptable tolerance levels")
} else {
  print("x is negative")
}



# Your turn: Part one -----------------------------------------------------

file_1 <- "Month-"
file_2 <- ".csv"
month <- 5

if (month %in% 1:9) {
  paste0("Week6/data/", file_1, 0, month, file_2)
} else if (month %in% 10:12) {
  paste0("Week6/data/", file_1, month, file_2)
} else {
  print("Invalid month")
}


#bonus, any other way without ifelse? (hint: a function we learned!)
pad_month <- stringr::str_pad(month, width = 2, pad = "0", side = "left")
paste0("Week6/data/", file_1, pad_month, file_2)

# Your turn: Part two -----------------------------------------------------
if (month %in% 1:9) {
  path <- paste0("Week6/data/", file_1, 0, month, file_2)
  file.exists(path)
} else if (month %in% 10:12) {
  path <- paste0("Week6/data/", file_1, month, file_2)
  file.exists(path)
} else {
  print("Invalid month")
}


# Iteration: For Loops ----------------------------------------------------

# What does this loop do?
years <- 2010:2017
for (i in seq_along(years)) {
  output <- paste("The year is", years[i])
  print(output)
}

# To save results from loop
result <- vector(mode = "character", 
                 length = length(years))
for (i in seq_along(years)) {
  output <- paste("The year is", years[i])
  result[i] <- output
}
result

# Extending if-else logic into a for loop
x <- c(-1, 7, 8, 11)
tolerance <- vector(mode = "character", 
                    length = length(x))
for (i in seq_along(x)) {
  if (x[i] >= 10) {
    value <- "x exceeds acceptable tolerance levels"
  } else if (x[i] >= 0 & x[i] < 10) {
    value <- "x is within acceptable tolerance levels"
  } else {
    value <- "x is negative"
  }
  tolerance[i] <- value
}
tolerance


# Your turn ---------------------------------------------------------------
file_1 <- "Month-"
file_2 <- ".csv"
month <- 1:13

# create empty data frame
df.all.months <- dta.frame(NULL)

for (i in month) {
  if (i %in% 1:9) {
    path <- paste0("Week6/data/", file_1, 0, i, file_2)
  } else if (i %in% 10:12) {
    path <- paste0("Week6/data/", file_1, i, file_2)
  } else {
    print(paste(i, "is an invalid month"))
  }
  
  # import data
  if(file.exists(path)) {
    df <- read_csv(path)
    df.all.months <- rbind(df.all.months, df)
    rm(df)
  } else {
    print(paste("There is not data availble for month", i))
  }
}


# Iteration: Map Functions ------------------------------------------------

# Basics of the map functions
map_dbl(mtcars, mean)
mtcars %>% map_dbl(mean)
mtcars %>% map(mean)


# Your turn ---------------------------------------------------------------
map_chr(mtcars, typeof)
map_dbl(mtcars, mean)
map_dbl(mtcars, mean) > 5
mtcars %>% map_dbl(mean) %>% map_lgl(~ . > 5)
mtcars %>% map_lgl(~ mean(.) > 5) # anonymous function

# Shortcuts for specifying .f ---------------------------------------------

# Which variables have missing data and how many missing values they have
# (method 1)
nycflights13::flights %>%
  map_dbl(~ sum(is.na(.)))

# Which variables have missing data and how many missing values they have
# (method 2)
nycflights13::flights %>%
  map_dbl(~ sum(is.na(.))) %>% 
  # convert a vector into a two-column data frame
  tibble::enframe(name = "variable", value = "num_missing_values") %>% 
  # apply some dplyr functions to this data frame
  filter(num_missing_values > 0) %>% 
  arrange(desc(num_missing_values))



# Map functions for quick model comparisons -------------------------------

# Want to apply a model over a list of data frames
cyl <- split(mtcars, mtcars$cyl)
str(cyl)
cyl[[1]]

# Obtain r-squared for each cylinder
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# Obtain slope of each linear model
mtcars %>% 
  split(.$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map("coefficients") %>%
  map_dbl(2)



# Your turn ---------------------------------------------------------------
ggplot2::diamonds %>% 
  split(.$cut) %>% 
  map(~ lm(price ~ carat, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")

ggplot2::diamonds %>% 
  split(.$cut) %>% 
  map(~ lm(price ~ carat, data = .)) %>% 
  map(summary) %>% 
  map("coefficients") %>%
  map_dbl(2)
