###############################################################################
# Mark_Hageman_Homework_1.R
# Mark Hageman
# BANA 7025-001
# Oct. 18, 2020
###############################################################################


# Acquainting yourself with the data --------------------------------------


# 1. ----------------------------------------------------------------------
# import data and look at first 10 rows
data <- read.csv('week 1/material/week1_cincy_crimes.csv', 
                 stringsAsFactors = FALSE)
head(data, 10)


# 2. ----------------------------------------------------------------------
# examine structure of data
dim(data)
str(data)
names(data)


# 3. ----------------------------------------------------------------------
# modify column names to use "snake case"
names(data) <- c("instance_id", "closed", "opening", "day_of_week", 
                 "victim_gender", "total_number_of_victims", "total_suspects")
names(data)


# 4. ----------------------------------------------------------------------

# change victim_gender and day_of_week to factors
data$victim_gender <- as.factor(data$victim_gender)
data$day_of_week <- as.factor(data$day_of_week)
data$day_of_week <- factor(data$day_of_week, 
                           levels = c("SUNDAY", "MONDAY", "TUESDAY", 
                                      "WEDNESDAY", "THURSDAY", "FRIDAY",
                                      "SATURDAY"))
str(data)


# 5. ----------------------------------------------------------------------

# find missing values per column
colSums(is.na(data))

# how many complete cases are there?
sum(complete.cases(data))


# Data Cleaning -----------------------------------------------------------


# 6. ----------------------------------------------------------------------

# what are the unique values in each column?
unique(data$closed)
unique(data$opening)
unique(data$victim_gender)
unique(data$day_of_week)

# 7. ----------------------------------------------------------------------



# 8. ----------------------------------------------------------------------



# EDA (Exploratory Data Analysis) -----------------------------------------



# 9. ----------------------------------------------------------------------



# 10. ---------------------------------------------------------------------



