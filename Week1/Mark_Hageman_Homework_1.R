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

# change opening, victim_gender, and day_of_week to factors
data$opening <- as.factor(data$opening)
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
length(unique(data$instance_id))
unique(data$opening)
unique(data$victim_gender)
unique(data$day_of_week)
unique(data$total_number_of_victims)
unique(data$total_suspects)

# for the data$opening field, modify "??" values to "UNKNOWN"
nrow(subset(data, opening == "??"))
data$opening[data$opening == "??"] <- "6 - UNKNOWN"
nrow(subset(data, opening == "??"))

# drop the unused level
data$opening <- as.character(data$opening)
unique(data$opening)
data$opening <- as.factor(data$opening)
levels(data$opening)

# set NA values to "6 - UNKNOWN"
sum(is.na(data$opening))
data$opening[is.na(data$opening)] <- "6 - UNKNOWN" 
sum(is.na(data$opening))
levels(data$opening)

# modify the levels of data$opening
levels(data$opening) <- c("DOOR", "WINDOW", "GARAGE", "SKYLIGHT", 
                          "OTHER", "UNKNOWN")
levels(data$opening)

# list the levels of data$victim_gender
levels(data$victim_gender)

# convert the field to character
data$victim_gender <- as.character(data$victim_gender)

# change "F - FEMALE" to just "FEMALE"
nrow(subset(data, victim_gender == "F - FEMALE"))
data$victim_gender[which(data$victim_gender == "F - FEMALE")] <- "FEMALE"
nrow(subset(data, victim_gender == "F - FEMALE"))

# change "M - MALE" to just "MALE"
nrow(subset(data, victim_gender == "M - MALE"))
data$victim_gender[which(data$victim_gender == "M - MALE",)] <- "MALE"
nrow(subset(data, victim_gender == "M - MALE"))

# change "NON-PERSON (BUSINESS" to just "NON-PERSON"
nrow(subset(data, victim_gender == "NON-PERSON (BUSINESS"))
data$victim_gender[which(data$victim_gender == "NON-PERSON (BUSINESS")] <-
  "NON-PERSON"
nrow(subset(data, victim_gender == "NON-PERSON (BUSINESS"))

# convert the victim_gender field back into a factor and list the levels
data$victim_gender <- as.factor(data$victim_gender)
levels(data$victim_gender)


# 7. ----------------------------------------------------------------------



# 8. ----------------------------------------------------------------------



# EDA (Exploratory Data Analysis) -----------------------------------------



# 9. ----------------------------------------------------------------------



# 10. ---------------------------------------------------------------------



