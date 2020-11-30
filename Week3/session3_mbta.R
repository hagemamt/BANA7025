###############################################################################
# session3_mbta.R
# Mark Hageman 
# BANA 7025-001
# Oct. 26, 2020
###############################################################################

library(readxl)

# Import the data ---------------------------------------------------------

#1. What spreadsheets exist in the workbook?
excel_sheets('week3/data/mbta.xlsx')

#2. Import mbta.xlsx
data <- read_xlsx('week3/data/mbta.xlsx', skip = 1)

# Examining the data ------------------------------------------------------

#1. View the structure of mbta.
str(data)

#2. View the first 6 rows of mbta.
head(data, 6)

#3. View a summary of mbta.
summary(data)

# Removing unnecessary rows and columns -----------------------------------

#1. Remove the first, seventh, and eleventh rows of mbta.
#2. Remove the first column.
data_orig <- data
data <- data[-c(1, 7, 11),-1]

# Observations are stored in columns --------------------------------------

#1. Load the tidyr package.
library(tidyr)

#2. Gather the columns of the mbta data. Call your new columns
# month and thou_riders (for “thousand riders”).
data <- data %>% gather(month, thou_riders, 2:59)
head(data)

# Type conversions --------------------------------------------------------

# Coerce the ridership column, mbta$thou_riders, into a numeric data type.
data$thou_riders <- as.numeric(data$thou_riders)

# Variables are stored in both rows and columns ---------------------------

#1. Use spread() to change values in the mode column of mbta into
# column names. The columns should contain the average weekday
# ridership values associated with that mode of transport.
data <- data %>% spread(mode, thou_riders)

#2. View the head of this new mbta data set.      
head(data)

# Separating columns ------------------------------------------------------

#1. Split the month column of mbta at the dash and create a new
# month column with only the month and a year column with only the year.
data <- data %>% separate(month, c('year', 'month'), sep = '-')

#2. View the head of this new mbta data set.
head(data)

# Do your values seem reasonable? -----------------------------------------

#1. View a summary() of the data.
summary(data)

#2. Generate a histogram of the Commuter Boat ridership.
hist(data$Boat)

#3. Use a boxplot to identify the month and year of this outlier.
boxplot(data$Boat, xlab = 'Boat')

# Dealing with entry error ------------------------------------------------

#1. Locate the row and column of the incorrect value.
which(data$Boat == 40)

#2. Replace the incorrect value with 4.
data[3,3] <- 4

#3. Now view the summary, histogram, and boxplots of the Boat variable.
summary(data$Boat)
hist(data$Boat)
boxplot(data$Boat, xlab = "Boat")
