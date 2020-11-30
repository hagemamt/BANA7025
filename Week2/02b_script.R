###############################################################################
# 02b_script.R
# Script to follow in-class examples
###############################################################################

###############
# Data Import #
###############


# Importing .csv files ----------------------------------------------------

# Base R
read.csv("data/flights.csv")

# Load package readr
library(readr)
readr::read_csv("data/flights.csv")

# Load package data.table
library(data.table)
fread("data/flights.csv")



# Importing Excel files ---------------------------------------------------

# import packages
library(readxl)

# identify the sheet you want
excel_sheets("data/mydata.xlsx")

# now read in the data
read_excel("data/mydata.xlsx", sheet = "PICK_ME_FIRST!")



################################
# Group Challenge: Excel Files #
################################

#1

#2

#3

#4

#5

#6

#7

#8

#9

#10



################################
# Group Challenge: Online File #
################################

#1

#2

#3

#4

#5

