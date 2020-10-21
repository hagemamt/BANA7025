###############################################################################
# 01a_script.R
# Script to follow in-class examples
# (yay less typing and more concentrating!)
###############################################################################


################
# Fundamentals #
################



# Understanding the RStudio IDE -------------------------------------------

# things to type into the RStudio console
1 + 1
4 + 3^2 - 2*8
test_variable <- 14
test_variable %% 3


# your turn!
#1
2:173
#2 and #3
table(iris$Species)

# Storing Objects ---------------------------------------------------------

# assignment operator
x <- 5
x
y = 6
y

# more on storing objects
p <- 3
p
p <- 19
p
ls()
rm(p)
rm(list = ls()) # be VERY careful with this command!


# your turn!
#1
integer_vector <- 1:30
#2
binary_flag <- c(0,1)
#3
integer_vector * binary_flag


# Functions ---------------------------------------------------------------

# using functions in R
round(2.71828)
sd(1:10)
round(sd(1:10))
round(2.71828, digits = 2)

# more on function arguments
sd(1:10, digits = 3)
args(sd)
sd(1:10, TRUE)

# getting help
help(sqrt)
?sqrt
example(sqrt)
??sqr


# your turn!
#1
# removes empty values
#2
quantile(1:297)
example(quantile)
quantile(x <- 1:297, c(.25, .75))


# Scripts -----------------------------------------------------------------

# set your working directory
getwd()
setwd(_____________) # insert your working directory here
getwd()


# your turn!



# Installing Packages -----------------------------------------------------

# install packages from CRAN
install.packages("packagename") # insert package name here

# install packages from Bioconductor
source("http://bioconductor.org/biocLite.R")  # only required the first time
biocLite()                                    # only required the first time
biocLite("packagename")

# install packages from GitHub
install.packages("devtools")                  # only required the first time
devtools::install_github("username/packagename")


# your turn!



# Loading Packages --------------------------------------------------------

# loading packages
library(tidyverse)
stringr::str_replace()

# getting help on packages
help(package = "tidyr")
vignette(package = "tidyr")
browseVignettes("KraljicMatrix")
vignette("tidy-data")


#############
# R Objects #
#############

# Atomic Vectors ----------------------------------------------------------

# what are atomic vectors?
example_vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
is.vector(example_vector)
is.character(example_vector)

# creating vectors
character_vector <- c("Hello", "how", "are", "you?")
numeric_vector <- seq(from = 1,
                      to = 39,
                      by = 2)
int_vector <- c(1L, 1e4L, -5L)
logical_vector <- c(TRUE, FALSE, TRUE)

# QA every day!
length(numeric_vector)
typeof(int_vector)
class(logical_vector)
test <- gl(2,2)
typeof(test) # returns atomic type
class(test) # returns high-level type

# your turn!
#1
help(runif)
#2
rand_values <- runif(75, min=-3, max=14)
#3
# Is it an atomic vector?
is.vector(rand_values)
# What kind of vector is it?
typeof(rand_values)
# How long is it?
length(rand_values)

# Attributes --------------------------------------------------------------

nlp_vector <- c("Hello", "how are you")

# look at attributes
attributes(nlp_vector)

# the names attribute
names(nlp_vector)
names(nlp_vector) <- c("greeting", "followup question")
names(nlp_vector) <- NULL
names(nlp_vector)



# Vector Coercion ---------------------------------------------------------

# coercing vectors into different types
as.character(43)
as.logical(0)
as.numeric(TRUE)

# vector coercion is sometimes very useful
logical_vector
sum(logical_vector)
mean(logical_vector)



# Fundamental data structure: matrix --------------------------------------

# attributes of a matrix
VADeaths
typeof(VADeaths)
class(VADeaths)
nrow(VADeaths)
ncol(VADeaths)
dim(VADeaths)

ncol(logical_vector)
NCOL(logical_vector)

nrow(logical_vector)
NROW(logical_vector)

# your turn!
#1
help(matrix)
#2
example_matrix <- as.matrix(c(1:20), nrow=5, ncol=4, byrow=T)
#3
names(example_matrix)
class(example_matrix)
dim(example_matrix)


# Fundamental data structure: list ----------------------------------------

# example of a list
list(
  seq(3, 30, 3),
  letters,
  matrix(1:6, nrow = 2)
)



# Fundamental data structure: data frame ----------------------------------

# example of data frame
cool_df <- data.frame(
  # columns of data frame
  observation = c("a", "b", "c", "d"),
  rand_norm_value = rnorm(4),
  exclude_flag = c(TRUE, FALSE, FALSE, TRUE),
  # other options for data frame
  row.names = NULL,
  stringsAsFactors = FALSE
)

# examine data frame
cool_df
class(cool_df)
str(cool_df)

cool_df2 <- data.frame(
  # columns of data frame
  observation = c("a", "b", "c", "d"),
  rand_norm_value = rnorm(4),
  exclude_flag = c(TRUE, FALSE, FALSE, TRUE),
  # other options for data frame
  row.names = NULL,
  stringsAsFactors = T
)
cool_df2
class(cool_df2)
str(cool_df2)
