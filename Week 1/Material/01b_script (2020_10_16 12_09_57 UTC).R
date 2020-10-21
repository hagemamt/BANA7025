###############################################################################
# 01b_script.R
# Script to follow in-class examples
###############################################################################


####################
# Selecting Values #
####################


# Selecting values from vectors -------------------------------------------

# your turn!
#1
set.seed(1234)
my_vector <- rnorm(n = 30,
                   mean = 0,
                   sd = 5)
names(my_vector) <- letters
#2
my_vector[]
my_vector[5]
my_vector[3:7]
my_vector[c(1,3,6)]
my_vector[-(2:14)]
my_vector[0]
my_vector[c(TRUE, FALSE, TRUE, FALSE, FALSE)]
my_vector[c("b", "n", "r")]



# Extracting values from matrices and data frames -------------------------

# your turn!
#1
?WorldPhones
str(WorldPhones)
dim(WorldPhones)
names(WorldPhones)

#2a
WorldPhones[1,]
#2b
WorldPhones[c(2,3,6), c(2:5)]
#2c
WorldPhones[,c(3,3)]
#2d
WorldPhones[,-c(1,7)]
#2e
WorldPhones[-c(1:7),-c(1:7)] # or WorldPhones[0,0]
#2f
WorldPhones[nrow(WorldPhones) - 1,]


# Selecting columns from data frames --------------------------------------

# preserve
mtcars["disp"]
mtcars[3]

# simplify
mtcars[["disp"]] # does not preserve the structure of the data frame
mtcars[[3]]

# simplify
mtcars$disp # does not preserve the structure of the data frame
typeof(mtcars) # list
typeof(mtcars$disp) # vector
mtcars$3



# Selecting values from lists ---------------------------------------------

# your turn!
#1
model <- lm(mpg ~ ., data = mtcars)
#2
str(model)
#3a
model['fitted.values']
#3b
model$coefficients[['wt']]
#3c
model[['residuals']][1:10]


####################
# Modifying Values #
####################


# Modifying vector values -------------------------------------------------

# modifying a single vector value
lame_dumb_vector <- c(1, 2, 3, 4, 5)
lame_dumb_vector[1] <- 8675309
lame_dumb_vector

# modifying many vector values
lame_dumb_vector[c(1, 3)] <- c(867, 5309)
lame_dumb_vector[c(2, 4, 5)] <- -1
lame_dumb_vector



# Modifying data frame values ---------------------------------------------

# modifying values
iris_df <- iris
iris_df$Sepal.Length[1:3] <- 1000
head(iris_df)

# logical comparisons
1 != 2
3 > c(5, 1, 3)
c(5, 1, 3) == c(3, 1, 5)
1 %in% c(5, 1, 3)
c(19, 3) %in% c(5, 1, 3)
c(1, 8, 0, 9) %in% c(5, 1, 3)

# modifying vectors with logic
# what does each command do?
iris_df <- iris
iris_df$Petal.Width > 1
iris_df$Petal.Width[iris_df$Petal.Width > 1]
iris_df$Petal.Width[iris_df$Petal.Width > 1] <- -3
iris_df$Petal.Width

# subset then select data frame columns
iris[iris$Sepal.Length > 7.3, ]
iris[iris$Sepal.Length > 7.3 & iris$Petal.Length > 6.3, ]
iris[iris$Sepal.Length > 7.3 | iris$Petal.Width < 1.8, ]


# your turn!
#1
help(esoph)
#2
head(esoph)
esoph[esoph$tobgp %in% c('0-9g/day','10-19') & esoph$ncases > 0,]


################
# Missing Data #
################


# Missing values in R -----------------------------------------------------

# how missing values mess with calculations
8 + NA
mean(c(1, 2, 3, NA, 5))
mean(c(1, 2, 3, NA, 5), na.rm = TRUE)

# counting missing values
df <- data.frame(col1 = c(1:3, NA),
                 col2 = c("this", NA,"is", "text"), 
                 col3 = c(TRUE, FALSE, TRUE, TRUE), 
                 col4 = c(2.5, NA, NA, NA),
                 stringsAsFactors = FALSE)
is.na(df)
sum(is.na(df))

df
# removing incomplete observations
complete.cases(df) # which rows are complete cases?
df[complete.cases(df), ]
df[!complete.cases(df), ]
na.omit(df)


# your turn!
#1
sum(is.na(airquality))
#2
colSums(is.na(airquality))
#3
airquality_copy <- airquality
airquality_copy$Ozone[is.na(airquality$Ozone)] <- 
  mean(airquality_copy$Ozone, na.rm=T)
mean(airquality_copy$Ozone)
#4
colSums(is.na(airquality_copy))
airquality_complete <- complete.cases(airquality)
# or na.omit(airquality)

####################
# Get To Know Data #
####################


# Example of Base R plots -------------------------------------------------

# example of scatterplot
plot(iris$Sepal.Length, iris$Sepal.Width)


# your turn!
#1
plot(chickwts$feed, chickwts$weight)
boxplot(weight ~ feed, data = chickwts)
#2
table(chickwts$feed)
#3
hist(chickwts$weight)
