###############################################################################
# coding_exercises_1_20201013.R
# Mark Hageman
# BANA 7025-001
# Oct. 12, 2020
###############################################################################

# Session Setup -----------------------------------------------------------
# 1.b
setwd('C:/users/Mark/Documents/BANA 7025 - Data Wrangling/Week 1/Material')


# Problem #2 --------------------------------------------------------------


#2.a
data <- read.csv('customer_churn_data_cleaning.csv', stringsAsFactors = F)

#2.b,c - There are 3 missing values out of 50 
#      in the 10 observations of 5 variables
data
str(data)
sum(is.na(data))
colSums(is.na(data))

#2.d
head(data, 5)


# Problem #3 --------------------------------------------------------------


#3.a
names(data)
names(data) <- c("customer_id", "monthly_charges", "total_charges", 
                 "payment_method", "churn")
#3.b
names(data)

#3.c
data$total_charges <- as.numeric(data$total_charges)
data$total_charges

#3.d
str(data)
 
#3.e
dim(data)
nrow(data)
ncol(data)


# Problem #4 --------------------------------------------------------------


#4.a - There are five missing values
sum(is.na(data))

#4.b
colSums(is.na(data))

#4.c
data$monthly_charges[is.na(data$monthly_charges)] <- 
  median(data$monthly_charges, na.rm = T)
data$total_charges[is.na(data$total_charges)] <- 
  median(data$total_charges, na.rm = T)

#4.d
sum(data$payment_method %in% c("", "--"))
data$payment_method[data$payment_method %in% c("", "--")] <- "Not stated"
sum(data$payment_method %in% c("", "--"))


# Problem #5 --------------------------------------------------------------


#5.a - Only the "total_charges" and "churn" columns are useful because the
#      "monthly_charges" column appears to have an invalid value for 
#      observation #8, and three of the ten observations have "Not stated"
#      in the "payment_method" field.
summary(data)

#5.b
table(data$customer_id)
table(data$payment_method)
table(data$churn)

#5.c - I notice that the mean of this variable is way out of line due to
#      the invalid value in observation #8.
summary(data$monthly_charges)

#5.d - Yes. The outlier is obviously invalid data.
hist(data$monthly_charges)
boxplot(data$monthly_charges)

#5.e
data$monthly_charges[data$monthly_charges < -10] <- 0 

#5.f - Yes, much better
summary(data$monthly_charges)
hist(data$monthly_charges)
boxplot(data$monthly_charges)

#5.g - There is nothing obvious to be fixed.
summary(data$total_charges)
hist(data$total_charges)
boxplot(data$total_charges)



# Problem #6 --------------------------------------------------------------


#6. - There does not appear to be any discernible relationship between 
#     monthly_charges and total_charges.
plot(data$monthly_charges, data$total_charges)












