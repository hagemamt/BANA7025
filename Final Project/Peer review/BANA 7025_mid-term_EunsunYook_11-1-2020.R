library(magrittr)   
library(tidyverse)  
library(car)        
library(knitr)      
library(psych) 

# Importing .csv files 
hotels <- read.csv("data/hotels.csv") 
# View the structure of hotels
str(hotels)
# View the first 6 rows of hotels
head(hotels)

hotels$is_canceled  <- as.factor(hotels$is_canceled)
hotels$arrival_date_year   <- as.factor(hotels$arrival_date_year )
hotels$arrival_date_week_number  <- as.factor(hotels$arrival_date_week_number )
hotels$arrival_date_day_of_month   <- as.factor(hotels$arrival_date_day_of_month)
hotels$is_repeated_guest   <- as.factor(hotels$is_repeated_guest)
hotels$previous_cancellations  <- as.factor(hotels$previous_cancellations)
hotels$previous_bookings_not_canceled <- as.factor(hotels$previous_bookings_not_canceled)
hotels$previous_cancellations  <- as.factor(hotels$previous_cancellations)
hotels$booking_changes  <- as.factor(hotels$booking_changes)

#create new variable: arrival_date
hotels$arrival_date <-paste(hotels$arrival_date_month, 
                            hotels$arrival_date_day_of_month,
                            hotels$arrival_date_year,sep="-")
as.Date(hotels$arrival_date, format="%B-%d-%Y")
#create new variable: stays_nights
hotels %>% mutate(stays_nights= stays_in_weekend_nights + stays_in_week_nights) -> hotels_new

summary(hotels_new)

unique(hotels_new$meal)
str(hotels_new$meal)
sum(hotels_new$meal == "Undefined")
sum(hotels_new$meal == "SC")
hotels_new$meal[hotels_new$meal == "Undefined"] <- "SC"

#levels(hotels_new$meal)[5] <- "SC"

colSums(is.na(hotels_new))

hotels_new$children[is.na(hotels_new$children)]  <- 0

hist(hotels_new$adr)
boxplot(hotels_new$adr) 

Boxplot(~adr, data=hotels_new, id=list(n=Inf)) 

hotels_new[48516, 28] <- NA # row num.  is 48516; column num. for adr is 28
boxplot(hotels_new$adr)

hotels_var_names <- colnames(hotels_new)
hotels_var_type <- lapply(hotels_new, class)
hotels_var_desc <- c("Hotel (City Hotel or Resort Hotel)",
                     "Value indicating if the booking was canceled (1) or not (0)",
                     "Number of days that elapsed between the entering date of the booking into the PMS and the arrival date",
                     "Year of arrival date",
                     "Month of arrival date",
                     "Week number of year for arrival date",
                     "Day of arrival date",
                     "Number of weekend nights (Saturday or Sunday) the guest stayed or booked to stay at the hotel",
                     "Number of week nights (Monday to Friday) the guest stayed or booked to stay at the hotel",
                     "Number of adults",
                     "Number of children",
                     "Number of babies",
                     "Type of meal booked. SC - no meal package; BB - Bed & Breakfast; HB - Half board (breakfast and one other meal - usually dinner); FB - Full board (breakfast, lunch and dinner)",
                     "Country of origin. Categories are represented in the ISO 3155-3:2013 format",
                     "Market segment designation. In categories, the term "TA" means "Travel Agents" and "TO" means "Tour Operators"",
                     "Booking distribution channel. The term "TA" means "Travel Agents" and "TO" means "Tour Operators"",
                     "Value indicating if the booking name was from a repeated guest (1) or not (0)",
                     "Number of previous bookings that were cancelled by the customer prior to the current booking",
                     "Number of previous bookings not cancelled by the customer prior to the current booking",
                     "Code of room type reserved. Code is presented instead of designation for anonymity reasons",
                     "Code for the type of room assigned to the booking. Sometimes the assigned room type differs from the reserved room type due to hotel operation reasons (e.g. overbooking) or by customer request. Code is presented instead of designation for anonymity reasons",
                     "Number of changes/amendments made to the booking from the moment the booking was entered on the PMS until the moment of check-in or cancellation",
                     "Indication on if the customer made a deposit to guarantee the booking. This variable can assume three categories: No Deposit - no deposit was made; Non Refund - a deposit was made in the value of the total stay cost; Refundable - a deposit was made with a value under the total cost of stay.",
                     "ID of the travel agency that made the booking",
                     "ID of the company/entity that made the booking or responsible for paying the booking. ID is presented instead of designation for anonymity reasons",
                     "Number of days the booking was in the waiting list before it was confirmed to the customer",
                     "Type of booking,  assuming one of four categories: Contract - when the booking has an allotment or other type of contract associated to it; Group - when the booking is associated to a group; Transient - when the booking is not part of a group or contract, and is not associated to other transient booking; Transient-party - when the booking is transient, but is associated to at least other transient booking",
                     "Average Daily Rate as defined by dividing the sum of all lodging transactions by the total number of staying nights",
                     "Number of car parking spaces required by the customer",
                     "Number of special requests made by the customer (e.g. twin bed or high floor)",
                     "Reservation last status, assuming one of three categories: Canceled - booking was canceled by the customer; Check-Out - customer has checked in but already departed; No-Show - customer did not check-in and did inform the hotel of the reason why",
                     "Date at which the last status was set. This variable can be used in conjunction with the ReservationStatus to understand when was the booking canceled or when did the customer checked-out of the hotel",
                     "Arrival Date",
                     "Stays in nights"
)

data_desc_df <- as_data_frame(cbind(hotels_var_names, hotels_var_type, hotels_var_desc))
colnames(data_desc_df) <-c("Variable", "Type", "Description")
kable(data_desc_df)

hotels_new %>% summary()

# Plot histograms of numeric columns
multi.hist(hotels_new[,sapply(hotels_new, is.numeric)])

# hotel and cancellation
hotels_new %>% ggplot( aes(x=hotel, 
                           fill=is_canceled)) + geom_bar() +
  scale_fill_discrete(labels=c("not canceled", "canceled")) +
  guides(fill=guide_legend(title=NULL))

# hotel and cancellation - facet: customer_type
hotels_new %>% ggplot( aes(x=hotel, 
                           fill=is_canceled)) + geom_bar() +
  scale_fill_discrete(labels=c("not canceled", "canceled")) +
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(. ~ customer_type) + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# hotel and cancellation - facet: deposit_type
hotels_new %>% ggplot( aes(x=hotel, 
                           fill=is_canceled)) + geom_bar() +
  scale_fill_discrete(labels=c("not canceled", "canceled")) +
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(. ~ deposit_type) + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# hotel and cancellation - facet: market_segment
hotels_new %>% ggplot(aes(x=hotel, 
                          fill=is_canceled)) + geom_bar() +
  scale_fill_discrete(labels=c("not canceled", "canceled")) +
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(. ~ market_segment) + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

#Lead_time for canceled and not canceled group
hotels_new$arrival_date_month<- factor(hotels_new$arrival_date_month, levels = 
                                         c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


hotels_new %>% ggplot( aes(x=arrival_date_month, 
                           y=lead_time, fill=is_canceled))+
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_discrete(labels=c("not canceled", "canceled")) +
  guides(fill=guide_legend(title=NULL)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# scatter plot between adr and lead_time
hotels_new %>% ggplot(aes(x=adr, y=lead_time)) +geom_point()
# scatter plot between adr and stays_nights
hotels_new %>% ggplot(aes(x=adr, y=stays_nights)) +geom_point()