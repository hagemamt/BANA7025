library(readxl)

#1.
excel_sheets('week 2/aircraft.xlsx')

#2.
data <- read_excel('week 2/aircraft.xlsx', sheet = 'Trainers', skip = 3)
data

#3. - There are some negative values
summary(data)
str(data)

#4.
unique(data$MD)
table(data$MD)

#5. - No missing years
sum(is.na(data$FY))
unique(data$FY)
table(data$FY)

#6. - 109828.8
quantile(data$FH, prob = .9)

#7. - 567 to 158,058,171: 158,057,604
range(data$Cost)
max(data$Cost) - min(data$Cost)
diff(range(data$Cost))

#8.
hist(data$FH)

#9.
boxplot(FH ~ MD, data = data)

#10.
barplot(table(data$FY))
