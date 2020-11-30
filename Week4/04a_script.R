###############################################################################
# 04a_script.R
# Script for more Tidyverse packages with data wrangling
###############################################################################


# Prerequisites -----------------------------------------------------------

# set working directory


# load packages
library(magrittr)
library(tidyverse)
library(lubridate)
library(glue)

# import data
crime <- read_csv("week4/cincinnati_crimes_20190812.csv")



# Intro: Logicals ---------------------------------------------------------

# creating boolean values
2 <= 3
!is.na(letters[1:15])

# values can be logical
typeof(TRUE)
typeof(FALSE)

# vectors can be logical
x <- c(TRUE, NA, FALSE)
typeof(x)

# create a logical variable
## generation z
crime %>% 
  select(INCIDENT_NO, SUSPECT_AGE) %>% 
  mutate(gen_z = SUSPECT_AGE %in% c("UNDER 18", "18-25"))

# generating insights from logicals
## count TRUEs by summing
x <- c(8, 4, 5, 1)
x
sum(x)
## find proportion of TRUEs by taking the mean
crime %>% 
  select(INCIDENT_NO, SUSPECT_AGE) %>% 
  mutate(gen_z = SUSPECT_AGE %in% c("UNDER 18", "18-25")) %>% 
  summarize(pct_gen_z = mean(gen_z, na.rm = TRUE))


# Your turn!
crime %>%
  group_by(DAYOFWEEK) %>%
  mutate(occurred_in_clifton = (SNA_NEIGHBORHOOD %in% c("CLIFTON"))) %>%
  summarize(num_clifton  = sum(occurred_in_clifton, na.rm = T),
            num_total = n(),
            pct_clifton = mean(occurred_in_clifton, na.rm = T),
            pct_clifton2 = num_clifton/num_total)
  # summarize(pct_in_clifton = mean(occurred_in_clifton, na.rm = T))



# Intro: Tibbles ----------------------------------------------------------

# creating tibbles
as_tibble(iris)

# create tibbles from individual vectors
# (recycling occurs)
tibble(
  division = c("Columbus",
               "Nashville",
               "Atlanta"),
  test_group = 1,
  # use backticks for non-syntactical name
  `:)_order` = 1:3
)

# differences in print methods
## tibble :)
as_tibble(iris)
## base R :(
iris

# subsetting with base R
## matrix subsetting simplifies
cars[, "speed"]
## list subsetting doesn't simplify
cars["speed"]

# subsetting with tibbles
cars %>% 
  as_tibble() %>% 
  # use the placeholder .
  # when piping into [ ] or [[ ]] or $
  .[, "speed"]



# Strings -----------------------------------------------------------------

# matching patterns with str_detect()
x <- c("apple", "pineapple",
       "crabapple", NA, "peach")
str_detect(x, "app")

# creating variables with str_detect()
crime %>% 
  select(HATE_BIAS) %>% 
  mutate(hate_toward_group = str_detect(HATE_BIAS, "ANTI-"))

# Your turn!
length(str_detect(crime$CLSD, regex("CLOSED", ignore_case = T)))

crime %>%
  select(CLSD) %>%
  mutate(closed_case = str_detect(CLSD, 
                                  regex("CLOSED", ignore_case = T))) %>%
  summarize(num_closed = sum(closed_case, na.rm = T),
            pct_closed = mean(closed_case, na.rm = T))

# how to ignore case
crime %>% 
  select(CLSD) %>% 
  mutate(closed_case = str_detect(CLSD,
                                  regex("cLoSeD", ignore_case = TRUE))) %>% 
  summarize(num_closed = sum(closed_case, na.rm = TRUE),
            pct_closed = mean(closed_case, na.rm = TRUE))

# your first regular expression
# match pattern at beginning of string
crime %>% 
  filter(str_detect(SNA_NEIGHBORHOOD, "^MT.")) %>% 
  count(SNA_NEIGHBORHOOD, sort = TRUE)
# match pattern at end of string
crime %>% 
  filter(str_detect(SNA_NEIGHBORHOOD, "HILL$")) %>% 
  count(SNA_NEIGHBORHOOD, sort = TRUE)
# check for multiple regular expressions
# at the same time
crime %>% 
  filter(str_detect(SNA_NEIGHBORHOOD,
                    "^MT.|HILL$|SOUTH")) %>% 
  count(SNA_NEIGHBORHOOD, sort = TRUE)

# quantifiers
## look for suspect ages in double-digits
crime %>% 
  filter(str_detect(SUSPECT_AGE, "^[0-9]{2}")) %>% 
  count(SUSPECT_AGE)

# extract location code with defined start/end positions
crime %>% 
  transmute(LOCATION,
            location_code = str_sub(string = LOCATION,
                                    start = 1,
                                    end = 2))
# extract last three digits by counting backwards from the last character
crime %>% 
  transmute(ZIP,
            last_three = str_sub(ZIP, -3))

# str_length()
# create variable to count number of digits in zip code
crime %>%
  transmute(ZIP = as.character(ZIP),
            num_digits_zip = str_length(ZIP),
            fixed_zip = str_pad(string = ZIP,
                                width = 5,
                                side = "right",
                                pad = "X")) %>% 
  filter(num_digits_zip < 5)

# Your turn!
# fill in the blanks!
crime %>% 
  # select a few variables
  select(HOUR_FROM, ZIP) %>% 
  mutate(
    # change hour_from to a character
    HOUR_FROM = as.character(HOUR_FROM),
    # left-pad zeroes to create 24-hour time
    HOUR_FROM = str_pad(string = HOUR_FROM,
                        width = 4,
                        side = "left",
                        pad = "0"),
    # change zip to a character
    ZIP = as.character(ZIP),
    # make if-then statement to right-pad zip codes less than 5 digits
    ZIP = if_else(
      # check the condition for the if_else function
      condition = str_length(ZIP) < 5,
      # if less than 5 digits, right-pad an X
      true = str_pad(ZIP, 5, "right", "x"),
      # otherwise keep the zip code as-is
      false = ZIP)
  )

# converting case and trimming whitespace
## a lame example vector
x <- c("VEG SOUP", " MEXED VEG/VEG MEDLEY", "bAd NaMe 4 VeG ")
## str_to_lower()--there is also str_to_upper() and str_to_title()
str_to_lower(x)
## str_trim removes whitespace from the side(s) you specify
str_trim(x)

# replacing patterns
## str_replace replaces the first matched pattern
str_replace(x,
            pattern = "VEG",
            replacement = "VEGETABLE")
# str_replace_all replaces all matched patterns
str_replace_all(x,
                pattern = "VEG",
                replacement = "VEGETABLE")



# Factors -----------------------------------------------------------------

# how R represents and stores factors
(eyes <- base::factor(x = c("blue", "green", "green"),
                      levels = c("blue", "brown", "green")))
# factors stored as an integer vector with a levels attribute
unclass(eyes)


# graphing without reordering factor levels
## create a new data set
age <- crime %>% 
  # filter suspect ages simply for readability
  filter(SUSPECT_AGE != "UNKNOWN")
## notice how SUSPECT_AGE is a character variable
age %>%
  count(SUSPECT_AGE)

# reorder levels with fct_relevel()
## relevel the SUSPECT_AGE variable
age_releveled <- age %>% 
  # fct_relevel() converts characters to factors
  mutate(SUSPECT_AGE = fct_relevel(SUSPECT_AGE,
                                   "UNDER 18",
                                    "18-25",
                                    "26-30",
                                    "31-40",
                                    "41-50",
                                    "51-60",
                                    "61-70",
                                    "OVER 70"))
## SUSPECT_AGE is now a factor that we reordered!
age_releveled %>% count(SUSPECT_AGE)

# recode levels with fct_recode()
age_recoded <- age_releveled %>% 
  mutate(
    SUSPECT_AGE = fct_recode(
      SUSPECT_AGE,
      #  new = old
      "< 18" = "UNDER 18",
      "> 70" = "OVER 70"
    )
  )


# Your turn!
crime %>% 
  mutate(suspect_generation = case_when(SUSPECT_AGE == "UNDER 18" ~ "student",
                                        SUSPECT_AGE == "OVER 70"  ~ "retired",
                                        is.na(SUSPECT_AGE)        ~ NA_character_,
                                        TRUE                      ~ "working adult"),
         suspect_generation = fct_relevel(suspect_generation,
                                          "student", "working adult", "retired")) %>% 
  ggplot(aes(x = suspect_generation)) +
    geom_bar() +
    labs(x = "Suspect Generation",
         y = "Frequency") +
    coord_flip() +
    theme_minimal()

# collapse factors with fct_collapse()
## show unique values for DAYOFWEEK variable
crime %>% 
  distinct(DAYOFWEEK)
## collapse these into 2 levels
day <- crime %>% 
  mutate(
    type_of_day = fct_collapse(
      DAYOFWEEK,
      weekday = c("MONDAY", "TUESDAY",
                  "WEDNESDAY", "THURSDAY",
                  "FRIDAY"),
      weekend = c("SATURDAY", "SUNDAY")
    ),
    # give missing values an explicit factor level
    # ensure they appear in summaries and on plots
    type_of_day = fct_explicit_na(type_of_day)
  )

# look at table to confirm collapsed levels
day %>% count(type_of_day)


# temporarily reorder factors
## check levels of type_of_day
base::levels(day$type_of_day)
## count records with each level
day %>% count(type_of_day)
## fct_infreq() orders by frequency
day %>% 
  ggplot(aes(x = fct_infreq(type_of_day))) +
    geom_bar() +
    coord_flip()
## fct_rev() orders by frequency
day %>% 
  ggplot(aes(x = fct_rev(type_of_day))) +
  geom_bar() +
  coord_flip()


# Your turn!
crime %>% 
  transmute(
    VICTIM_GENDER = fct_explicit_na(VICTIM_GENDER), 
    VICTIM_GENDER = fct_collapse(
      VICTIM_GENDER,
      female = c("FEMALE", "F - FEMALE"),
      male = c("MALE", "M - MALE"),
      non_person = "NON-PERSON (BUSINESS",
      not_reported = c("(Missing)", "UNKNOWN")
      )
    ) %>%
  count(VICTIM_GENDER) %>% 
  ggplot(aes(x = fct_reorder(VICTIM_GENDER, n),
             y = n)) +
    geom_col() +
    labs(x = NULL,
         y = "Frequency")



# Lubridate ---------------------------------------------------------------

# lubridate handles many string formats
## year, month, day
ymd("2019-08-20")
## some parsing functions allow unquoted numbers
ymd(20190820)
## day, month, year, hour
dmy_h("20/08/2019 14")
## year, day, month, hour, minute
ydm_hm("2019/20/08 07:20")
## month, day, year, hour, minute, second
mdy_hms("August 20, 2019 10:12:32")


# extract boolean components
## check if datetime in am
am("2019-08-20 17:00:00")
## check for daylight savings time
dst(now())
## check for leap year (requires date input)
x <- as_date("2019-08-20")
leap_year(x)

# extract numeric components
## extract year
year("2019-08-20")
## extract full weekday name
wday("2019-08-20", label = TRUE, abbr = TRUE)
## extract hour
hour("2019-08-20 02:42")
## extract calendar year quarter
quarter("2019-08-20")


# your turn!
crime %>% 
  # convert the DATE_REPORTED variable into
  # a datetime variable showing the month, day, year, hour, minute
  mutate(DATE_REPORTED = mdy_hm(DATE_REPORTED),
         # create a month variable by extracting the month
         # from the DATE_REPORTED variable
         month = month(DATE_REPORTED)) %>% 
  # what should you group by?
  group_by(month) %>% 
  # we need a total_victims statistic
  summarize(total_victims = sum(TOTALNUMBERVICTIMS, na.rm = TRUE)) %>% 
  # create a line graph to show change over time
  ggplot(aes(x = month, y = total_victims)) +
    geom_line()


# durations
(surge_age <- today() - ymd(19970727))
## store information as a duration
as.duration(surge_age)

(mark_age <- today() - ymd(19690226))
as.duration(mark_age)

# functions to create durations
dseconds(20)
dminutes(c(11, 525600))
dweeks(1:4)

# add and multiply durations
3 * dhours(1)
dyears(2) + dweeks(3) + dhours(1)

# add and subtract durations involving days
today() - dyears(2)


# where durations fail us
## leap years
(five_somewhere <- ymd_hms("2016-01-01 17:00:00"))
five_somewhere + dyears(1)
## daylight saving time
(hashtag_fall <- ymd_hms("2019-11-02 15:00:00", tz = "America/New_York"))
hashtag_fall + ddays(1)

# lubridate also uses periods
hashtag_fall
hashtag_fall + days(1)
# examples of creating periods
seconds(20)
minutes(c(11, 525600))
weeks(1:4)

# adding and multiplying periods
4 * (years(2) + minutes(3))
days(6) + minutes(600) + seconds(3)

# add periods to dates
## leap year
five_somewhere + dyears(1)
five_somewhere + years(1)
## daylight saving time
hashtag_fall + ddays(1)
hashtag_fall + days(1)
tday <- ymd("2020-11-02")
class(tday)
tday <- as_date("2020-11-02")
class(tday)
tday2 <- ymd(tday)
class(tday2)
