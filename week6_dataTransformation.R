#Latika Yadav
#ANLY 506-52- B-2019/Summer - Exploratory Data Analytics
#Code Portfolio - Week -6 - Data Transformation

library(nycflights13)
library(tidyverse)
flights<- nycflights13::flights
#All verbs work similarly:
#The first argument is a data frame.
#The subsequent arguments describe what to do with the data frame, using the variable names (without quotes).
#The result is a new data frame.

##### Filter
#Filter rows with filter()
filter(flights, month == 12, day == 30)
#R either prints out the results, or saves them to a variable. If you want to do both, you can wrap the assignment in parentheses:
(dec25 <- filter(flights, month == 12, day == 25))

#Comparisons
filter(flights, month == 1) #not = but ==
#Logical Operators
filter(flights, month == 11 | month == 12) 
#flights that weren’t delayed (on arrival or departure) by more than two hours
filter(flights, !(arr_delay > 120 | dep_delay > 120))

#Missing values
#to determine if a value is missing, use is.na():
x<-NA
is.na(x)
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)


#Exercises
#Find all flights that:

names(flights)
#Had an arrival delay of two or more hours
filter(flights, arr_delay>=120)

#Flew to Houston (IAH or HOU)
filter(flights, dest=="IAH" | dest=="HOU")

#Were operated by United, American, or Delta
count(flights, carrier)
carrier_DL_AA_UA<- filter(flights, carrier=="UA" | carrier=="AA" | carrier=="DL")
count(carrier_DL_AA_UA, carrier) #spotchecking, counts of all the mentioned airlines match

#Departed in summer (July, August, and September)
filter(flights, month==7 | month==8 | month==9)

#Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay > 120 & dep_delay == 0)

#Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, arr_delay<= 30 & dep_delay >= 60)

#Departed between midnight and 6am (inclusive)
filter(flights, between(dep_time, 0000, 600))
       
#How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
count(flights, is.na(flights$dep_time)) #8255
table(is.na(flights$dep_time))

#######Arrange

#Arrange rows with arrange()
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))

#Missing values are always sorted at the end
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

#Exercises
#How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(df, desc(is.na(x)))

#Sort flights to find the most delayed flights. Find the flights that left earliest.
head(arrange(flights, desc(dep_delay)))
head(arrange(flights, dep_delay))

#Sort flights to find the fastest flights.
arrange(flights, distance/(air_time* 60))

#Which flights travelled the longest? Which travelled the shortest?
arrange(flights, air_time) 
arrange(flights, desc(air_time))

#### Select
#Select columns with select()

#Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

#There are a number of helper functions you can use within select():
#starts_with("abc"): matches names that begin with “abc”.
#ends_with("xyz"): matches names that end with “xyz”.
#contains("ijk"): matches names that contain “ijk”.
#matches("(.)\\1"): selects variables that match a regular expression. This one matches any variables that contain repeated characters.
#num_range("x", 1:3): matches x1, x2 and x3.

#moving select colums first and everything towards the end
select(flights, time_hour, air_time, everything())

rename(flights, tail_num = tailnum)

#What happens if you include the name of a variable multiple times in a select() call?
select(flights, time_hour, air_time, time_hour) #prints repeated column only once

#What does the one_of() function do? Why might it be helpful in conjunction with this vector?
#Matches variable names in a character vector
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

#Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE)) #ignores all columns since all columns have time in small letters

#Add new variables with mutate()
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
#adding two new columns gain and speed
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / (air_time * 60))

#If you only want to keep the new variables, use transmute():
transmute(flights,
            gain = dep_delay - arr_delay,
            hours = air_time / 60,
            gain_per_hour = gain / hours)

####Grouped summaries with summarise()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#calculating avg. delay by day
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

#using pipe to use multiple functions
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
#selecting/filtering flights with flight count>20 and destination is not HNL
delay <- filter(delay, count > 20, dest != "HNL")

#does distance and delay correlate?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# delays increase with distance up to ~500 miles and then decreases. 
#reason 1 as given in the book: Maybe as flights get longer there's more ability to make up delays in the air
#reason 2 that I think this happens is: The estimated arrival time for flights already have some delay time accounted for. 
#In case of shorter flights the allowed possible delay is small and thus if longer delays happen the differnece between actual and estimated arrival time is large as compared to longer filghts where the
#allowed possible delay is large and thus there is more time for the flight to cover actual delays and still have shorter differnce between actual and esitmated arrivals.

#counts
#counting by arrival delay times and flights not cancelled (using is.na())
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
#grouping the not_cancelled data by year,month,day
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )
#seeing frequency plot showing count of delays
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# another example of counts: Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

### Grouped mutates (and filters)
#ranking by delays < 10
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
#finding popular destinations
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests
#per group calculations
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)