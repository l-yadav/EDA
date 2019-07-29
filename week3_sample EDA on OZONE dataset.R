#Latika Yadav
#ANLY 506-52- B-2019/Summer - Exploratory Data Analytics
#Code Portfolio - Week - 3 - EDA Checklist using US EDA OZONE 2017 File provided in moodle

#using readr libraby to import csv file provided in moodle
library(readr)
library(dplyr)
library(lubridate)

#load file
setwd("~/Documents/Harrisburg/EDA 506 52 B/R/Workshop-R-Workflow/Data/week3")
ozone <- read_csv("US EPA data 2017.csv") #Columns 55, 66869 rows

#rewrite the names of the columns to remove any spaces and adds "." (State Code -> State.Code)
names(ozone) <- make.names(names(ozone))

#Columns 55, 66869 rows
nrow(ozone)
ncol(ozone)

#Structure
str(ozone)

#Look at the top and the bottom of your data, checking columns 6,7,10 - Latitude, Longitude, Sample.Duration
head(ozone[, c(6:7, 10)])
tail(ozone[, c(6:7, 10)])

#Check your “n”s
odate<- date(ozone$X1st.Max.DateTime)
otime_minute<- minute(ozone$X1st.Max.DateTime)
new_ozone<- cbind(ozone, odate, otime_minute)
off_hour<- subset(new_ozone, new_ozone$otime_minute!=0)
select(off_hour, State.Name) %>% unique %>% nrow #34 states where ozone monitoring is happening off-hours 
#check how many states original data had
select(ozone, State.Name) %>% unique %>% nrow #54 states
unique(ozone$State.Name) # Extra states: District Of Columbia, Virgin Islands, Country Of Mexico, Puerto Rico
freq<-count(off_hour, off_hour$State.Name) #Hawaii, Florida, Pennsylvania out of the 34 states have most cases where off-hour monitoring is happening

#Mean of Observation.Count
mean(ozone$Observation.Count)
#summary of ozone counts
summary(ozone$X1st.Max.Value) 
#summary shows data has potential huge outliers affecting the mean of the observed ozone values since the median is very small

#checking the data distribution using quantiles
quantile(ozone$X1st.Max.Value, seq(0, 1, 0.1))

#Which counties in the United States have the highest levels of ambient ozone pollution?

#step 1: arrange the dataset by state name and county name using X1st values in df named "ranking"
ranking <- group_by(ozone, State.Name, County.Name) %>%
summarize(ozone = mean(X1st.Max.Value)) %>%
as.data.frame %>%
arrange(desc(ozone))
#step 2: lookup top 10 locations using head()
head(ranking, 10) #Florida, New Mexico, Rhode Island and NY are on top of the list

#step 3: lookup bottom 10 locations using tail()
tail(ranking, 10)

#step 4: check for number of observations in the Broward county with highest counts
filter(ozone, State.Name == "Florida" & County.Name == "Broward") %>% nrow #246 obs

#step 5; finding out ozone monthly averages
ozone <- mutate(ozone, X1st.Max.DateTime = as.Date(X1st.Max.DateTime))
filter(ozone, State.Name == "Florida" & County.Name == "Broward") %>%
mutate(month = factor(months(X1st.Max.DateTime), levels = month.name)) %>%
group_by(month) %>%
summarize(ozone = mean(X1st.Max.Value)) #seeing huge values and possibly errored values for the month of Jan/Apr/Nov

# Setting up a sample dataset
set.seed(10234)
N <- nrow(ozone)
idx <- sample(N, N, replace = TRUE)
ozone2 <- ozone[idx, ]

#recreating the same ranking as before
ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
summarize(ozone = mean(X1st.Max.Value)) %>%
as.data.frame %>%
arrange(desc(ozone))

#compare the 2 rankings
cbind(head(ranking, 10),head(ranking2, 10))

#sampled data shows similar results with New Mexico, Florida and New York on top of the list with only Rhode Island's ranking different
