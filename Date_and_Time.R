#### Environment ####
library(tidyverse)
library(dslabs)
library(lubridate)
library(purrr)
library(pdftools)

data("polls_us_election_2016")
head(polls_us_election_2016)

data("brexit_polls")
head(brexit_polls)

data("movielens")
head(movielens)
#####

# Date in R
# special class: date
# if use as.numeric() for date, 
# turns into days since the epoch (January 1, 1970)

#### Extract and Convert Dates ####
set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% 
  sort
dates

# extract dates
tibble(date = dates,
       month = month(dates),
       day = day(dates),
       year = year(dates)
       )
month(dates, label = T) # can also extract labels for months

x <- "09/01/02"

# convert dates
ymd(x) #年月日
ydm(x)
myd(x)
myd(x)
dmy(x)
dym(x) #所有可能性都有
#####



#### Extract and Convert Time ####

now() # get time by timezone (from location)
now("GMT") # define timezone

# extract time
tibble(time = now(),
       hour = hour(now()),
       minute = minute(now()),
       second = second(now())
)

# convert time
x <- c("12:34:56")
hms(x)
x <- "Nov/2/2012 12:34:56" 
mdy_hms(x) # date and time

#####



#### Other useful functions ####
# make date
make_date(1970, 7, 6)
make_date(1960:1970)
make_date(1980:1989, 3:8, 18:23) # can also custom range, default is 1

# round date to nearst
polls_us_election_2016 %>%
  mutate(week = round_date(startdate, "week")) %>% 
  group_by(week) %>%
  summarize(margin = mean(rawpoll_clinton - rawpoll_trump)) %>% 
  qplot(week, margin, data = .)

#####



#### Exercise ####

# 1 - 4
tibble(date = brexit_polls$startdate,
       month = month(brexit_polls$startdate)) %>%
  filter(month == 4) %>%
  count()

brexit_polls %>%
  mutate(endweek = round_date(enddate, "week")) %>%
  filter(endweek == "2016-06-12") %>%
  count()

brexit_polls %>%
  mutate(endday = weekdays(enddate)) %>%
  count(endday) %>%
  arrange(desc(n))

# 5
reviews <- movielens %>%
  mutate(time = as_datetime(movielens$timestamp))

reviews <- movielens %>%
  mutate(review_year = year(reviews$time),
         review_hour = hour(reviews$time)) 

reviews %>%
  count(review_year) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

reviews %>%
  count(review_hour) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

#####




  










