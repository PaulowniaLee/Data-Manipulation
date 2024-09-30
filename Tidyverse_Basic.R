#### Environment ####
install.packages("dslabs")
install.packages("NHANES")
library(tidyverse)
library(dslabs)
library(NHANES)
data(murders)
data(heights)
data("NHANES")
warnings()
#####

# Tidy Data #
# For the tidyverse packages to be optimally used, 
# data need to be reshaped into tidy format
# => will be discussed in Data Wrangling Part

#### dplyr basic ####
# select 
select (murders, state, population)
# can show certain columns of a dataframe

# filter 
filter(murders, state == "Texas")
# simple condition 
filter(murders, state %in% c("New York", "Virginia"))
# choose from a list of conditions
filter(murders, population < 5000000 | region == "Northeast")
# OR opertor
filter(murders, population > 5000000 & region == "West")
# AND opertor 

# Pipe Operator 
# sends the result of the left side to be the first argument of the 
# function on the right side 
16 %>% sqrt()
16 %>% sqrt() %>% log(base = 2)
# it can be used to refine data without changing original data frame
my_states <- murders %>%
  mutate (rate = total / population * 100000, rank = rank(-rate)) %>%
  # mutate add new variables
  filter (region %in% c("Northeast", "West") & rate <= 1) %>%
  select (state, rate, rank)

# Summarize 
# used to compute summary statistics (e.g. mean)
# Always return a data frame object
s <- heights %>% 
  filter (sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s
# take original data frame and keep only female data, then take mean & sd
# (of the height column)
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  pull(rate)
# Use Pull operator to make result a vector

# group_by 
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), 
            standard_deviation = sd(height))
# group heights by sex column: uesful for dividing categories
murders %>% 
  group_by(region) %>%
  summarize(median_rate = median(rate))

# Notes:
easier to use .by argument inside summarise function 
can group by more than one element, which is hard to achieve if we use group_by function




# Arrange 
# sort data frame by numeric column 可用于排名
murders %>% 
  arrange (desc(rate)) %>%
  top_n(7, rate)
# use descending rate to list states, and show top 7 with top_n 
murders %>%
  arrange(region, rate) %>%
  head()
# order by region, and within each region order by murder rate



# Exercise for basic dplyr #
mean(na_example, na.rm = T) # use na.rm argument to ignore NAs

# Step 1: Get standard group 
ref <- NHANES %>%
  filter (AgeDecade == " 20-29") %>%
  summarize(average = mean(BPSysAve, na.rm = T), 
            sd = sd(BPSysAve, na.rm = T))

ref_avg <- NHANES %>%
  filter (AgeDecade == " 20-29") %>%
  summarize(average = mean(BPSysAve, na.rm = T)) %>%
  pull(average)

# Step 2: Get blood presure for every female group 
NHANES %>%
  filter (Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = T), 
            sd = sd(BPSysAve, na.rm = T))

# Step 3: Get for blood presure for every group 
NHANES %>%
  group_by(Gender, AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = T), 
            sd = sd(BPSysAve, na.rm = T))

# Step 4: Compare across race
NHANES %>%
  filter(AgeDecade == " 40-49") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = T)) %>%
  arrange(desc(average))



# Tibbles #
# The tbl, pronounced tibble, is a special kind of data frame. 
# The functions group_by and summarize 
# always return this type of data frame.

# manipulation verbs (select, filter, mutate, arrange) keep data type

# can create tibble with tibble()
# can covert data frame to tibble with as_tibble



# Dot Operator
# an alternative to pull
rates <- filter (murders, region == "South") %>%
  mutate(rate = total/population * 10^5) %>%
  .$rate
median(rates)

# Do
# tidyverse functions consistently return data frames
# but most R functions do not recognize grouped tibbles 
# nor do they return data frames
# => use do as bridge 
my_summary <- function(dat){
  x <- quantile(dat$height, c(0, 0.5, 1))
  return(tibble(min = x[1], median = x[2], max = x[3]))
}

heights %>%
  group_by(sex) %>%
  do(my_summary(.))
# Define a function, then use "do" to make it part of tidyverse verbs

#####

#### Purrr basic ####
# map 
# apply same function to every elements of an object
compute_s_N <- function(n){
  x <- 1:n
  return(sum(x))
}
compute_s_n <- function(n){
  x <- 1:n
  return(tibble(sum = sum(x)))
}
n <- 1:25

s_n <- map(n, compute_s_N) # return as a list
s_n <- map_dbl(n, compute_s_N) # return as a value
s_n <- map_df(n, compute_s_n) # return as a tibble
# last part is useful in tidyverse
#####

#### Tidyverse Conditionals ####
# Case When
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE ~ "Zero")
# ~ 用于连接条件和分类名，TRUE指代最后分剩的那个
# used to define categorical variables based on existing variables
murders %>% 
  mutate(group = case_when(
    abb %in% c("MH", "NH", "VT", "MA", "RI", "CT") ~ "New England", 
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    T ~ "Others")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5)

# Between
t = 9
between(t, 3, 6)
# use to check whether a value falls in an interval 

#####


