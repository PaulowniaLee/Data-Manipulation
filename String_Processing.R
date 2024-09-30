#### Environment ####
install.packages("english")
install.packages("pdftools")

library(tidyverse)
library(stringr)
library(rvest)
library(dslabs)
library(english)
library(pdftools)

data("reported_heights")
head(reported_heights)

data("gapminder")
head(gapminder)

#####



# Steps for string processing:
# 1. detect
# 2. locate
# 3. extract
# 4. replace
# Most functions in stringr are included in base R, 
# but this package makes them more consistent



#### Case 1 US Murders ####
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))

# challenge 1: commas in character #
as.numeric(murders_raw$population[1:3]) # cannot simply convert to numeric

commas <- function(x) any(str_detect(x, ","))
# any(): return TRUE if at least one of the values in () is TRUE
# str_detect: return TRUE for each element of string that matches pattern
murders_raw %>% summarise_all((commas))
# summarise_all: apply one function to everything in table

murders_new <- murders_raw %>%
  mutate_at(2:3,
            parse_number
            )
# mutate_at: give precise location and (a tight) function wish to apply

#####   


# Lessons from Case 2 #
# A first stepis to survey the problematic entries and 
# try to define specific patterns followed by a large groups of entries.


#### Case 2 Self_reported heights ####
# We wish to convert height data to numbers

# but some data are not standard
reported_heights %>%
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n = 20)
# we see a pattern: x'y'', 
# which can be transformed into inch standard by 12x + y
# => we wish to convert other non-standard data to this format

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  return(ind)
}
# ind is a logical vector of TRUE and FALSE, 
# equal in length to the vector x (in the arguments list). 
# TRUE indicates that a height entry is incorrectly formatted.


# a function intends to find: non-inch height and impossible height
problems <- reported_heights %>%
  filter(not_inches(height)) %>% 
  pull(height)
# filter takes "Expressions that return a logical value" (any logical test)
length(problems)

# Further find some large groups of patterns
str_subset(
  problems,
  pattern = "^[4-7]'\\d{1,2}\"$"
) # find inch format data 
str_view(
  problems,
  pattern = "^[4-7]'\\d{1,2}\"$"
) # view locations of founded data
# str_subset 提出匹配的项,str_view显示匹配项的位置
# 使用正则表达式可以很好地定位模式



# Test: Replace with regex
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet,ft,foot with '
  str_replace("inches|in|''|\"","") %>% # remove all inches symbols
  str_detect("^[4-7]\\s*'\\s*\\d{1,2}$") %>% # allow space like 6 1
  # see how many standard data after replacement
  sum()

pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
# [4-7] = one digit, either 4, 5, 6, or 7
# \\s* = none or more white space (note in R we use \\ instead of \)
# [,\\.\\s+] = feet symbol is either , or . or at least one space
# \\d* = none or more digits
problems %>%
  str_subset(pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") 
# 转化用小数表英尺英寸的


# Convert original data
converted <- problems %>%
  str_replace("feet|foot|ft", 
              "'") %>%
  str_replace("inches|in|''|\"",
              "") %>%
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", 
              "\\1'\\2")

desired_pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
# [4-7] = one digit, either 4, 5, 6, or 7
# \\s* = none or more white space (note in R we use \\ instead of \)
# ' inch symbol
# 后面那个括号是在讨论带小数点的inch
# \\d+ = one or more digits 
# \\.? = zero or one decimal digit (.)
# \\d* = none or more digits
index <- str_detect(converted, desired_pattern)
mean(index) # proportion of desired pattern in data
converted[!index] # exam remaining patterns


# Dealing with further problems
converted %>%
  str_replace("^([4-7])$", "\\1'0") %>% # replace 5 alike entry
  str_replace("^([56])'?$", "\\1'0") # replace 5' alike entry





# Finally summarise a final solution for inches 
convert_format <- function(s){
  s %>%         # \\s* remove possible spaces around words
    str_replace("\\s*(feet|foot|ft)\\s*", 
                "'") %>% # change feet English to '
    str_replace("\\s*(inches|in|''|\"|cm|and)\\s*",
                "") %>% # remove inch/cm symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", 
                "\\1'\\2") %>% # remove other feet symbols
    str_replace("^([56])'?$", "\\1'0") %>% # change 5 and 5' alike entry
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    # change meters using commas
    str_trim() # remove empty spaces at start/end of entry
}

words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for (i in 0:11){
    s <- str_replace_all(s, words(i), as.character(i))
  }
  # words function convert 1 to one
  return (s)
}

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){ 
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
  ((inches >= smallest & inches <= tallest) | 
     (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind 
}

converted <- problems %>%
  words_to_numbers() %>%
  convert_format()

remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# Till here, problems left are seemingly impossible to fix

#####



#### Summary codes for case 2 ####
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
  mutate(original = height,
         height = words_to_numbers(height) %>%
           convert_format()) %>%
  extract(height, c("feet", "inches"),
          regex = pattern,
          remove = F) %>%
  mutate_at(c("height", "feet", "inches"),
            as.numeric) %>%
  mutate(guess = 12 * feet + inches) %>%
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height, #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA) #unsolvable problems
    )) %>%
  mutate(height = ifelse(is.na(height) &
                           inches < 12 & 
                           between(guess, smallest, tallest),
                         guess, height
                         )) %>%
  select(-guess)

#####



#### Case 3 Extracting tables from a PDF ####

# get pdf 
temp_file <- tempfile()
url <- paste0(
  "https://www.pnas.org/action/downloadSupplement?",
  "doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf")
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# current result is a long string and includes each line on the page
raw_data_research_funding_rates <- txt[2]

tab <- str_split(raw_data_research_funding_rates, 
                 "\n")
tab <- tab[[1]][1:18]

tab[1:18]

# First Step: Work Out Table Titles
# note here we can already found lines with table titles
the_names_1 <- tab[3]
the_names_2 <- tab[5]

the_names_1 <- the_names_1 %>%
  str_trim() %>% #remove spaces
  str_replace_all(",\\s.", "") %>% #remove unit
  str_split("\\s{2,}", simplify = T)
# str_split: split a string into pieces (output as list) by certain regex

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = T)
tmp_names <- str_c(rep(the_names_1, each = 3),
                   the_names_2[-1],
                   sep = "_")
# str_c merges multiple string list into list single string
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
  


# Second Step: Put in Real Data
temp_research_funding_rates <- tab[7:16] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = T)
# remove useless words in data
temp_9 <- str_extract(temp_research_funding_rates[,9],
                      pattern = ".{4}"
)
temp_10 <- str_extract(temp_research_funding_rates[,10],
                       pattern = ".{4}"
)

new_research_funding_rates <- data.frame(temp_research_funding_rates)
new_research_funding_rates$X9 <- temp_9
new_research_funding_rates$X9 <- temp_10

new_research_funding_rates <- new_research_funding_rates %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number) %>%
  as_tibble()
# finally get a neat table

#####



#### Case 4 Recording names of categorical variables ####
# certain country name in the data is too long
gapminder %>% 
  filter(region=="Caribbean") %>% 
  mutate(country = 
           recode(country,
  `Antigua and Barbuda` = "Barbuda",
  `Dominican Republic` = "DR",
  `St. Vincent and the Grenadines` = "St. Vincent", 
  `Trinidad and Tobago` = "Trinidad")
  ) %>%
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()
  
# recode function is specially designed to change names

#####



#### Exercises: Extract table from graph ####

fn <- system.file("extdata", 
                  "RD-Mortality-Report_2015-18-180531.pdf", 
                  package="dslabs")
system2("open", args = fn) # this function directly open file in Rstudio

# get on hand with page 9 
txt <- pdf_text(fn)
s <- txt[9] %>%
  str_split("\n")
s <- s[[1]] %>%
  str_trim()

# get column name of table
header <- s[[3]] %>%
  str_split(pattern = "\\s{2,}")
header <- header[[1]]
month <- header[1]
header <- header[2:length(header)]

# remove useless rows
# header and tail
header_index <- str_which(s, pattern = "SEP")
tail_index <- str_which(s, pattern = "^\\w{5}")
s <- s[(header_index+1) : (tail_index-1)]
# rows with only one data
n <- str_count(s, pattern = "\\d+")
s <- s[-which(n == 1)]

# remove non-numeric entries, extra data, and convert into table
temp_table <- s %>%
  str_replace_all(pattern = "\\D",
                  replacement = " ") %>%
  str_split_fixed(pattern = "\\s{2,}",
                  n = 6)
table <- temp_table[, 1:5]

colnames(table) <- c("day", header)
dat <- cbind(month, table) %>%
  as_tibble() %>%
  mutate_at(-1, as.numeric)

ggplot(data = dat, aes(x = day)) +
  geom_line(aes(y = `2015`), color = "yellow") +
  geom_line(aes(y = `2016`), color = "red") +
  geom_line(aes(y = `2017`), color = "blue") 

#####



#### Practice ####
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Opinion_polling_for_the_United_Kingdom_European_", 
              "Union_membership_referendum&oldid=896735054"
)  
tab <- read_html(url) %>% 
  html_nodes("table")
polls <- tab[[6]] %>% 
  html_table(fill = TRUE)
remove(url, tab)

colnames(polls) <- c("dates", "remain", "leave", 
                     "undecided", "lead", "samplesize", 
                     "pollster", "poll_type", "notes")
n <- (str_detect(polls$remain, pattern = "%"))
polls <- polls[-which(n == FALSE), ] 
remove(n)
  
polls$undecided <- str_replace(
  polls$undecided,
  pattern = "N/A", replacement = "0%")
#####

  

