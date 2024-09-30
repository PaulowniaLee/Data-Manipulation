#### Environment ####
library(tidyverse)
library(dslabs)

# Part 1 Data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
remove(path, filename)

path <- system.file("extdata", package = "dslabs")
filename <- file.path(path,
"life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
remove(path, filename)

data("admissions")
dat <- admissions %>% select(-applicants)

# Part 2 Data 
data("murders")
head(murders)

data(polls_us_election_2016) 
head(polls_us_election_2016)
head(results_us_election_2016)

tab_1 <- slice(murders, 1:6) %>% 
  select(state, population)

tab_2 <- results_us_election_2016 %>%
  filter(state %in% 
           c("Alabama", "Alaska", "Arizona", 
             "California", "Connecticut", "Delaware")) %>%
  select(state, electoral_votes) %>% 
  rename(ev = electoral_votes)

install.packages("Lahman")
library(Lahman)
help("Lahman")

top <- Batting |> 
  filter(yearID == 2016) |>
  arrange(desc(HR)) |>
  slice(1:10)
top |> as_tibble()

People <- People |> as_tibble()
Salaries <- as_tibble(Salaries)
#####



#### Reshaping Data ####
# tidy vs. wide
{
# tidy data
# each row represents an observation 
# each column represents a different variable.

# wide data
# each row includes several observations 
# one of the variables is stored in the header.
}

{
# Pivot_longer: wide -> tidy
new_tidy_data <- wide_data %>% # first argument: original data frame
  pivot_longer(cols = "1960":"2015", # col: 要转换的列
               names_to = "year", # names_to：header中数据的新列名
               values_to = "fertility"# values_to: observation数据新列名
               )
class(new_tidy_data$year)
# pivot_longer assumes that column names are characters: 
# need to convert them into integer
mutate(new_tidy_data, year = as.integer(year))
# now can plot the graph
new_tidy_data %>% 
  ggplot(aes(year, fertility, colour = country)) + geom_point()


# Pivot_wider: wide -> tidy (as an intermediate step)
new_wide_data <- new_tidy_data %>%
  pivot_wider(names_from = year,
              values_from = fertility)
# 相当于逆转pivout_longer操作
select(new_wide_data, country, "1960": "1967")


# Separate_wider_delim
# 使用符号分割
dat <- raw_dat %>%
  pivot_longer(-country)

dat %>% separate_wider_delim(
  cols = name, # 要操作的列
  delim = "_", # 分割依据的符号
  names = c("year", "temp_name"), # new name for column created
  too_many = "merge" # merge together additional pieces
) %>%
  pivot_wider(
    names_from = "temp_name" 
  ) # 进一步整理表格


# Unite
# unite two columns into one 
var_names <- c("year", 
               "first_variable_name", "second_variable_name")
dat %>% 
  separate(name, var_names, fill = "right") %>%
  unite(name, first_variable_name, second_variable_name) %>%
  pivot_wider() %>%
  rename(fertility = fertility_NA) # change column name
}


# Exercises #
{
# 1. 
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>% 
  pivot_longer(
  cols = -year,
  names_to = "month",
  values_to = "co2"
) %>% 
  mutate(month = as.integer(month))

# 2.
co2_tidy %>%
  ggplot(aes(month, co2, color = year)) + geom_line()

# 3.
# CO2 increased year by year

# 4.
dat_tidy <- pivot_wider(
  dat, 
  names_from = gender,
  values_from = admitted
)

# 5-6 展示了一个很有用的重新组织表的过程
tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), 
               names_to = "key", 
               values_to = "value")
tmp
# 5. 
tmp2 <- unite(tmp,
              column_name,
              c(key, gender))

# 6. 
final <- pivot_wider(tmp2,
                     names_from = column_name,
                     values_from = value)
}
#####



#### Join Tables ####
# 这一部分和SQL很像

# Joins #
{
# Left Join 
left_join(tab_1, tab_2,
          by = "state")
# 与SQL同，向左“join”, 依据是左边那个表state一栏有的
# join一切，会产生NA

# We omitted Right Join for the same reason in SQL

# Inner Join 
# 同样是向左join，依据是state一栏两个表都有，不会产生NA
tab_1 %>%
  inner_join(tab_2,
             by = "state")

# Full Join 
# 同样是向左join，依据是state一栏中在任意一个表里出现过的，产生最多NA
tab_1 %>% 
  full_join(tab_2, 
            by = "state")

# Semi Join 
# 依据与inner join同，但不会有第二栏
# keep the part of first table for which 
# we have information in the second. 
# Do not add the columns of the second
tab_1 %>% 
  semi_join(tab_2,
            by = "state")

# Anti Join 
# 与semi join类似，但是保留没有信息的部分
tab_1 %>%
  anti_join(tab_2,
            by = "state")

}

# Binding # 
# simply combine data sets
{

# Bind by Columns
bind_cols(a = 1:3, # assign names to columns 
          b = 4:6)
# R-base function cbind does the same
# But cbind roduces different types, bind_col only produces data frame
test <- bind_cols(results_us_election_2016,
          murders) 
# can also bind data frames

# Bind by Rows
# 似乎同上， 略
}

# Set Operators
{
  # Intersect 
  # 找相似，可用于data frame
  dplyr::intersect(results_us_election_2016[1:5, ],
                   results_us_election_2016[3:7, ])
  
  # Union 
  # finds all rows in either x or y, excluding duplicates
  dplyr::union(results_us_election_2016[1:5, ],
               results_us_election_2016[3:7, ])
  # union_all(x, y) 
  # finds all rows in either x or y, including duplicates.

  # setdiff
  # finds all rows in x that aren't in y
  dplyr::setdiff(results_us_election_2016[1:5, ],
                 results_us_election_2016[3:7, ])
  # note: returns matched rows in first data frame
  # symdiff(x, y) 
  # computes the symmetric difference, 
  # i.e. all rows in x that aren't in y 
  # and all rows in y that aren't in x.
  
  # setequal 
  dplyr::setequal(results_us_election_2016[1:5, ],
                  results_us_election_2016[3:7, ])
  # 测试两个set是否一样
  
}

# Exercises #
{
# 1. 
top <- left_join(top %>% select(playerID, HR),
               People %>% select(playerID, nameFirst, nameLast),
               by = "playerID")
# Note here we only joined selected columns 
# 当然也可以先join再select
top <- top[, c(1, 3, 4, 2)]
# switch order of columns

# 2. 
top <- left_join(top,
               Salaries %>% 
                 filter(yearID == "2016") %>% 
                 select(salary, playerID),
               by = "playerID"
               )
# 这里其实也可以用right join拆开来写，
# 但我们只用left join 

# 2.5 
# set operators can make tedious comparison easy
AwardsPlayers <- as_tibble(AwardsPlayers)
award_2016 <- AwardsPlayers %>%
  filter(yearID == "2016") %>%
  select(playerID, awardID)

dplyr::intersect(award_2016$playerID,
                 top$playerID)
length(
  dplyr::setdiff(award_2016$playerID,
               top$playerID)
)
  
# 3. 
# use co2_tidy from Part 1
co2_tidy <- mutate(co2_tidy, co2 = as.integer(co2))
yearly_avg <- co2_tidy %>%
  group_by(year) %>%
  summarize(avg = mean(co2)) 

# 4.
co2_wide <- left_join(co2_wide,
                      yearly_avg,
                      by = "year") # add yearly_avg to wide data

for (i in c(1: nrow(co2_wide))) {
  for (j in c(1:12)){
    co2_wide[i, j] <- co2_wide[i, j] - co2_wide[i, 14]
  }
}
# 用了一个循环来求residuals，还是很快的

# 5.
co2_tidy <- co2_wide %>% 
  pivot_longer(
    cols = "1":"12",
    names_to = "month",
    values_to = "co2"
  ) %>% 
  mutate(month = as.integer(month),
         year = as.integer(year))

co2_tidy %>%
  ggplot(aes(month, co2, color = year)) + geom_point()

}

#####

