#### Environment ####
install.packages("tidytext")
install.packages("textdata")
install.packages("broom")
install.packages("gutenbergr")

library(tidyverse) 
library(lubridate) 
library(scales)
library(dslabs)
library(tidytext)
library(textdata)
library(broom)
library(gutenbergr)

data("trump_tweets")
head(trump_tweets)

data("gutenberg_metadata")
head(gutenberg_metadata)
#####



#### Tweets Frequency Analysis ####

#note: extract data from tweet: rtweet package

# analysis needed: tidytext package

trump_tweets$text[16413] %>% 
  str_wrap(width = options()$width) %>% 
  cat # example tweet text
trump_tweets %>% 
  count(source) %>% 
  arrange(desc(n)) %>% 
  head(10) # source of tweets

campaign_tweets <- trump_tweets %>%
  mutate(source = str_remove(
    trump_tweets$source, pattern = "Twitter\\sfor\\s")) %>%
  filter(source %in% c("Android", "iPhone") & # only want phone tweets
           created_at >= ymd("2015-06-17") &
           created_at < ymd("2016-11-08")) %>% # tweets during campaign
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  # with_tz: get date_time in a different time zone
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>% # hourly tweets as a portion of total 
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

# We can see there is a pattern: two entities are using these two devices

#####



#### Text as Data ####

# this part convert free text into a tidy table

# unnest_tokens
# token refer to "unit" we consider to be a data point
# can be word, single character, sentence, or regex pattern

poem <- c("Roses are red,", 
          "Violets are blue,", 
          "Sugar is sweet,", 
          "And so are you.")
example <- tibble(line = c(1, 2, 3, 4), 
                  text = poem) %>% # arrange as tibble
  unnest_tokens(output = word,
                input = text,
                format = "text")
# Split a column into tokens, flattening the table into one-token-per-row.
# take data frame (tibble)
# default format is text
# default token is word
remove(example, poem)



# Now return to tweet case
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
# for tweets, include patterns that start with @ and #
i = 3008
campaign_tweets [i,] %>%
  unnest_tokens(output = word,
                input = text,
                token = "regex",
                pattern = pattern) %>%
  pull(word)
remove(i)
# also need remove links for picture at the end of tweets
tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(
    text, "https://t.co/[A-Za-z\\d]+|&amp;", "")
  ) %>%
  unnest_tokens(output = word,
                input = text,
                token = "regex",
                pattern = pattern) 



# Now can study word frequency 
# but meaningful analysis should first remove structural words
stop_words # table for such words
# also, we need to remove numbers, and remove ' at the start of words
tweet_words <- filter(tweet_words,
                      !word %in% stop_words$word &
                        !str_detect(word, "^\\d+$") &
                        !str_detect(word, "^#\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))
 
# total ranking        
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# can also study correlation between words and source
android_iphone_odd_ratio <- tweet_words %>%
  count(word, source) %>%
  pivot_wider(names_from = source,
              values_from = n,
              values_fill = 0 ) %>%
  mutate(odd_ratio = (Android + 0.5) / (sum(Android) - Android + 0.5) /
  ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5) )
  )
# top for Android 
android_iphone_odd_ratio %>%
  filter(Android + iPhone > 100) %>%
  arrange(desc(odd_ratio))
# top for iPhone
android_iphone_odd_ratio %>%
  filter(Android + iPhone > 100) %>%
  arrange(odd_ratio)

# There do seems to be some patterns

#####



#### Sentiment Analysis ####

# assign a word to one or more "sentiment"
# a quick approach to analyse large number of words

# package:
# tidytext
# textdata

# for tweets, we use nrc, one of prepared lexicon

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
# now can compare sentiments of tweets
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word", relationship = "many-to-many") %>%
  # set many-to-many since sentiments overlaps in some words
  count(source, sentiment) %>%
  pivot_wider(names_from = source, values_from = n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))

# So we can quantitatively study relationship of emotion and source
# odd ratio test
log_or <- sentiment_counts %>%
  mutate(log_or = log((Android / (sum(Android) - Android)) / 
                        (iPhone / (sum(iPhone) - iPhone))),
         se = sqrt(1/Android + 1/(sum(Android) - Android) + 
                     1/iPhone + 1/(sum(iPhone) - iPhone)),
         conf.low = log_or - qnorm(0.975)*se,
         conf.high = log_or + qnorm(0.975)*se) %>% 
  arrange(desc(log_or))
# visualise test result
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  #treats first argument as a categorical variable, 
  #reorders its levels based on the values of the second variable,
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

# disgust, anger, negative, sadness, and fear sentiments 
# are significantly associated with the Android source
# Words not associated to a sentiment 
# are significantly associated with the iPhone source


# Can further check which specific words are driving these differences
android_iphone_odd_ratio %>% 
  inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(odd_ratio))
# can also make graphs
android_iphone_odd_ratio %>%
  inner_join(nrc, by = "word") %>%
  mutate(log_or = log(odd_ratio)) %>%
  filter(Android + iPhone > 10 & abs(log_or) > 1) %>%
  # select significant words
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#####



#### Exercise: Scrap Book and Analyse ####
# locate id of desired book 
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice")) %>%
  select(gutenberg_id)

book <- gutenberg_works(title == "Pride and Prejudice",
                languages = "en") %>%
  # built in filter function of the database
  pull(gutenberg_id) %>%
  gutenberg_download()
  # built in download function 

words <- tibble(book) %>%
    unnest_tokens(output = word,
                  input = text,
                  format = "text",
                  to_lower = T)

# remove stop words and words containing one digit
words <- words %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "\\d+")) %>%
  mutate(word = str_replace(word, "^_", "")) %>%
  mutate(word = str_replace(word, "_$", "")) 

# count word frequency
nrow(words)
count(words, word) %>%
  filter(n > 100) %>%
  arrange(desc(n))

# study sentiment
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words, afinn, by = "word")
# total sentiment
nrow(afinn_sentiments)
# positive sentiment 
filter(afinn_sentiments, value > 0) %>%
  count()
# specific sentiment 
filter(afinn_sentiments, value == 4) %>%
  count()

#####






  





