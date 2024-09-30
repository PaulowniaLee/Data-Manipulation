# a short note on regex

#### Escape and dot ####
# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)
#> \.

# And this tells R to look for an explicit .
str_extract(c("abc", "a.c", "bef"), "a\\.c")
#> [1] NA    "a.c" NA

# 总之，通常的\在R中需要写成\\

# useful function:
s <- "5'10\""
cat(s) # show what the string actually looks like inside R

#####



#### Useful Pairs ####
# \d: matches any digit. 
# \D, matches any character that is not a decimal digit.
# \w: any alphanumeric character
# \W: any non-alphanumeric character
str_extract_all("1 + 2 = 3", "\\d+")

# \s: matches any whitespace. 
# This includes tabs, newlines, form feeds, etc  
# \S, matches any non-whitespace character.
text <- "Some  \t badly\n\t\tspaced \f text"
str_replace_all(text, "\\s+", " ")

# [abc]: matches a, b, or c.
# [a-z]: matches every character between a and z 
# [^abc]: matches anything except a, b, or c.
# [\^\-]: matches ^ or -.

# Anchors 
# ^ and $ which represent the beginning and end of a string
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)
# look for an uppercase ([A-Z]) letter at the end of the string ($)

# Number of matchs
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)
# look for either 4 or 5 lowercase letters in a row anywhere in the string

# Alternation |
# | is the alternation operator, 
# which will pick between one or more possible matches.
str_detect(c("abc", "def", "ghi"), "abc|def")


# Exercise #
# regex to return TRUE for every instance of the number 19?
#"\\d" TRUE
#"[3-42]" FALSE 
#"[1-20]" TRUE
#"\\d{3}" FALSE
# note regex only recognise single digits

#####



#### Grouping ####
# You can use parentheses to override the default precedence rules:
str_extract(c("grey", "gray"), "gre|ay")
str_extract(c("grey", "gray"), "gr(e|a)y")

# Parenthesis also define “groups” that you can refer to 
# with backreferences, like \1, \2 etc, 
# and can be extracted with str_match()


# Anchors 
# ^ matches the start of string.
# $ matches the end of the string.

#####



#### Repetition ####
# ?: 0 or 1.
# +: 1 or more.
# *: 0 or more.

# By default these matches are “greedy”: 
# they will match the longest string possible. 
# You can make them “lazy”, 
# matching the shortest string possible by putting a ? after them:

# ??: 0 or 1, prefer 0.
# +?: 1 or more, match as few times as possible.
# *?: 0 or more, match as few times as possible.
# {n,}?: n or more, match as few times as possible.
# {n,m}?: between n and m, , match as few times as possible, at least n.

#####


# 中文
# 匹配中文字符的正则表达式： [\u4e00-\u9fa5]


# A useful exercise site for different regex:
# https://regexone.com/ 






