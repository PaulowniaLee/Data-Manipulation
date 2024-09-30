# Environment #
library(tidyverse)
library(readr)
library(readxl)

# get path for a file in package
path<-system.file("extdata", package="dslabs")
list.files(path)

# create full path with filename and directory 
filename <-"murders.csv"
fullpath<-file.path(path, filename)
fullpath

# copy file to current directory and check whether file does exist
file.copy(fullpath, getwd()) 
# 第一个参是（源文件）当前位置，第二个参是目标位置
file.exists(filename)



# Download Data from Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
# can read file directly with various "read" function
dat <- read.csv(url)

# can also download file 
download.file(url, "murders.csv") # note second argument is the name 
                                  # it can be different 

# random name function:
tmp_filename <- tempfile() # tempfile generate a random unique name
download.file(url, tmp_filename) # give a random name for downloaded file
dat <- read.csv(tmp_filename)
file.remove(tmp_filename)



# Exercise
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
download.file(url, "breast")
dat <- read_csv(url, col_names = F) # since it does not have header
nrow(dat)
ncol(dat) # two useful functions to check number of rows and columns
