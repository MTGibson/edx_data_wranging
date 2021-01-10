### R Wrangling section 1
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)


#learning readr and readxl
getwd()
# see working directory
getwd()
# change your working directory
#setwd()
# set path to the location for raw data files in the dslabs package and list files
#path <- system.file("extdata", package="dslabs")
#list.files(path)
# generate a full path to a file
#filename <- "murders.csv"
#fullpath <- file.path(path, filename)
#fullpath
# copy file from dslabs package to your working directory
#file.copy(fullpath, getwd())
# check if the file exists
#file.exists(filename)


# Example -- learning reading csvs
# inspect the first 3 lines
#read_lines("murders.csv", n_max = 3)

# read file in CSV format
#dat <- read_csv(filename)

#read using full path
#dat <- read_csv(fullpath)
#head(dat)

#Ex：
#path <- system.file("extdata", package = "dslabs")
#files <- list.files(path)
#files

#filename <- "murders.csv"
#filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
#filename2 <- "fertility-two-countries-example.csv"
#dat=read.csv(file.path(path, filename))
#dat1=read.csv(file.path(path, filename1))
#dat2=read.csv(file.path(path, filename2))


# Base R versions:

# filename is defined in the previous video
# read.csv converts strings to factors
#dat2 <- read.csv(filename)
#class(dat2$abb)
#class(dat2$region)


## Reading directly from the internet

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)