#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

setwd("C:\\Users\\10494\\Desktop\\Data")

#Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Read overall_data file
overall_data <- read.csv("overall_data.csv",header = T,as.is = T)

#Read 256 error urls file
error_urls <- read.csv("256_error_urls.csv",header = T,as.is = T)

error_data <- overall_data[which(overall_data$url %in% error_urls$url),]
error_data$y <- ifelse(error_data$rank <=10,1,0)
