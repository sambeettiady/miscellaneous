#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

setwd("C:\\Users\\10494\\Desktop\\Data")

#Load necessary packages
library(dplyr)

#Code for merging all files except backlink details
    
    #Reading the rank data file
    rank_data <- read.csv("rank_50_150915.csv",header = T,as.is = T)
    
    #Reading the backlink summary file
    backlink_summary_data <- read.csv("backlink_summary_data.csv",header = T,as.is = T)
    
    #Reading the social files
    facebook_data <- read.csv("facebook_data.csv",header = T, as.is = T)
    linkedin_data <- read.csv("linkedin_data.csv",header = T, as.is = T)
    twitter_data <- read.csv("twitter_data.csv",header = T, as.is = T)
    
    #Reading onpage data
    #onpage_data <- read.csv("onpage.csv",header = T,as.is = T)
    
    #Dropping two unnecessary variables from linkedin dataset
    linkedin_data <- linkedin_data[-c(2:3)]
    
    #Changing names for twitter and linkedin dataset
    names(twitter_data) <- c("twitter_count","url")
    names(linkedin_data) <- c("linkedin_count","url")
    
    #Merging all the datasets except onpage data into overall dataset
    overall_data <- left_join(rank_data,backlink_summary_data,by = "url")
    overall_data <- left_join(overall_data,facebook_data, by = "url")
    overall_data <- left_join(overall_data,twitter_data, by = "url")
    overall_data <- left_join(overall_data,linkedin_data, by = "url")
    
    #Write overall dataset into a .csv file
    write.csv(overall_data,"overall_data.csv",row.names = F)
    