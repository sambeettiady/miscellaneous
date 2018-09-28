rm(list = ls())

#Set Working Directory
setwd("C:\\Users\\10494\\Desktop\\Products\\ALPS\\final_files_10_08_2015\\Factor Analysis")

#Read dataset
pcr_data <- read.csv("pcr.csv",header = T,as.is = T) 

#Load packages
library(caret)
library(car)
library(dplyr)
library(ggplot2)

#Training & testing dataset - based on keywords
keywords <- unique(pcr_data$keyword)

set.seed(123) #for reproducible results


#keyword_sample <- 
intrain <- createDataPartition(y = pcr_data$keyword,p = 0.7,list = F)

training <- pcr_data[intrain,]
testing <- pcr_data[-intrain,]

#Classification of rank
training$y <- ifelse(training$rank <= 10,1,0)
testing$y <- ifelse(testing$rank <= 10,1,0)

#Initial logistic model
pclr <- glm(data = training[-c(1:3)],formula = y~.,family = "binomial")
summary(pclr)



