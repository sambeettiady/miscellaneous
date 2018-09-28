rm(list = ls())

#Set Working Directory
    setwd("C:\\Users\\10494\\Desktop\\Products\\ALPS\\final_files_10_08_2015\\Factor Analysis")

#Load necessary packages
    library(dplyr)
    library(nFactors)
    library(FactoMineR)
    library(psych)
    library(ggplot2)
    library(XLConnect)
    source(file = "C:\\Users\\10494\\Documents\\Summary_Statistics.R")
    library(caret)

#Read Data into R
    onpage_data <- read.csv("onpage_data_r.csv",header = T)
    social_plus_link_data <- read.csv("other_data_r.csv",header = T)

#Merging the datasets
    overall_data <- left_join(social_plus_link_data,onpage_data,by = c("keyword","rank","url.x"))

#Imputing Mean refdomains
    overall_data[which(overall_data$refdomains == 0 & overall_data$backlinks != 0),"refdomains"] <- mean(overall_data[-which(overall_data$refdomains == 0 & overall_data$backlinks != 0),"refdomains"])
    overall_data[which(overall_data$refclass_c == 0 & overall_data$backlinks != 0),"refclass_c"] <- mean(overall_data[-which(overall_data$refclass_c == 0 & overall_data$backlinks != 0),"refclass_c"])
    overall_data[which(overall_data$refips == 0 & overall_data$backlinks != 0),"refips"] <- mean(overall_data[-which(overall_data$refips == 0 & overall_data$backlinks != 0),"refips"])

#KMO criterion & Bartlett's Test of sphericity
    KMO(overall_data[-c(1:3)])                                      #Overall MSA = 0.79
    cortest.bartlett(R = cor(overall_data[-c(1:3)]), n = 19742)     #Significant (p = 0)

#Dropping variables with low individual MSA ( <0.5)
    overall_data_new <- overall_data[-c(5,11,15,25)]
    
#New KMO criterion & Bartlett's Test of Sphericity
    KMO(overall_data_new[-c(1:3)])                                      #Overall MSA = 0.81 / Each individual MSA > 0.5
    cortest.bartlett(R = cor(overall_data_new[-c(1:3)]), n = 19742)     #Significant (p = 0)
    scree(overall_data_new[-c(1:3)])
    
#Dropping other unqiue/unimportant variables
    overall_data_new2 <- overall_data_new[-c(4,5,7,9,10,13,15,18,19)]
    KMO(overall_data_new2[-c(1:3)])                                      #Overall MSA = 0.73 / Each individual MSA > 0.5
    cortest.bartlett(R = cor(overall_data_new2[-c(1:3)]), n = 19742)     #Significant (p = 0)
    
#Number of componenets to extract
    pcatest <- prcomp(x = overall_data_new2[-c(1:3)],retx = T,center = T,scale. = T)
    screeplot(pcatest,type = "line",npcs = 20)