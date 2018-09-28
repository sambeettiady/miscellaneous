#Set Working Directory
    setwd("C:\\Users\\10494\\Desktop\\Products\\ALPS\\final_files_10_08_2015\\Factor Analysis")

#Read Data into R
    overall_data <- read.csv("model_data_modified_overall.csv",header = T)
    onpage_data <- read.csv("onpage_data_cleaned.csv",header = T)
    link_data <- read.csv("link_data_cleaned.csv",header = T)
    social_data <- read.csv("social_data_cleaned.csv",header = T)

#Load necessary packages
    library(dplyr)
    library(nFactors)
    library(FactoMineR)
    library(psych)
    library(ggplot2)
    library(XLConnect)
    source(file = "C:\\Users\\10494\\Documents\\Summary_Statistics.R")
    library(caret)
    
#Summary Statistics for each dataset
    summarystats(x = onpage_data,filename = "onpage_summary.xlsx")
    summarystats(x = link_data,filename = "link_summary.xlsx")
    summarystats(x = social_data,filename = "social_summary.xlsx")
    
#Correlation Matrix
    write.csv(cor(onpage_data[-c(1,3)]),file = "onpage_corr.csv")
    write.csv(cor(link_data[-c(1,3)]),file = "link_corr.csv")
    write.csv(cor(social_data[-c(1,3)]),file = "social_corr.csv")    
    write.csv(cor(overall_data[-c(1,3)]),file = "overall_corr.csv")    
    
#Dropping columns which make the matrix singular
    onpage_data <- onpage_data[-c(30,34)]       #Dropping image_alt_exact & image title exact
    link_data <- link_data[-c(6,24)]            #Dropping sitewide & nofollow
    
#KMO criterion for each dataset
    KMO(onpage_data[-c(1:3)])       #Overall MSA = 0.69
    KMO(link_data[-c(1:3)])         #Overall MSA = 0.81
    KMO(social_data[-c(1:3)])       #Overall MSA = 0.62 (Could be merged with link_data due to KMO on lower side)

#Bartlett's Test of sphericity
    cortest.bartlett(R = cor(onpage_data[-c(1:3)]),n = 19742)     #Significant (p = 0)
    cortest.bartlett(R = cor(link_data[-c(1:3)]),n = 19742)       #Significant (p = 0)
    cortest.bartlett(R = cor(social_data[-c(1:3)]), n = 19742)    #Significant (p = 0)
    
#Merging Social & Link data
    social_plus_link_data <- left_join(link_data,social_data,by = c("keyword","rank","url.x"))

#KMO criterion and Bartlett's Test of Sphericity for merged dataset
    KMO(social_plus_link_data[-c(1:3)])                       #Overall MSA = 0.81
    cortest.bartlett(R = cor(social_plus_link_data[-c(1:3)]), n = 19742)     #Significant (p = 0)
    
    