#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

setwd("C:\\Users\\10494\\Desktop\\Data")

#Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Read overall dataset
overall_data <- read.csv("overall_data.csv",header = T,as.is = T)

#Remove unnecessary variables
overall_data <- overall_data %>% tbl_df() %>% select(-c(refpages,alternate,rss,click_count,commentsbox_count)) %>%
                                 as.data.frame()

#Impute zero instead of NA's
overall_data[is.na(overall_data)] <- 0

#Remove cases where backlinks !=0 & refdomains = 0
overall_data <- overall_data %>% tbl_df() %>% filter(!(backlinks != 0 & refdomains == 0)) %>% as.data.frame()

#Variable transformations - percentage,log,binary & ratio
overall_data <- overall_data %>% tbl_df() %>% select(-c(base_domain)) %>% mutate(text_pct = ifelse((text+image) != 0, (text/(text+image))*100,0),image_pct = ifelse((text+image) != 0, (image/(text+image))*100,0)) %>%
                                mutate(edu_pct = ifelse(backlinks != 0, (edu/backlinks)*100,0),redirect_pct = ifelse(backlinks != 0, (redirect/backlinks)*100,0)) %>%
                                mutate(canonical_pct = ifelse(backlinks != 0, (canonical/backlinks)*100,0),gov_pct = ifelse(backlinks != 0, (gov/backlinks)*100,0)) %>%
                                mutate(dofollow_pct = ifelse((dofollow+nofollow) != 0, (dofollow/(dofollow+nofollow))*100,0),nofollow_pct = ifelse((dofollow+nofollow) != 0, (nofollow/(dofollow+nofollow))*100,0)) %>%
                                mutate(not_sitewide_pct = ifelse((sitewide+not_sitewide) != 0, (not_sitewide/(sitewide+not_sitewide))*100,0),sitewide_pct = ifelse((sitewide+not_sitewide) != 0, (sitewide/(sitewide+not_sitewide))*100,0),backlinks_ln = log(backlinks + 1),fb_share_ln_ln = log(log(share_count + 1)+1)) %>%
                                mutate(html_binary = ifelse(html_pages == 0,0,1),pages_binary = ifelse(pages == 0,0,1),dom_to_link_ratio = ifelse(backlinks != 0,ifelse(refdomains/backlinks>1,1,refdomains/backlinks),0)) %>%
                                mutate(ln_dom_to_ln_backlinks = ifelse(backlinks != 0,ifelse(log(refdomains+1)/log(backlinks+1) > 1,1,log(refdomains+1)/log(backlinks+1)),0)) %>% as.data.frame()                                

#Z-scoring @ a keyword level
overall_data_z <- overall_data %>% tbl_df() %>% group_by(keyword) %>% mutate_each(funs(scale),ahrefs_rank:dom_to_link_ratio) %>% arrange(keyword,rank) %>% as.data.frame()
