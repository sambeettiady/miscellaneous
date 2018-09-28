#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

#setwd("C:\\Users\\10494\\Desktop\\Data")
setwd("~/Desktop/transfer/Data")

#Load required packages
library(dplyr)
library(ggplot2)

#Read backlink details files
backlink_details <- read.csv("backlink_details_non_text.csv",header = T,as.is = T) #Put name of backlink_details file here

#Drop other variables
backlink_details <- tbl_df(backlink_details) %>% select(url:domain_rating,links_external,links_internal,page_size) %>% as.data.frame()

#Summarise for all data
overall_summary <- tbl_df(backlink_details) %>% group_by(url) %>% replace(is.na(.),0) %>%
                   summarise(total_backlinks = length(url),ar_0_to_14 = sum(ifelse(ahrefs_rank <= 14,1,0)),
                             ar_15_to_19 = sum(ifelse(ahrefs_rank <= 19 & ahrefs_rank >= 15,1,0)),
                             ar_20_to_33 = sum(ifelse(ahrefs_rank <= 33 & ahrefs_rank >= 20,1,0)),
                             ar_34_to_100 = sum(ifelse(ahrefs_rank > 33,1,0)),ar_0_to_1 = sum(ifelse(ahrefs_rank <= 1,1,0)),
                             ar_2_to_6 = sum(ifelse(ahrefs_rank <= 6 & ahrefs_rank >= 2,1,0)),
                             ar_7_to_10 = sum(ifelse(ahrefs_rank <= 10 & ahrefs_rank >= 7,1,0)),
                             ar_11_to_100 = sum(ifelse(ahrefs_rank > 10,1,0)),dr_0_to_49 = sum(ifelse(domain_rating <= 49,1,0)),
                             dr_50_to_63 = sum(ifelse(domain_rating <= 63 & domain_rating >= 50,1,0)),
                             dr_64_to_100 = sum(ifelse(domain_rating > 63,1,0)),dr_0_to_5 = sum(ifelse(domain_rating <= 5,1,0)),
                             dr_6_to_50 = sum(ifelse(domain_rating <= 50 & domain_rating >= 6,1,0)),
                             dr_51_to_100 = sum(ifelse(domain_rating > 50,1,0)),
                             le_0_to_3 = sum(ifelse(links_external <= 3,1,0)),le_greater_than_3 = sum(ifelse(links_external > 3,1,0)),
                             li_0_to_18 = sum(ifelse(links_internal <= 18,1,0)),li_greater_than_18 = sum(ifelse(links_external > 18,1,0)),
                             ps_0_to_3404 = sum(ifelse(page_size <= 3404,1,0)),ps_greater_than_3404 = sum(ifelse(page_size > 3404,1,0)),
                             mean_ar = mean(ahrefs_rank,na.rm = T),sd_ar = sd(ahrefs_rank,na.rm = T),mean_dr = mean(domain_rating,na.rm = T),
                             sd_dr = sd(domain_rating,na.rm = T),mean_le = mean(links_external,na.rm = T),
                             sd_le = sd(links_external,na.rm = T),mean_li = mean(links_internal,na.rm = T),
                             sd_li = sd(links_internal,na.rm = T),mean_ps = mean(page_size,na.rm = T),
                             sd_ps = sd(page_size,na.rm = T)) %>% as.data.frame()

overall_summary[3:22] <- 100*overall_summary[3:22]/overall_summary$total_backlinks
overall_summary <- overall_summary %>% replace(is.na(.),0)

write.csv(overall_summary,"backlink_details_numerical_summary_final.csv",row.names = F)