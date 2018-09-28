rm(list = ls())

#setwd('D://Testing Data/')

library(dplyr)
library(readr)

rank_data = read_csv('onpage_data_merged_test.csv')   
rank_data = rank_data %>% select(keyword,url,rank_organic)

bd_data_concat = read_csv(file = 'bd_merged_test_sriram.csv')

rank_data = rank_data %>% left_join(bd_data_concat,by = 'url') 

write.csv(rank_data,'bd_rank_merged_test.csv',row.names = F)
