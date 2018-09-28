rm(list = ls())

library(readr)
library(dplyr)

bd_data = read_csv('bd_data_test.csv')

bd_data_numerical = bd_data %>% select(url,backlink_url,url_rating,domain_rating,links_internal,links_external)
write.csv(bd_data_numerical,'bd_data_numerical_test.csv',row.names = F)

bd_data = bd_data %>% select(url,backlink_url,title_ac) %>% group_by(url) %>% summarise(backlink_text = paste(title_ac,collapse = ' '))
write.csv(bd_data,'bd_merged_test_sriram.csv',row.names = F)