rm(list = ls())

library(readr)
library(dplyr)

onpage_data = read_csv('onpage_data_merged_test.csv')
onpage_data = onpage_data %>% select(keyword,url) %>% unique(.)

bd_data = read_csv('bd_data_test.csv')
bd_data = bd_data %>% select(url,backlink_url) %>% unique(.)

onpage_bd_merged = onpage_data %>% left_join(y = bd_data,by = 'url')

write.csv(onpage_bd_merged,'onpage_bd_combination_test.csv',row.names = F)