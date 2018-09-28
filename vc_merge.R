rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)

onpage_data = read_csv('onpage_data_merged_test.csv')

onpage_data = onpage_data %>% select(url,title,visible_content) %>% unique(.)
onpage_data = unite(onpage_data,col = title_vc,sep = ' ',remove = T,title,visible_content)
write.csv(onpage_data,'onpage_data_test_concat.csv',row.names = F)
