rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)

bd_data = read_csv('bd_data.csv')
onpage_data = read_csv('onpage_data.csv')
onpage_data = onpage_data %>% select(keyword,url,rank_organic,title,visible_content)

onpage_data = onpage_data %>% left_join(y = bd_data,by = 'url')

print('File Merged!')

write.csv(onpage_data,'onpage_bd_merged.csv')
