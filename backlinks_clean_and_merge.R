rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)

temp = read_csv('unique_urls_aet.csvoutput__backlink_details.csv')
names(temp) = tolower(names(temp))
temp = temp[which(temp$url != 'URL'),]
temp = temp %>% select(url,url_rating = ahrefs_rank,domain_rating:links_external,title,text_pre,anchor,text_post,nofollow) %>% 
        filter(nofollow == 'False') %>% select(-nofollow) %>% unique(.)
temp$anchor[which(is.na(temp$anchor))] = ''
temp$text_pre[which(is.na(temp$text_pre))] = ''
temp$title[which(is.na(temp$title))] = ''
temp$text_post[which(is.na(temp$text_post))] = ''
temp = unite(data = temp,col = title_ac,sep = ' ',remove = T,title,text_pre,anchor,text_post)

bd_data_numerical = temp %>% select(url,backlink_url,url_rating,domain_rating,links_internal,links_external)
write.csv(bd_data_numerical,'bd_data_numerical_aet.csv',row.names = F)

bd_data = temp %>% select(url,backlink_url,title_ac) %>% group_by(url) %>% summarise(backlink_text = paste(title_ac,collapse = ' '))
write.csv(bd_data,'bd_merged_aet.csv',row.names = F)

write.csv(temp,'bd_data_aet.csv',row.names = F)
