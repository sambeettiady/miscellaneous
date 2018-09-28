rm(list = ls())

library(readr)
library(dplyr)

x = read_csv('onpage_bd_merged.csv')

x = x %>% select(ahrefs_rank:nofollow) %>% unique(.)

write.csv(x,'bd_numerical_data.csv',row.names = F)