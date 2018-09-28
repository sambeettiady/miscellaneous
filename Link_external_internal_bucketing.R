#Remove all previous objects
rm(list = ls())

#----------------************************Set your working directory here******************************---------------------------

#setwd("C:\\Users\\10494\\Desktop\\Data")
setwd("~/Desktop/transfer/Data")

#Load required packages
library(dplyr)
library(ggplot2)
library(party)

#Read overall dataset
overall_data <- read.csv("overall_data.csv",header = T,as.is = T)

#Keep rank, links_external & links_internal and remove NA values
overall_data <- tbl_df(overall_data) %>% select(url,rank) %>% mutate(y = ifelse(rank <= 10,1,0)) %>% 
    select(-rank) %>% replace(is.na(.),0) %>% as.data.frame()

summary(overall_data)

#Read backlink details file
backlink_details <- read.csv("backlink_details_non_text.csv",header = T,as.is = T)

backlink_details_summ <- tbl_df(backlink_details) %>% select(url,links_external,links_internal,page_size) %>% group_by(url) %>%
    summarise(mean_le = mean(links_external,na.rm = T),mean_li = mean(links_internal,na.rm = T),sd_le = sd(links_external,na.rm = T),sd_li = sd(links_internal,na.rm = T),mean_ps = mean(page_size,na.rm = T),sd_ps = sd(page_size,na.rm = T)) %>%
    replace(is.na(.),0) %>% as.data.frame()

rm(backlink_details)

overall_data <- left_join(overall_data,backlink_details_summ,by = "url") %>% replace(is.na(.),0)

#--------------------------------------********************Decision tree analysis***************----------------------------------

#Decision tree with links_external
links_external_tree <- ctree(y ~ mean_le,data = overall_data)
print(links_external_tree)
plot(links_external_tree,type = "simple")

#Decision tree with links_internal
links_internal_tree <- ctree(y ~ mean_li,data = overall_data)
print(links_internal_tree)
plot(links_internal_tree,type = "simple")

#Decision tree with page_size
page_size_tree <- ctree(y ~ mean_ps,data = overall_data)
print(page_size_tree)
plot(page_size_tree,type = "simple")

#------------------------------------************Weight of evidence/Information value analysis***************---------------------

#Weight of Evidence and Information value
total.goods <- sum(overall_data$y)
total.bads <- sum(!overall_data$y)

cat_links_external <- c(0,3,max(overall_data$mean_le))
cat_links_internal <- c(0,18,max(overall_data$mean_li))
cat_page_size <- c(0,3404,max(overall_data$mean_ps))

woe.data <- tbl_df(overall_data) %>% mutate(le.class = cut(mean_le, breaks = cat_links_external, labels = F),
                                     li.class = cut(mean_li, breaks = cat_links_internal, labels = F),
                                     ps.class = cut(mean_ps, breaks = cat_page_size, labels = F)) %>%
                                     replace(is.na(.),0) %>% as.data.frame()

#links_external woe
woe.agg.le <- woe.data %>% group_by(le.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
                           mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
                           woe = log(goods.per / bads.per), diff = goods.per - bads.per,
                           iv = round(diff * woe * 100, 2))

#links_internal woe
woe.agg.li <- woe.data %>% group_by(li.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
                           mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
                           woe = log(goods.per / bads.per), diff = goods.per - bads.per,
                           iv = round(diff * woe * 100, 2))

#page_size woe
woe.agg.ps <- woe.data %>% group_by(ps.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
    mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
           woe = log(goods.per / bads.per), diff = goods.per - bads.per,
           iv = round(diff * woe * 100, 2))

#Plot Information Value vs buckets
plot(y = woe.agg.le$iv,x = woe.agg.le$le.class)
plot(y = woe.agg.li$iv,x = woe.agg.li$li.class)
plot(y = woe.agg.ps$iv,x = woe.agg.ps$ps.class)

#Plot Weight of evidence vs buckets
plot(y = woe.agg.le$woe,x = woe.agg.le$le.class)
plot(y = woe.agg.li$woe,x = woe.agg.li$li.class)
plot(y = woe.agg.ps$woe,x = woe.agg.ps$ps.class)
