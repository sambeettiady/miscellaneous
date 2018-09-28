#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

#setwd("C:\\Users\\10494\\Desktop\\Data")
setwd("~/Desktop/transfer/Data")

#Load required packages
library(dplyr)
library(ggplot2)
library(party)

#Read overall data file
overall_data <- read.csv("overall_data.csv",header = T,as.is = T)

#Keep rank, ahrefs_rank & domain_rating and remove NA values
overall_data <- tbl_df(overall_data) %>% select(url,rank,ahrefs_rank,domain_rating) %>% mutate(y = ifelse(rank <= 10,1,0)) %>% 
                                         select(-rank) %>% replace(is.na(.),0) %>% as.data.frame()

summary(overall_data)

#Read backlink details file
backlink_details <- read.csv("backlink_details_after_rerun.csv",header = T,as.is = T)

backlink_details_summ <- tbl_df(backlink_details) %>% select(url,ahrefs_rank,domain_rating) %>% group_by(url) %>%
  summarise(mean_ar = mean(ahrefs_rank,na.rm = T),mean_dr = mean(domain_rating,na.rm = T),sd_ar = sd(ahrefs_rank,na.rm = T),sd_dr = sd(domain_rating,na.rm = T)) %>%
  replace(is.na(.),0) %>% as.data.frame()

rm(backlink_details)

overall_data <- left_join(overall_data,backlink_details_summ,by = "url") %>% replace(is.na(.),0)

#--------------------------------------********************Decision tree analysis***************----------------------------------

#Decision tree with ahrefs_rank
ahrefs_rank_tree <- ctree(y ~ ahrefs_rank,data = overall_data)
print(ahrefs_rank_tree)
plot(ahrefs_rank_tree,type = "simple")

#Decision tree with domain_rating
domain_rating_tree <- ctree(y ~ domain_rating,data = overall_data)
print(domain_rating_tree)
plot(domain_rating_tree,type = "simple")

#Decision tree with both domain_rating & ahrefs_rank
both_vars_tree <- ctree(y ~ ahrefs_rank + domain_rating,data = overall_data)
print(both_vars_tree)
plot(both_vars_tree,type = "simple")

#Decision tree with ahrefs_rank_mean
ahrefs_rank_tree_2 <- ctree(y ~ mean_ar,data = overall_data)
print(ahrefs_rank_tree_2)
plot(ahrefs_rank_tree_2,type = "simple")

#Decision tree with domain_rating_mean
domain_rating_tree_2 <- ctree(y ~ mean_dr,data = overall_data)
print(domain_rating_tree_2)
plot(domain_rating_tree_2,type = "simple")

#Decision tree with both domain_rating_mean & ahrefs_rank_mean
both_vars_tree_2 <- ctree(y ~ mean_ar + mean_dr,data = overall_data)
print(both_vars_tree_2)
plot(both_vars_tree_2,type = "simple")

#------------------------------------************Weight of evidence/Information value analysis***************---------------------

#Weight of Evidence and Information value
total.goods <- sum(overall_data$y)
total.bads <- sum(!overall_data$y)

cat_ahrefs_rank <- c(0,14,19,33,100)
cat_domain_rating <- c(0,49,63,100)

woe.data <- tbl_df(overall_data) %>% mutate(ar.class = cut(ahrefs_rank, breaks = cat_ahrefs_rank, labels = F),
                              dr.class = cut(domain_rating, breaks = cat_domain_rating, labels = F)) %>%
                              replace(is.na(.),0) %>% as.data.frame()

#ahrefs_rank woe
woe.agg.ar <- woe.data %>% group_by(ar.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
  mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
         woe = log(goods.per / bads.per), diff = goods.per - bads.per,
         iv = round(diff * woe * 100, 2))

#domain_rating woe
woe.agg.dr <- woe.data %>% group_by(dr.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
  mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
         woe = log(goods.per / bads.per), diff = goods.per - bads.per,
         iv = round(diff * woe * 100, 2))

#Plot Information Value vs buckets
plot(y = woe.agg.ar$iv,x = woe.agg.ar$ar.class)
plot(y = woe.agg.dr$iv,x = woe.agg.dr$dr.class)

#Plot Weight of evidence vs buckets
plot(y = woe.agg.ar$woe,x = woe.agg.ar$ar.class)
plot(y = woe.agg.dr$woe,x = woe.agg.dr$dr.class)

#Cut values
cat_ahrefs_rank_mean <- c(0,1,6,10,100)
cat_domain_rating_mean <- c(0,5,50,100)

#Woe for mean values
woe_mean_data <- tbl_df(overall_data) %>% mutate(ar.class = cut(mean_ar, breaks = cat_ahrefs_rank_mean, labels = F),
                                          dr.class = cut(mean_dr, breaks = cat_domain_rating_mean, labels = F)) %>%
                                          replace(is.na(.),0) %>% as.data.frame()

#ahrefs_rank_mean woe
woe.agg.ar.mean <- woe_mean_data %>% group_by(ar.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
  mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
         woe = log(goods.per / bads.per), diff = goods.per - bads.per,
         iv = round(diff * woe * 100, 2))

#domain_rating_mean woe
woe.agg.dr.mean <- woe_mean_data %>% group_by(dr.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
  mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
         woe = log(goods.per / bads.per), diff = goods.per - bads.per,
         iv = round(diff * woe * 100, 2))

#Plot Information Value vs buckets
plot(y = woe.agg.ar.mean$iv,x = woe.agg.ar.mean$ar.class)
plot(y = woe.agg.dr.mean$iv,x = woe.agg.dr.mean$dr.class)

#Plot Weight of evidence vs buckets
plot(y = woe.agg.ar.mean$woe,x = woe.agg.ar.mean$ar.class)
plot(y = woe.agg.dr.mean$woe,x = woe.agg.dr.mean$dr.class)
