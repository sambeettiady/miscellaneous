#PCA new method
    rm(list = ls())

#Setting working directory
    setwd("C:\\Users\\10494\\Desktop\\Products\\ALPS\\final_files_10_08_2015\\Factor Analysis")
    
    #***Remember to z-score variables for keywords & create data partition by keywords***

#Loading necessary packages
    library(ggplot2)
    library(psych)
    library(caret)
    library(dplyr)
        
#Reading dataset
    pca_data <- read.csv("PCA_keyword_dataset.csv",header = T,as.is = T)

#Converting variables into percentages
    data <- tbl_df(pca_data) %>% select(-c(ln_domain_to_ln_link,fbshare_ln_ln,ln_backlinks,dofoll_pct_ln_var2)) %>%
                            mutate(image_pct = ifelse(backlinks != 0, 100*image/backlinks,0)) %>%
                            mutate(redirect_pct = ifelse(backlinks != 0, 100*redirect/backlinks,0)) %>%
                            mutate(dofollow_pct = ifelse(backlinks != 0, 100*dofollow/backlinks,0)) %>%
                            mutate(dom_to_link_ratio = ifelse(backlinks != 0, refdomains/backlinks,0)) %>%
                            select(-c(image,redirect,dofollow,link_to_dom_ratio)) %>% as.data.frame()

#Measuring Sample Adequacy & Bartlett's test of sphericity - Initial
    KMO(data[-c(1:3)])                                  # MSA = 0.68
    cortest.bartlett(cor(data[-c(1:3)],use = "complete.obs"),n = 19472)      # Significant (p = 0)
    
#Z-scoring variables @ a keyword level
    #Storing mean & sd values for each variable for each keyword
        data_mean <- tbl_df(data) %>% group_by(keyword) %>% select(-c(rank,url.x)) %>% summarise_each(funs(mean))
        #write.csv(data_mean,file = "PCA_var_means.csv")
        rm(data_mean)
        data_sd <- tbl_df(data) %>% group_by(keyword) %>% select(-c(rank,url.x)) %>% summarise_each(funs(sd))
        #write.csv(data_sd,file = "PCA_var_sd.csv")
        rm(data_sd)
    #Z-scoring
        data_z <- tbl_df(data) %>% group_by(keyword) %>% mutate_each(funs(scale),domain_rating:dom_to_link_ratio) %>% arrange(keyword,rank) %>% as.data.frame()
        #write.csv(data_z,"data_z.csv")
        rm(pca_data,data)
    #NA z-score values turned to zero - (NA/NaNs = 0)
        data_z[is.na(data_z)] <- 0
            
#MSA & Bartlett's test of sphericity
    KMO(data_z[-c(1:3)])                                                        # MSA = 0.82
    cortest.bartlett(cor(data_z[-c(1:3)],use = "complete.obs"),n = 19472)      # Significant (p = 0)
    write.csv(cor(data_z[-c(1,3)]),"corr.csv")
    
#Principal Component Analysis
    pcatest <- prcomp(x = data_z[-c(1:3)],retx = T,center = F,scale. = F)
    screeplot(pcatest,type = "line",npcs = 23)
    
#PCA results
    write.csv(pcatest$sdev,"pca$sdev.csv")
    write.csv(pcatest$rotation,"pca$rotation.csv")
    write.csv(pcatest$x,"pca$scores.csv")
    write.csv(cor(pcatest$x),"PCA cor.csv")