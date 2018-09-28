rm(list = ls())

#Set Working Directory
    setwd("C:\\Users\\10494\\Desktop\\Products\\ALPS\\final_files_10_08_2015\\Factor Analysis")

#Read dataset
    pcr_data <- read.csv("component_dataset.csv",header = T,as.is = T) 

#Load packages
    library(caret)
    library(car)
    
#Training & testing dataset
    set.seed(123) #for reproducible results
    intrain <- createDataPartition(y = pcr_data$rank,p = 0.7,list = F)

    training <- pcr_data[intrain,]
    testing <- pcr_data[-intrain,]

#Classification of rank
    training$y <- ifelse(training$rank <= 10,1,0)
    testing$y <- ifelse(testing$rank <= 10,1,0)
    
#Initial logistic model
    pclr <- glm(data = training[-c(1:3)],formula = y~.,family = "binomial")
    summary(pclr)
    
#Second iteration for model
    pclr2 <- glm(data = training[-c(1:3)],formula = y~.-PC6-ahrefs_rank,family = "binomial")
    summary(pclr2)
    
#Third iteration for model
    pclr3 <- glm(data = training[-c(1:3)],formula = y~.-PC6-ahrefs_rank-ln_domain_to_ln_link,family = "binomial")
    summary(pclr3)
    
    vif(pclr3)
    
#4th iteration for model
    pclr4 <- glm(data = training[-c(1:3)],formula = y~.-PC6-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    summary(pclr4)
    
    vif(pclr4)
    anova(pclr,pclr2,pclr3,pclr4)
#Removing observations with cook's distance greater than 4/(n-k-1)
    cutoff <- 4/(nrow(training)-18-1) 
    a <- cooks.distance(pclr4) <= cutoff
    
#Final model with outlier's removed
    pclr5 <- glm(data = training[a,-c(1:3)],formula = y~.-PC6-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    summary(pclr5)
    
    vif(pclr5)
    #good
    pclr6 <- glm(data = training[a,-c(1:3)],formula = y~.-PC8-PC6-fbshare_ln_ln-linked_root_domains-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    
    anova(pclr,pclr2,pclr3,pclr4,pclr5)
    
    pclr7 <- glm(data = training[a,-c(1:3)],formula = y~.-PC1-PC5-PC8-PC6-fbshare_ln_ln-linked_root_domains-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    pclr8 <- glm(data = training[a,-c(1:3)],formula = y~.-PC3-PC8-PC6-fbshare_ln_ln-linked_root_domains-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    #good
    pclr9 <- glm(data = training[a,-c(1:3)],formula = y~.-PC2-PC8-PC6-fbshare_ln_ln-linked_root_domains-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    
    pclr10 <- glm(data = training[a,-c(1:3)],formula = y~.-PC8-PC12-PC13-PC6-fbshare_ln_ln-linked_root_domains-ahrefs_rank-PC10-PC15-PC16-PC17-PC18-PC19-PC20,family = "binomial")
    
#Prediction on testing dataset    
    #concordance on training = 68.8%
    test.pred <- predict(object = pclr6,newdata = testing[-c(1:3)],type = "response")
    
    pred.test <- data.frame(y = testing$y,fitted.values = test.pred)
    #concordance on testing = 63.84%
    