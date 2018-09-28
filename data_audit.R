#Remove all previous objects
rm(list = ls())

#----------------************************Set you working directory here******************************---------------------------

setwd("C:\\Users\\10494\\Desktop\\Data")

#-----------------------------------*******Function for generating summary statistics report*******-----------------------------

generate_summary_statistics_report <- function(x = data.frame(),filename = "Summary_Statistics_report.xlsx"){
    library(XLConnect)
    #library(ggplot2)                                               #ggplot2 creates problems with histogram sometimes 
    wb <- loadWorkbook(filename,create = T)
    for(i in 1:ncol(x)){
        if(class(x[,i]) == "numeric" || class(x[,i]) == "integer"){
            
            df <- data.frame(var = x[,i])
            sheetname <- names(x[i])
            createSheet(wb,sheetname)
            
            summstats <- data.frame(stats = c("mean","std. dev","min","maximum","median","count_of_zeros","count_of_NAs","no._of_distinct_values","quantile_5%","quantile_25%","quantile_50%","quantile_75%","quantile_95%"),values = c(mean(df$var,na.rm = T),sd(df$var,na.rm = T),min(df$var,na.rm = T),max(df$var,na.rm = T),median(df$var,na.rm = T),sum(df[which(df$var == 0),"var"]),sum(is.na(df$var)),length(unique(df$var)),quantile(df$var,probs = 0.05,na.rm = T),quantile(df$var,probs = 0.25,na.rm = T),quantile(df$var,probs = 0.5,na.rm = T),quantile(df$var,probs = 0.75,na.rm = T),quantile(df$var,probs = 0.95,na.rm = T)))
            
            boxplot <- paste(sheetname,"!$F$1",sep = "")
            name1 <- paste("boxplot_",i,sep = "")
            createName(wb,name = name1,formula = boxplot)
            bpfilename <- paste("boxplot_",i,".png",sep = "")
            
            boxplot2 <- paste(sheetname,"!$V$1",sep = "")
            name3 <- paste("boxplot_",i,"_",i,sep = "")
            createName(wb,name = name3,formula = boxplot2)
            bpfilename2 <- paste("boxplot_",i,"_",i,".png",sep = "")
            
            png(bpfilename,width = 400, height = 400)
            #b <- ggplot(data = df,aes(y = var,x = factor(0))) + geom_boxplot()                     #for ggplot2
            boxplot(df$var,col = "steelblue2",main = "Boxplot with outliers")
            #print(b)                                                                               #for ggplot2
            dev.off()
            
            png(bpfilename2,width = 400, height = 400)
            boxplot(df$var,col = "steelblue3",outline = F,main = "Boxplot without outliers")
            dev.off()
            
            histogram <- paste(sheetname,"!$N$1",sep = "")
            name2 <- paste("histogram_",i,sep = "")
            createName(wb,name = name2,formula = histogram)
            histfilename <- paste("histogram_",i,".png",sep = "")
            
            histogram2 <- paste(sheetname,"!$AD$1",sep = "")
            name4 <- paste("histogram_",i,"_",i,sep = "")
            createName(wb,name = name4,formula = histogram2)
            histfilename2 <- paste("histogram_",i,"_",i,".png",sep = "")
            
            png(histfilename,width = 400, height = 400)
            #h <- ggplot(data = df,aes(x = var)) + geom_histogram(binwidth = 1,colour = "black", fill = "white") #for ggplot2
            hist(df$var,col = "snow2",breaks = 100,main = "Histogram with outliers")
            #print(h)                                                                                           #for ggplot2
            dev.off()
            
            png(histfilename2,width = 400, height = 400)
            hist(df$var[!df$var %in% boxplot.stats(df$var)$out],col = "snow3",breaks = 100,main = "Histogram without outliers")
            dev.off()
            
            writeWorksheet(object = wb,data = summstats,sheet = sheetname,startRow = 1,startCol = 1,rownames = F,header = F)
            
            addImage(wb, filename = bpfilename, name = name1,originalSize = TRUE)
            addImage(wb, filename = bpfilename2, name = name3,originalSize = TRUE)
            addImage(wb, filename = histfilename, name = name2,originalSize = TRUE)
            addImage(wb, filename = histfilename2, name = name4,originalSize = TRUE)
        }
    }
    saveWorkbook(wb)
}

#------------------------------------------***End of summary statistics function***---------------------------------------------



#----------------------------------------*******Function for generating QQ plots report*******----------------------------------

generate_qqplot_report <- function(x = data.frame(),filename = "QQPlot_report.xlsx"){
    library(XLConnect)
    library(ggplot2)
    
    wb <- loadWorkbook(filename,create = T)

    for(i in 1:ncol(x)){
        if(class(x[,i]) == "numeric" || class(x[,i]) == "integer"){
            df <- data.frame(var = x[,i])
            graphname <- paste("graph_",i,".png",sep = "")
            sheetname <- names(x[i])
            region <- paste(sheetname,"!$A$1",sep = "")
        
            createSheet(wb,sheetname)
            createName(wb,name = graphname,formula = region)
        
            png(filename = graphname,width = 400,height = 400)
            g <- ggplot(data = df, aes(sample = scale(var))) + geom_point(stat = "qq")
            print(g)
            dev.off()
        
            addImage(wb, filename = graphname, name = graphname, originalSize = TRUE)
        }
    }
    saveWorkbook(wb)
}

#------------------------------------------***End of QQplot function***--------------------------------------------------------






#----------------------------------------*******Function for backlinks data validation*******----------------------------------

backlink_validation <- function(backlink.summary.df = data.frame(),backlink.detail.df = data.frame()){
    
    out <- data.frame(matrix(, nrow = length(backlink.summary.df$url), ncol=3))
    names(out) <- c("URL","backlink_from_summary","backlink_from_details","difference")
    
    for(i in 1:length(backlink.summary.df$url)){
        
        out[i,1] <- backlink.summary.df$url[i]
        out[i,2] <- backlink.summary.df$backlinks[i]
        out[i,3] <- sum(match(x = backlink.detail.df$URL,table = out[i,1],nomatch = 0))
        out[i,1] <- out[i,2] - out[i,3]
    }
    
    write.csv(out,file = "data_validation.csv",row.names = F,col.names = T)
}

#------------------------------------------***End of data validation function***-----------------------------------------------
