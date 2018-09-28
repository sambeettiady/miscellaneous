#Remove previous objects
rm(list = ls())

#Set working directory
setwd('/Users/sambeet/Data Science/Stocks/')

#Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(bdscales)
library(stringr)
library(manipulate)
library(lubridate)
library(quantmod)
library(stockPortfolio)
source('get_historical_price_yahoo.R')

#Read ticker index
#ticker_index = read_csv(file = 'EQUITY_L.csv')

#List of stocks to track
index_to_track = 'NSE'
stocks_to_track = c('ICICIBANK','SBIN','BANKBARODA','RELIANCE','HDFC','LICHSGFIN','ITC','DABUR',
                    'HINDUNILVR','TATAMOTORS','MARUTI','ASHOKLEY','M&M','TCS','WIPRO','INFY')

picker_choices = c(index_to_track,stocks_to_track)

overall_data = get_historical_price_yahoo(stock = index_to_track)
overall_data$stock = index_to_track

for(i in 1:length(stocks_to_track)){
    
    print(paste('Fethcing Data For:',stocks_to_track[i]))
    temp_historical_data = get_historical_price_yahoo(stock = stocks_to_track[i],index = index_to_track)
    temp_historical_data$stock = stocks_to_track[i]
    overall_data = rbind(overall_data,temp_historical_data)
    rm(temp_historical_data)
}

names(overall_data) = tolower(str_replace_all(string = names(overall_data),pattern = ' ',
                                              replacement = '_'))
overall_data$volume = as.numeric(overall_data$volume)

manipulate(
    {   
        ma_200 = colMeans(overall_data[which((overall_data$stock == selected_stock) & (overall_data$date >= Sys.Date() - 200)),price],na.rm = T)
        ma_100 = colMeans(overall_data[which((overall_data$stock == selected_stock) & (overall_data$date >= Sys.Date() - 100)),price],na.rm = T)
        ma_50 = colMeans(overall_data[which((overall_data$stock == selected_stock) & (overall_data$date >= Sys.Date() - 50)),price],na.rm = T)
        
        if(time_period_last5yrs){
            plot_data = overall_data %>% filter(date >= Sys.Date() - dyears(5))
        }else if(time_period_last3yrs){
            plot_data = overall_data %>% filter(date >= Sys.Date() - dyears(3))
        }else if(time_period_last1yr){
            plot_data = overall_data %>% filter(date >= Sys.Date() - dyears(1))
        }else if(time_period_last6mon){
            plot_data = overall_data %>% filter(date >= Sys.Date() %m-% months(6))
        }else if(time_period_last3mon){
            plot_data = overall_data %>% filter(date >= Sys.Date() %m-% months(3))
        }else if(time_period_last1mon){
            plot_data = overall_data %>% filter(date >= Sys.Date() %m-% months(1))
        }else if(time_period_last2wks){
            plot_data = overall_data %>% filter(date >= Sys.Date() - dweeks(2))
        }else if(time_period_last1wk){
            plot_data = overall_data %>% filter(date >= Sys.Date() - dweeks(1))
        }else{
            plot_data = overall_data
        }
        plot_data = plot_data %>% filter(stock == selected_stock)
        min_x_label_value = min(plot_data$date)
        max_x_label_value = max(plot_data$date)
        y_label_value = as.numeric(plot_data[which(plot_data$date == max_x_label_value),price])
        g = ggplot(data = plot_data,mapping = aes_string(x = 'date',y = price)) + 
            geom_line(size = 1,col = 'darkorange1') + ggtitle(selected_stock) + theme_bw() + 
            geom_hline(yintercept = c(ma_200,ma_100,ma_50),col = c('red4','blue4','orange4'),
                       linetype = c('solid','dashed','dotted')) + 
            geom_text(data=data.frame(x = c(min_x_label_value,min_x_label_value,
                                            min_x_label_value,max_x_label_value),
                                      y = c(ma_200,ma_100,ma_50,y_label_value)), 
                      aes(x, y),label = c(paste('MA 200 days:',round(ma_200,0)),
                                          paste('MA 100 days:',round(ma_100,0)),
                                          paste('MA 50 days:',round(ma_50,0)),
                                          paste('CP:',round(y_label_value))),
                      vjust = 1) + scale_x
        g
    },
    selected_stock = picker(as.list(picker_choices)),
    price = picker(as.list(names(overall_data)[2:7])),
    time_period_overall = button('Overall'),
    time_period_last5yrs = button('Last 5 Years'),
    time_period_last3yrs = button('Last 3 Years'),
    time_period_last1yr = button('Last 1 Year'),
    time_period_last6mon = button('Last 6 Months'),
    time_period_last3mon = button('Last 3 Months'),
    time_period_last1mon = button('Last 1 Month'),
    time_period_last2wks = button('Last 2 Weeks'),
    time_period_last1wk = button('Last 1 Week')
)
