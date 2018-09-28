#Function to download historical stock prices
get_historical_price_yahoo = function(stock = 'NSE',index = 'NSE'){
    
    if(stock == 'NSE'){
        stock_name = '%5ENSEI'
    }else if(stock == 'BSE'){
        stock_name = '%5EBSESN'
    }else if(index == 'NSE'){
        stock_name = paste(stock,'.NS',sep = '')
    }else{
        stock_name = paste(stock,'.BO',sep = '')
    }
    
    #Get current date
    current_date = Sys.Date()
    
    #Extract year day and month from current date
    current_year = as.numeric(str_split(string = current_date,pattern = '-')[[1]][1])
    current_day = as.numeric(str_split(string = current_date,pattern = '-')[[1]][3])
    current_month = as.numeric(str_split(string = current_date,pattern = '-')[[1]][2])
    
    #Parameters
    destination_file = paste(stock_name,'_historical_data.csv',sep = '')
    url_to_download = paste('http://real-chart.finance.yahoo.com/table.csv?s=',stock_name,'&d=',current_month - 1,'&e=',current_day,'&f=',current_year,'&g=d&a=0&b=1&c=1970&ignore=.csv',sep = '')
    
    #Download csv from URL file
    download.file(url = url_to_download,quiet = T,destfile = destination_file,mode = 'w')
    
    #Read and return data from file
    stock_historical_data = read_csv(file = destination_file)
    return(stock_historical_data)
}
