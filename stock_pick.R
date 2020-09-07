stock_pick = function(ind, vol=1e9, cap = 1e9, s_date, n_month =12, N = 2){
  
  # s_date = "2017-09-01"
  # ind = c('Technology')
  # vol = 1e6
  # cap = 1e9
  # N = 2
  # n_month = 12

  start_date = as.numeric(as.POSIXct(s_date, origin="1970-01-01"))
  end_date = as.numeric(as.POSIXct(AddMonths(s_date, n_month+1), origin="1970-01-01"))
  
  
  stockList = read.csv("https://public.opendatasoft.com/explore/dataset/nasdaq-companies/download/?format=csv&timezone=Europe/Berlin&lang=en&use_labels_for_header=true&csv_separator=%3B", 
                  sep = ";",
                  stringsAsFactors = FALSE)
  
  list1 = stockList[stockList$Sector %in% ind,]
  list2 = list1[list1$MarketCap >= cap, c('Symbol','Sector')]
  
  
  #calculate avg trading volume and growth from start_date to end_date of each stock in list 2
  
  symbol_f = c()
  sector_f = c()
  vol_avg_f= c()
  growth_f = c()
  
  i = 1
  
  for(s in list2$Symbol){
    # s = list2$Symbol[65]
    
    message(paste(i, '/', length(list2$Symbol),' ', s))
    data = get_data(s, start_date, end_date)
    i = i+1
    if (class(data) == 'character'){
      # print('aaa')
      next
      
    } else if(length(data$date) < n_month+1){
      # print('bbb')
      next
      
    } else {
      vol_avg = mean(data$vol[1:(length(data$date)-1)])
      if(vol_avg >= vol){
        symbol_f = c(symbol_f, s)
        sector_f = c(sector_f, list2$Sector[list2$Symbol == s])
        vol_avg_f = c(vol_avg_f, vol_avg)
        
        growth =  log(tail(data,1)[,'adj_close'] / data[1, 'adj_close'])
        growth_f= c(growth_f, growth)
        
      } else{
        # print('ccc')
        next
      }# end if
    }# end if
    
  } # end for loop
  
  list3 = data.frame(symbol_f,sector_f,vol_avg_f, growth_f, stringsAsFactors = FALSE)
  
  list4 = list3 %>% group_by(sector_f) %>% top_n(N, growth_f)
  
  return(list4)                  
} #end function stock_pick
