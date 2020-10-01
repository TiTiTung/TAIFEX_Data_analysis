source("packages.you.need.R")
#==================================================================================
# T-T-Tung
#==================================================================================
# 期交所下載之資料的網址只有兩個地方不一樣，至作變數遠端控制變數
# "https://www.taifex.com.tw/cht/3/ -[dlFutDataDown]- "
# "?down_type=1&commodity_id= -[TX]- &commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F24"
#==================================================================================
{
# ============================= 基本參數 ==================================== #
start_year <- 2002
y <- NULL
# 計算for 迴圈跑的時間
time.start <- Sys.time() 

#==================================================================================
# 用 read_csv() 把資料爬進來
#==================================================================================

for(year in start_year:( Sys.Date() %>% year()) ){
  # x <- read.csv(str_c("https://www.taifex.com.tw/cht/5/fSPDataDown?start_year=",
  #                       year, "&start_month=01&end_year=", year, "&end_month=12&dlFileType=3"), 
  #                 sep = ",",header = T, stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5') 
  URL <- str_c("https://www.taifex.com.tw/cht/5/fSPDataDown?start_year=",
               year, "&start_month=01&end_year=", year, "&end_month=12&dlFileType=3")
  
  x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5")) 
  
    # rbind() 
    y = rbind(y,x)
    
  time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
  cat("All Data are processed! Execution time: ", time.interval, "mins", "\n")

}

}
#==================================================================================
# 爬完資料後的後續處理
#==================================================================================
# 更改至正確的 column 名稱
colnames(y) <- c("date", "delivery.month", "product", "product_cht", "settlement.price")
y$date %<>% 
  as.Date()
y$delivery.month %<>% 
  str_trim()
y$settlement.price %<>%
  as.numeric() 

#==================================================================================
# 資料儲存
#==================================================================================
write_rds(y, "final.settlement.price.RDS")
