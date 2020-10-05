source("packages.you.need.R")
source("zfun.R")

#==================================================================================
# T-T-Tung
#==================================================================================
# 期交所下載之資料的網址只有兩個地方不一樣，至作變數遠端控制變數
# "https://www.taifex.com.tw/cht/3/ -[dlFutDataDown]- "
# "?down_type=1&commodity_id= -[TX]- &commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F24"
#==================================================================================
taifex.commodity.type <- "Fut"
taifex.commodity <- "MTX"
#=======================找出有開盤的日期===========================================
# 下載期交所得資料最多只能載一個月，因此先用 quantmod 抓出大盤資料，選出有開盤的時間
# 再把每個月有開盤的第一天跟最後一天選出來，塞到下載連結裡面
# ----------------------------------------------------------------------------------
# # 期交所網站:
# "https://www.taifex.com.tw/cht/3/dlOptDataDown"
# # 下載動作：
# "?down_type=1&commodity_id=TXO&commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F18"
#==================================================================================
{
  # https://rdrr.io/cran/quantmod/man/getSymbols.html
twii_date <- quantmod::getSymbols("^TWII", auto.assign = FALSE,
                                  from = "2000-01-01", to=Sys.Date()) %>% time()
start_year <- 2000

for(year in start_year:( Sys.Date() %>% year()) ){

    for(month in sprintf('%02d', 1:12)){
      x = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% min()
      y = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% max()

        if(year  == start_year & month == sprintf('%02d', 1)){
          min.date = x
          max.date = y
        }else{
          min.date = c(min.date, x)
          max.date = c(max.date, y)
        }
    }
}

# 剔除 NA
# 轉換成期交所抓資料需要的日期格式
# magrittr::%<>%
min.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
max.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")

downlode.operation <- str_c("?down_type=1&commodity_id=",
                            taifex.commodity,
                            "&commodity_id2=&queryStartDate=",
                            min.date,"&queryEndDate=",max.date)
}

#==================================================================================
# 用 read.csv() 把資料爬進來
#==================================================================================
# n 是用來儲存第一筆下載的檔案開關，為了後面rbind(y,x)
y <- NULL
# 計算for 迴圈跑的時間
time.start <- Sys.time()

for(i in downlode.operation[1:20]){

  URL <- str_c("https://www.taifex.com.tw/cht/3/dl", taifex.commodity.type, "DataDown",i)
  # x <- read.csv(URL, sep = ",",header = T, stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5')
  x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5"))
      # rbind()
      y = rbind(y,x)

  time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
  cat("All Data are processed! Execution time: ", time.interval, "mins", "\n")
}

#==================================================================================
# 爬完資料後的後續處理
#==================================================================================
# 更改至正確的 column 名稱
colnames(y) <- c("date", "contract", "contract.month", "open", "high", "low", "close", "change",
                 "change.percent", "volume", "settlement.price", "OI", "Best.Bid",	"Best.Ask",
                 "historical.high",	"historical.low",	"Trading.Halt", "trading.session",
                 "Volume(executions among spread order and single order only)")

y$trading.session %<>% stringr::str_replace_all("一般", "regular")
y$trading.session %<>% stringr::str_replace_all("盤後", "after-hours")
# ============================= TX 資料預處理 ==================================== #
# 更改日期型態
y$date %<>% as.Date()
y$contract.month %<>% str_trim()

# 把資料改成 numeric
y[,4:12] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble()

#==================================================================================
# 資料儲存
#==================================================================================
write_rds(y, str_c(taifex.commodity,".raw.dat.RDS"))
