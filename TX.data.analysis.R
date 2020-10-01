source("packages.you.need.R")
#==================================================================================
# T-T-Tung
#==================================================================================
# 資料預處理
# 下載 TX 資料
TX <- read_rds("TX.raw.dat.RDS") %>% 
  select(date, contract, contract, contract.month, open, high, low, close, 
         settlement.price, change,  volume, OI, trading.session) %>%  
  filter(trading.session == "regular")  # 日盤資料


# 下載結算日期與結算價資料
final.settlement.price <- read_rds("final.settlement.price.RDS") %>% 
  filter(product == "TXO", delivery.month %>% str_length == 6 )

# ============================= 盤後 資料預處理 ==================================== #

TX.afterhours <- TX %>% 
  select(date, contract, contract, contract.month, open, high, low, close, 
         settlement.price, change,  volume, OI, trading.session) %>% 
  ## 盤後資料
  filter(trading.session == "regular")

# 把資料改成 numeric 
TX.afterhours[,4:11] %<>% 
  sapply(as.numeric) %>% 
  na.replace(0) %>% 
  as.tibble()

#==================================================================================
# 區分近遠月資料流程
#==================================================================================
# 1. 先用長度區分，只留月份合約 ("202009" vs "202009/202106")
# 2. 用group_by區分日期之後，不能直接用filter()，所以還要再創新column篩選 (mutate())
# 3. 再用mutate(first(),nth())算出同個人期的第一第二個合約，分別就是近月跟次近月合約
# 4. 用filter把contract.month是近月與次近月的合約留下來
# 5. 以防萬一，再創一個 nearby.deferred 來定義是近月(nearby)或次近月(deferred)
# ---------------------------------------------------------------------------------
# **由於是用順序來篩選近遠月合約，後續排序錯誤的跨可能出bug
#==================================================================================
TX.nearby.deferred <- TX %>% 
  filter(contract.month %>% str_length == 6 , date >= "2009-01-01" , date <= "2020-04-18") %>% 
  group_by(date) %>% 
  mutate(nearby.month = first(contract.month),deferred.month = nth(contract.month,2) ) %>% 
  filter(contract.month == nearby.month | contract.month == deferred.month) %>% 
  mutate(nearby.deferred = ifelse(contract.month == nearby.month, "nearby", "deferred") ) %>% 
  select(-nearby.month, -deferred.month) 


# ============================= 近月 資料預處理 ==================================== #
TX1 <- TX.nearby.deferred %>% 
  filter(nearby.deferred == "nearby") %<>% 
  mutate(Settlement = ifelse(date %in% (final.settlement.price$date ), 1, 0) )

# ============================= 次近 資料預處理 ==================================== #
TX2 <- TX.nearby.deferred %>% 
  filter(nearby.deferred == "deferred")%<>% 
  mutate(Settlement = ifelse(date %in% (final.settlement.price$date ), 1, 0) )


#==================================================================================
# 計算近遠月價差
#==================================================================================
# 建立空tibble  
data.diff = tibble::tribble(~date, ~TX1.contract, ~TX2.contract, ~Spread, ~Spread.Int ,~Settlement ,~T)

# 建立空tibble  
for(i in 2:(nrow(TX1))){
  data.diff[i, 1] = TX1$date[i]
  data.diff[i, 2] = TX1$close[i]
  data.diff[i, 3] = TX2$close[i]
    if(TX1$Settlement[i-1] ==1){
      data.diff[i, 5] = TX2$close[i] - TX1$close[i]
      data.diff[i, 4] = 0
    }
    if(TX1$Settlement[i-1] !=1){
      data.diff[i, 5] = data.diff[i-1,5]
      data.diff[i, 4] = TX2$close[i] - TX1$close[i] - data.diff[i, 5]
    }

      data.diff[i, 6] = TX1$Settlement[i]
}

data.diff$date %<>% as.Date()

data.diff %<>% 
  filter(!is.na(date)) %>%
  arrange(desc(date))


for(i in 2:nrow(data.diff)){
  if(data.diff$Settlement[i] == 1){
    data.diff$T[i] = 1
  }
  if(data.diff$Settlement[i] == 0){
    data.diff$T[i] = data.diff$T[i-1] + 1
  }
}

data.diff %<>% 
  filter(!is.na(Spread),!is.na(T)) %>%
  arrange(date)


#==================================================================================
# 價差統計表
#==================================================================================
year_filter <- 2020

TX.diff <- data.diff %>% 
  filter(year(date) == year_filter)

# 觀察價差變化可以發現，根據越靠近結算，其價差全距 越大，且25百分位
# 數的值負數越大，可從統計資訊得知， 越接近結算轉倉空單越不利。
TX.diff %>% 
  filter(T < 6)%>% 
  group_by(T) %>% 
  summarise(min = min(Spread),
            Qu_1st  = quantile(Spread, 0.25),
            median = median(Spread),
            mean = mean(Spread) %>% round(2),
            Qu_3st = quantile(Spread, 0.75),
            max = max(Spread)) %>% 
  arrange(desc(T)) %>% 
  DT::datatable( caption = 'Table 1: Spread'
                # , filter = 'top'
                )

#==================================================================================
# 繪圖
#==================================================================================
# 價差圖
plot(TX.diff$T, TX.diff$Spread, xlim = c(25,1),
     main = "?", xlab = "", ylab = "")
abline(h = 0, col = 'red', lwd = 2)
axis(1,25:1, 25:1, font.axis = 1)

plot(TX.diff$date, TX.diff$Spread, type='l',
     xlab = "", ylab = "", main = "")
abline(h = 0, col = 'red', lwd = 2)
# 基本boxplot裡面的參數只能是vector

boxplot(TX.diff$Spread[which(TX.diff$T==1)],
        TX.diff$Spread[which(TX.diff$T==2)],
        TX.diff$Spread[which(TX.diff$T==3)],
        TX.diff$Spread[which(TX.diff$T==4)],
        TX.diff$Spread[which(TX.diff$T==5)], 
        names = c("1","2","3","4","5") )


x <- TX.diff %>% filter(T < 6)

hcboxplot(x = x$Spread , var = x$T ) %>% 
  hc_chart(type = "column", color = "#2980b9")%>%
  hc_title(text = list(str_c("TX ", year_filter, "近遠月價差分佈"))) %>%
  hc_subtitle(text = list("Source: TAIFEX Database"))%>%
  hc_add_theme(hc_theme_google()) # to put box vertical) 


highchart() %>%
  hc_title(text = list(str_c("TX ", year_filter, "近遠月價差分佈圖"))) %>%
  hc_subtitle(text = list("Source: Source: TAIFEX Database")) %>%
  hc_xAxis(categories = TX.diff %>% arrange(desc(T)) %>% .$T) %>%
  hc_add_series(TX.diff$Spread, yAxis = 0, name = "TX_spread", type = "") %>%
  hc_add_theme(hc_theme_smpl())



highchart() %>%
  hc_title(text = list(str_c("TX ", year_filter, "近遠月價差分佈圖"))) %>%
  hc_subtitle(text = list("Source: Source: TAIFEX Database")) %>%
  hc_xAxis(categories = TX.diff$date) %>%
  hc_add_series(TX.diff$Spread, yAxis = 0, name = "TX_spread") %>%
  hc_add_theme(hc_theme_smpl())


DT::datatable(TX.nearby.deferred, 
              caption = 'Table 1: This is a simple caption for the table.', 
              filter = 'top',
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = list(list(extend = 'colvis', columns = c(1:8)))
              )
)%>% 
  formatStyle('change', fontWeight = styleInterval(100, c('normal', 'bold'))) %>%
  formatStyle(
    'change',
    color = styleInterval(c(-500, 500), c('white', 'blue', 'red'))
    # backgroundColor = styleInterval(-33, c('gray', 'yellow'))
  ) %>%
  formatStyle(
    'volume',
    background = styleColorBar(TX.nearby.deferred$volume, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )
