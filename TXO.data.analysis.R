source("packages.you.need.R")
options(tibble.print_min = 10)
#==================================================================================
# T-T-Tung
#==================================================================================
# 資料預處理
#==================================================================================
TXO.raw.dat <- read_rds("TXO.raw.dat.RDS")
pryr::object_size(TXO.raw.dat)
glimpse(TXO.raw.dat)

# 2012-11-28 才開始有週選
final.settlement.price <- read_rds("final.settlement.price.RDS") %>%
  filter(product == "TXO") %>% 
  filter(date >= "2012-11-28") %>% 
  select(-product_cht) 


zz <- final.settlement.price %>% 
  filter(delivery.month %>% str_length == 6 | 
           str_sub(delivery.month, 7, 8) == "W2")%>% 
  group_by(year(date), month(date)) %>% 
  summarise(max = as.Date(date) %>% max , 
            min = as.Date(date) %>% min )


yy <- as.Date(NA)

for (i in seq_along(zz$max)) {
  xx = (zz$min[i] : zz$max[i]) %>% as.Date()
  yy = c(yy,xx)
}



DT::datatable(max.OI, caption = '', filter = 'top', class = 'cell-border stripe', 
              options = list(pageLength = 300, autoWidth = TRUE))

#==================================================================================
# 臺指選擇權最大未平倉壓力與支撐
# 週選結算當天的週合約，要被剃除!!!!
#==================================================================================
# 週選合約
week.max.OI <- TXO.raw.dat %>% 
  filter(date == "2020-09-01") %>% 
  filter((contract.month %>% str_length > 6) | 
           (date %in% yy & # str_sub(contract.month, 1, 6)
              str_c(str_sub(date, 1, 4),str_sub(date, 6, 7)) == parse_number(contract.month)) ) %>% 
  filter(!(str_c(date, contract.month) %in%  
             str_c(final.settlement.price$date, final.settlement.price$delivery.month))) %>% 
  group_by(date, call.put) %>% 
  filter(OI == max(OI)) %>% 
  select(date , contract.month, call.put, strike.price) %>% 
  spread(key = call.put, value = strike.price)

month.max.OI <- TXO.raw.dat %>% 
  as.tibble() %>% 
  filter( date >"2020-01-10", (contract.month %>% str_length == 6)) %>%
  group_by(date) %>% 
  mutate(nearby.month = first(unique(contract.month)), 
         deferred.month = nth(unique(contract.month), 2) ) %>% 
  mutate(ken = ifelse(date %in% zz$max,  deferred.month, nearby.month) ) %>% 
  filter(contract.month == ken) %>% 
  group_by(date, call.put) %>% 
  filter(OI == max(OI)) %>% 
  select(date , call.put, strike.price) %>% 
  spread(key = call.put, value = strike.price)

all.month.max.OI <- TXO.raw.dat %>% 
  filter(date > "2020-09-15") %>% 
  filter(!(str_c(date, contract.month) %in%  
             str_c(final.settlement.price$date, final.settlement.price$delivery.month))) %>% 
  group_by(date, call.put) %>% 
  filter(OI == max(OI)) %>% 
  select(date , call.put, strike.price) %>% 
  spread(key = call.put, value = strike.price)


# TXO.raw.dat %>% 
#   filter(put.value  <- x %>% 
#            as.tibble() %>% 
#            # filter(date == "2020-09-18") %>% 
#            mutate(value = settlement.price * OI) %>% 
#            filter(call.put == "put") %>% 
#            group_by(date) %>% 
#            summarise(put.value = value %>% sum())) %>% 
#   filter((contract.month %>% str_length == 6) ) 

# https://stackoverflow.com/questions/47928278/r-highcharter-polar-graph-having-conditional-colors
highchart() %>%
  hc_title(text = list("TXO")) %>%
  # hc_chart(polar = TRUE) %>% 
  hc_subtitle(text = list("Source: Source: TAIFEX Database")) %>%
  hc_yAxis_multiples(list(title = list(text = "Call / Put TXO"),
                          plotBands = list(
                            list(from = 700, to = 1000, color = "#DCDCDC",
                                 label = list(text = "This is a plotBand")))),
                     list(title = list(text = NULL), opposite = TRUE)) %>%
  hc_xAxis(categories = month.max.OI$date,polar = TRUE) %>%
  hc_add_series(month.max.OI$call, yAxis = 0, name = "call",dashStyle= 'ShortDash') %>%
  hc_add_series(month.max.OI$put, yAxis = 0, name = "put", type="",dashStyle= 'ShortDash') %>%
  hc_add_theme(hc_theme_smpl())




#==================================================================================
# 臺指選擇權Put/Call比
# 週選結算當天的週合約，要被剃除!!!!
#==================================================================================
PCR <- TXO.raw.dat %>% 
  as.tibble() %>% 
  filter(date > "2020-04-28") %>% 
  filter(!(str_c(date, contract.month) %in%  
           str_c(final.settlement.price$date, final.settlement.price$delivery.month))) %>% 
  # filter(date == "2020-09-18") %>% 
  mutate(value = settlement.price * OI) %>% 
  group_by(date, call.put) %>% 
  summarise(total.OI = sum(OI)) %>% 
  spread(key = call.put, value = total.OI) %>% 
  mutate(PCR = (put / call))


highchart() %>%
  hc_title(text = list("TXO")) %>%
  hc_subtitle(text = list("Source: Source: TAIFEX Database")) %>%
  hc_yAxis_multiples(list(title = list(text = "Call / Put TXO"),
                          plotBands = list(
                            list(from = 700, to = 1000, color = "#DCDCDC",
                                 label = list(text = "This is a plotBand")))),
                     list(title = list(text = NULL), opposite = TRUE)) %>%
  hc_xAxis(categories = PCR$date) %>%
  hc_add_series(PCR$PCR, yAxis = 0, name = "Call/Put Ratio", type="column") %>%
  hc_add_theme(hc_theme_smpl())


#==================================================================================
# 月選合約
#==================================================================================

date %in% zz$max

aa=TXO.raw.dat %>% 
  as.tibble() %>% 
  filter( date >"2019-01-01", (contract.month %>% str_length == 6)) %>%
  group_by(date) %>% 
  mutate(nearby.month = first(unique(contract.month)), 
         deferred.month = nth(unique(contract.month), 2) ) %>% 
  mutate(ken = ifelse(date %in% zz$max,  deferred.month, nearby.month) ) %>% 
  filter(contract.month == ken) 

DT::datatable(aa, caption = '', filter = 'top', class = 'cell-border stripe', 
              options = list(pageLength = 100, autoWidth = TRUE))


# CALL TXO 的日結算價* OI / Put TXO 的日結算價* OI 
# TXO結算當日 結算合約不帶入計算
# 肉眼看起來感覺是
# 高點常常伴隨著這個值到一個高峰後 盤整或下跌
# 雖然是必然的結果(因為高點代表CALL權利金*OI/PUT權利金*OI很高) 
# 但感覺很像是主力推高之後 這個值到一個高峰之後下來 指數就不漲

call.value <- aa %>% 
  as.tibble() %>% 
  # filter(date == "2020-09-18") %>% 
  mutate(value = settlement.price * OI) %>% 
  filter(call.put == "call") %>% 
  group_by(date) %>% 
  summarise(call.value = value %>% sum())

put.value  <- aa %>% 
  as.tibble() %>% 
  # filter(date == "2020-09-18") %>% 
  mutate(value = settlement.price * OI) %>% 
  filter(call.put == "put") %>% 
  group_by(date) %>% 
  summarise(put.value = value %>% sum())


call.put <- cbind(call.value,put.value$put.value) %>% 
  as.tibble()




TX1 %<>% 
  filter( date >"2019-01-01",date <"2020-09-19")
TX1$close


highchart() %>%
  hc_title(text = list("TXO")) %>%
  hc_subtitle(text = list("Source: Source: TAIFEX Database")) %>%
  hc_yAxis_multiples(list(title = list(text = "Call / Put TXO"),
                          plotBands = list(
                            list(from = 700, to = 1000, color = "#DCDCDC",
                                 label = list(text = "This is a plotBand")))),
                     list(title = list(text = NULL), opposite = TRUE)) %>%
  hc_xAxis(categories = call.put$date) %>%
  hc_add_series(TX1$close, yAxis = 1, name = "TX1") %>%
  hc_add_series(call.put$call.value/put.value$put.value, yAxis = 0, name = "Call/Put Ratio", type="area") %>%
  hc_add_theme(hc_theme_smpl())
