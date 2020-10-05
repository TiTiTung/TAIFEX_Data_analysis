# version: 2020-10-04

library(zoo)
library(ggplot2)
library(dplyr)

# 計算某事件區間n1到n2的累積報酬率 (扣除市場報酬)
CR <- function(mydat, eventdate,  n = 20, nowdate = Sys.Date(),
               startdate=as.Date("2005-01-01")){

  mydat <- mydat[index(mydat)  >= as.Date(startdate) &
                   index(mydat)  <= as.Date(nowdate) ]

  event <- mydat[index(mydat)  == as.Date(eventdate)] %>%
    as.numeric()

  event <- (mydat - event) / event

  CR = tibble(date = index(event) ,
              x = as.vector(coredata(event)) %>% as.vector())
  colnames(CR) = c("date", colnames(mydat)[1])


  CR.backward <- CR %>%
    filter(date < eventdate) %>%
    arrange(desc(date)) %>%
    .[1:n,]%>%
    arrange(date)

  CR.forward <- CR %>%
    filter(date >= eventdate) %>%
    .[1:(n+1),]


  CR <- rbind(CR.backward,CR.forward) %>%
    mutate(n = -n:n)


  return(CR)

}
