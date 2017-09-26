# library(tidyverse)
# library(xts)
# library(caret)
# library(PerformanceAnalytics)
# library(lubridate)
# library(ranger)
# library(TTR)
# library(DataCombine)
# library(ranger)
# library(varhandle)

# Technical Analysis calculators Function------------------------------------------

tecA = function (p) {

  names(p)="Close"
  p <- merge(
  p,
  ROC(p),
  RSI(p),
  MACD(p) ) #,
  # aroon(p), # aroon(p,n=20)
  # BBands(p),
  # CCI(p),
  # CMO(p),
  # DonchianChannel(p),
  # DPO(p),
  # GMMA(p),
  # KST(p),
  # PBands(p),
  # # rollSFM Analysis of Running/Rolling/Moving Windows -- read this.. very intereting
  #  # try making an index with all other contracts
  #
  #
  # # Moving Averages
  # SMA(p), #Simple moving average.
  # EMA(p), # Exponential moving average.
  # WMA(p), # Weighted moving average.
  # DEMA(p), # Double-exponential moving average.
  # ZLEMA(p), # Zero lag exponential moving average.
  # HMA(p), # Hull moving average.
  # ALMA(p), # Arnaud Legoux moving average.
  # stoch(p),  # Stochastic Oscillator / Stochastic Momentum Index
  # TDI(p),  # Trend Detection Index
  # TRIX(p), # Triple Smoothed Exponential Oscillator
  # VHF(p), # Vertical Horizontal Filter
  # WPR(p) # William’s %R
  # )

# Converting to Data frame and making the basic signals
  p <- data.frame(Day = index(p),
                  p,
                  row.names = NULL)
  p=p%>% filter(is.na(Close.1)==F)%>%
    arrange(Day)%>%slide(Var = "Close.1", slideBy = 1, NewVar = "sig_Cl1", reminder=F)
  p=p%>%mutate(sig_Cl=ifelse(Close.1>0,1,-1)%>%as.factor(),
               sig_Cl1=ifelse(sig_Cl1>0,1,-1)%>%as.factor())


## make the indicators here it self.. i.e. 1,0, or other factors..


# making the Signals for ROC

  return(p)
}


# Data prep EEXQB ---------------------------------------------------------

eexqb_data_prep = function (newdata) {
  load("power_coal_prices_all.RData")


  # Preparing the data ------------------------------------------------------




  cdate=NULL
  eexbq=NULL
  for (i in 1: length(list_eexbq) ){

    p=list_eexbq[[i]]
    #p=list_eexbq[[36]]
    p=p%>%select(Day,eexq_Close)%>%arrange(Day)%>%tail(200)

    # adding the newdata here
    if ( i == length(list_eexbq) ){
      p=bind_rows(p,
                  newdata %>%
                    select (Day, eexq_Close=eexq))

    }

    p <- xts(p[,-1], order.by = p[,1])
    p=tecA(p)
    # storing the last date
    if (i==1) {cdate= tail(p,1)%>%select(Day)
    cdate= cdate-90
    } else { cdate=cdate }

    p = p %>% filter(Day>cdate[1,1])

    if ( i != length(list_eexbq) ){
      p = na.omit(p)
    }


    eexbq=bind_rows(eexbq,p)

    cdate=tail(p,1)%>%select(Day)

    #print(i)
  }

  eexbq = eexbq %>% select(Day, eexbq_Cl=Close, eexbq_roc = Close.1, sig_Cl, sig_Cl1)


  # coal
  cdate=NULL
  api2q=NULL
  for (i in 1: length(list_api2q) ){

    p=list_api2q[[i]]
    p=p%>%select(Day,api2q_Close)%>%arrange(Day)%>%tail(200)
    # adding the new data here
    if ( i == length(list_api2q) ){
      p=bind_rows(p,
                  newdata %>%
                    select (Day, api2q_Close=coalq))

    }

    p <- xts(p[,-1], order.by = p[,1])
    p=tecA(p)

    if (i==1) {cdate= tail(p,1)%>%select(Day)
    cdate= cdate-90
    } else { cdate=cdate }

    p = p %>% filter(Day>cdate[1,1])

    if ( i != length(list_api2q) ){
      p = na.omit(p)
    }


    api2q=bind_rows(api2q,p)

    cdate=tail(p,1)%>%select(Day)

    #print(i)
  }

  api2q = api2q %>% select(Day,api2q_roc = Close.1, EMA, macd,signal)%>%
    mutate(st1 = ifelse(macd >= signal, 1, -1) %>% factor(),      # signal line cross
           st2 = ifelse(macd >= 0, 1, -1) %>%factor(),            # center line cross
           st3 = ifelse(EMA >= 65 | EMA < 30, 1, 0) %>% factor())

  p = dplyr::inner_join(eexbq, api2q, by= "Day")
  rm(api2q,eexbq)
  p=p%>%filter(year(Day)>2013)

  return(p)
}


# add_to_newdata  ---------------------------------------------------------

add_to_newdata = function (eex, coal, data) {
  data=data %>% dplyr::select(Day,eexq,coalq)
  data=bind_rows(data,
                 data.frame(Day=Sys.Date(), eexq=eex, coalq=coal ))
  return(data)
}



# ROC ---------------------------------------------------------------------

sh_roc = function (p) {
  m=ROC(p)
  names(m)="roc"
  m <- data.frame(Day = index( m),  m, row.names = NULL) %>%
    mutate (sig_Cl = ifelse(roc>0,1,-1),
            sig_Cl1 = lead(sig_Cl,1) )
  m <- xts( m[,-1], order.by =  m[,1])
  return(m)
}

# RSI ---------------------------------------------------------------------

sh_rsi = function (p) {
  m=RSI (p)
  m$sig_rsi=ifelse(m$EMA >= 65 | m$EMA < 30, 1, 0)
  m=m[,-1]
  return(m)
}


# MACD --------------------------------------------------------------------

sh_macd = function (p) {
  m = MACD (p)
  m$sig_macd= ifelse ( m$macd > m$signal, 1,0 )
  m=m[,-(1:2)]
  return (m)
}


# Aroon -------------------------------------------------------------------


sh_aroon = function (p) {

  # Up (down) trends are indicated when the aroonUp(Dn) is between 70 and 100. Strong trends are
  # indicated when when the aroonUp(Dn) is above 70 while the aroonDn(Up) is below 30.
  m = aroon(p)

  m$sig_aroon_trend_up = ifelse ( m$aroonUp> 70 & m$aroonUp<100, 1,0 )
  m$sig_aroon_trend_dn = ifelse ( m$aroonDn> 70 & m$aroonDn<100, 1,0 )
  m$sig_aroon_strong_up = ifelse ( m$aroonUp> 70 & m$aroonDn<30, 1,0 )
  m$sig_aroon_strong_dn = ifelse ( m$aroonDn> 70 & m$aroonUp<30, 1,0 )
  m=m[,-(1:3)]
  return (m)
}



# TDI ---------------------------------------------------------------------


sh_tdi = function (p) {

  #buy if the TDI and the direction indicator are positive, and
  #sell if the TDI is positive while the direction indicator is negative.

  m = TDI (p)
  m$sig_tdi_buy = ifelse ( m$tdi > 0 & m$di > 0, 1,0 )
  m$sig_tdi_sell = ifelse ( m$tdi > 0 & m$di < 0, 1,0 )
  m=m[,-(1:2)]
  return(m)
}



# TRIX --------------------------------------------------------------------

sh_trix = function (p){

  # Buy/sell signals are generated when the TRIX crosses above/below the signal line
  # and is also above/below zero.
  m=TRIX(p)

  m$sig_trix_buy = ifelse ( m$TRIX > m$signal & m$TRIX > 0, 1,0)
  m$sig_trix_sell = ifelse ( m$TRIX < m$signal & m$TRIX < 0, 1,0)
  m=m[,-(1:2)]
  return(m)
}


# KST ---------------------------------------------------------------------

sh_kst = function (p){

  # The KST indicates bullish/bearish momentum as it crosses above/below its moving average.
  # Because the KST tends to lead price action, look for trend confirmation in the price.
  m = KST (p)
  m$sig_kst = ifelse ( m$kst > m$signal, 1,0)
  m=m[,-(1:2)]
  return(m)
}


# Technical Analysis calculators Function2------------------------------------------

tecA2  <-  function (p, c_name) {

  names(p) <- "Close"
  # funcitns after here

  p = merge (p,
             sh_roc(p),
             sh_rsi(p),
             sh_macd(p),
             sh_aroon (p),
             sh_tdi (p)
             #sh_trix(p),
             #sh_kst(p)
  )

  p <- data.frame(Day = index(p),
                  p,
                  row.names = NULL)

  p[,-c(1:3)]=lapply(p[,-c(1:3)], as.factor)
  names(p)[-1]=paste0(c_name,"_",names(p)[-1])
  return(p)
}




# list_to_df --------------------------------------------------------------


list_to_df <- function(list_xyz, cname) {
  cdate <- NULL
  eexbq <- NULL
  for (i in 1: length(list_xyz) ){

    p <- list_xyz[[i]]
    #p <- list_xyz[[36]]
    p <- p[,1:2] %>%arrange(Day)#%>%tail(200)
    p <- xts(p[,-1], order.by = p[,1])
    # function call ----------------
    p <- tecA2(p, cname)
    # storing the last date
    if (i==1) {cdate <-  tail(p,1)%>%select(Day)
    cdate <-  cdate-90
    } else { cdate <- cdate }

    p  <-  p %>% filter(Day>cdate[1,1])
    if ( i != length(list_xyz) ){
      p  <-  na.omit(p)
    }

    eexbq <- bind_rows(eexbq,p)
    cdate <- tail(p,1)%>%select(Day)

    #print (i)
  }
  print (paste(cname, "<--Done"))
  return(eexbq)
}

