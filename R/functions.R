


# ROC ---------------------------------------------------------------------

sh_roc <-  function (p) {
  m <- TTR::ROC(p)
  names(m) <- "roc"
  m <- data.frame(Day = index( m),  m, row.names = NULL) %>%
    dplyr::mutate (sig_Cl = ifelse(roc>0,1,-1),
            sig_Cl1 = lead(sig_Cl,1) )
  m <- xts( m[,-1], order.by =  m[,1])
  return(m)
}

# RSI ---------------------------------------------------------------------

sh_rsi = function (p) {
  m=TTR::RSI (p)
  m$sig_rsi=ifelse(m$EMA >= 65 | m$EMA < 30, 1, 0)
  m=m[,-1]
  return(m)
}


# MACD --------------------------------------------------------------------

sh_macd = function (p) {
  m = TTR::MACD (p)
  m$sig_macd= ifelse ( m$macd > m$signal, 1,0 )
  m=m[,-(1:2)]
  return (m)
}


# Aroon -------------------------------------------------------------------


sh_aroon = function (p) {

  # Up (down) trends are indicated when the aroonUp(Dn) is between 70 and 100. Strong trends are
  # indicated when when the aroonUp(Dn) is above 70 while the aroonDn(Up) is below 30.
  m = TTR::aroon(p)

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

  m = TTR::TDI (p)
  m$sig_tdi_buy = ifelse ( m$tdi > 0 & m$di > 0, 1,0 )
  m$sig_tdi_sell = ifelse ( m$tdi > 0 & m$di < 0, 1,0 )
  m=m[,-(1:2)]
  return(m)
}



# TRIX --------------------------------------------------------------------

sh_trix = function (p){

  # Buy/sell signals are generated when the TRIX crosses above/below the signal line
  # and is also above/below zero.
  m=TTR::TRIX(p)

  m$sig_trix_buy = ifelse ( m$TRIX > m$signal & m$TRIX > 0, 1,0)
  m$sig_trix_sell = ifelse ( m$TRIX < m$signal & m$TRIX < 0, 1,0)
  m=m[,-(1:2)]
  return(m)
}


# KST ---------------------------------------------------------------------

sh_kst = function (p){

  # The KST indicates bullish/bearish momentum as it crosses above/below its moving average.
  # Because the KST tends to lead price action, look for trend confirmation in the price.
  m = TTR::KST (p)
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




