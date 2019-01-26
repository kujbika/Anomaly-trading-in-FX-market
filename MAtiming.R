#based on Han et al(2013), I apply Moving Average trading rule to the long and short portfolio more sophisticatedly
ChooseAnomaly <- function(anomaly = 'Carry'){
  #choose witch anomaly should be handled with MA
  rm(list=ls())
  if (anomaly == 'Momentum') {source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
  }else if (anomaly == 'Carry'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/carry_trading.R")
  }else if (anomaly == 'Volatility'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/volatility_trading.R")
}
}
anomaly = "Carry"
ChooseAnomaly(anomaly)


#I apply MA portfolio-wise as Han et al(2013) does.
MA_assigner <- function(lag = 10, f = 1, h = 1){
  trade <- do.call( paste0( 'Trade_', anomaly ), list( f, h ) )
  weights <- trade[[2]][seq(1, nrow(trade[[2]]), h * 21)]
  idx_winners <- order(weights[1,2:10])[7:9]
  idx_losers <- order(weights[1,2:10])[1:3]
  for (i in 2:nrow(weights)) {
    idx_winners = idx_winners %>% 
                  rbind(order(weights[i, 2:10])[7:9])
    idx_losers = idx_losers %>% 
                  rbind(order(weights[i,2:10])[1:3])}
  idx_winners = data.table(idx_winners, freq = h *21) %>% tidyr::uncount(freq)
  idx_losers = data.table(idx_losers, freq = h *21) %>% tidyr::uncount(freq)
  Spots <- select_vars(names(trade[[1]]), starts_with('S', ignore.case = TRUE))
  for (i in Spots){
    column = paste('MA',i, sep = '.')
    trade[[1]] = trade[[1]] %>% mutate( !!column  := c(rep(0,lag-1), RcppRoll::roll_mean(trade[[1]][[i]], 
                                                             n = lag, align = 'right')))
  }
  return (list(trade, idx_winners, idx_losers))
}


MA_trade <- function(lag = 10, f = 1, h = 1){
  ma_list <- MA_assigner(lag, f, h)
  data.all <- ma_list[[1]][[1]][-(1:lag),]; weights <- ma_list[[1]][[2]][-(1:lag),] 
  idx_winners <- ma_list[[2]][-(1:lag),]; idx_losers <- ma_list[[3]][-(1:lag),]
  Spots <- select_vars(names(data.all), starts_with('S', ignore.case = TRUE))
  for (i in 1:nrow(data.all)){
    ma <- data.all[i,((ncol(data.all)-8):ncol(data.all))]
    winners_ma <- sum(ma[as.matrix(idx_winners)[i,]], na.rm = T); losers_ma <- sum(ma[as.matrix(idx_losers)[i,]], na.rm = T)
    columns_win = Spots[as.matrix(idx_winners)[i,]]; columns_lose <- Spots[as.matrix(idx_losers)[i,]]
    winners_spot <- sum(data.all[i, columns_win], na.rm =T); losers_spot <- sum(data.all[i, columns_lose], na.rm = T)
    if (winners_spot < winners_ma) weights[i, 1+as.matrix(idx_winners)[i,]] = 0
    if (losers_spot > losers_ma) weights[i, 1+as.matrix(idx_losers)[i,]] = 0
  }
  return (list(data.all, weights))
}
k = MA_trade()
