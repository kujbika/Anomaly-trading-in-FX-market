#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
######### SECTION4 - MOVING AVERAGE APPLICATION ##############
#########         © Marcell Kujbus              ##############
##############################################################

#based on Han et al(2013), I apply Moving Average trading rule to the 
#dynamics of the portfolio reallocation.
ChooseAnomaly <- function(anomaly = 'Carry'){
  #choose witch anomaly should be handled with MA
  rm(list=setdiff(ls(), "anomaly"))
  if (anomaly == 'Momentum') {source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
  }else if (anomaly == 'Carry'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/carry_trading.R")
  }else if (anomaly == 'Volatility'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/volatility_trading.R")
}
}
#I apply MA portfolio-wise as Han et al(2013) does.
MA_assigner <- function(lag = 10, f = 1, h = 1){
  trade <- do.call( paste0( 'Trade_', anomaly ), list( f, h ) )
  weights <- trade[[2]][seq(1, nrow(trade[[2]]), h * 21)]
  idx_winners <- order(weights[1,2:10])[7:9]
  for (i in 2:nrow(weights)) {
    idx_winners = idx_winners %>% 
                  rbind(order(weights[i, 2:10])[7:9])}
  idx_winners = data.table(idx_winners, freq = h *21) %>% tidyr::uncount(freq)
  Spots <- select_vars(names(trade[[1]]), starts_with('S', ignore.case = TRUE))
  for (i in Spots){
    column = paste('MA',i, sep = '.')
    trade[[1]] = trade[[1]] %>% mutate( !!column  := c(rep(0,lag-1), RcppRoll::roll_mean(trade[[1]][[i]], 
                                                             n = lag, align = 'right')))
  }
  return (list(trade, idx_winners))
}


MA_trade <- function(lag = 10, f = 1, h = 1){
  ma_list <- MA_assigner(lag, f, h)
  data.all <- ma_list[[1]][[1]][-(1:lag),]; weights <- ma_list[[1]][[2]][-(1:lag),] 
  idx_winners <- ma_list[[2]][-(1:lag),]
  Spots <- select_vars(names(data.all), starts_with('S', ignore.case = TRUE))
  for (i in 1:nrow(data.all)){
    ma <- data.all[i,((ncol(data.all)-8):ncol(data.all))]
    winners_ma <- sum(ma[as.matrix(idx_winners)[i,]], na.rm = T)
    columns_win = Spots[as.matrix(idx_winners)[i,]]
    winners_spot <- sum(data.all[i, columns_win], na.rm =T)
    if (winners_spot < winners_ma) weights[i,2:10] = 0
    }
  return (list(data.all, weights))
}

StrategyEvaluation_MA <- function(lag = 10, f = 1, h = 1){
  trade <- MA_trade(lag, f, h)
  logSpots <- c("Date", select_vars(names(trade[[1]]), starts_with('log', ignore.case = TRUE)))
  spotlogret <<- trade[[1]][,logSpots]
  intRates <- c("Date", select_vars(names(trade[[1]]), starts_with('int', ignore.case = TRUE)))
  intrate_diff <<- trade[[1]][,intRates]
  returns_each <- FxReturn( 1)
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1 ) )
  }
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  perannum <- sum( portfolio_return$dailyreturn, na.rm = T ) / nrow( portfolio_return ) * 252
  return (perannum)
}

StrategyEvaluationplot_MA <- function(lag = 10, f = 1, h = 1){
  trade <- MA_trade(lag, f, h)
  logSpots <- c("Date", select_vars(names(trade[[1]]), starts_with('log', ignore.case = TRUE)))
  spotlogret <<- trade[[1]][,logSpots]
  intRates <- c("Date", select_vars(names(trade[[1]]), starts_with('int', ignore.case = TRUE)))
  intrate_diff <<- trade[[1]][,intRates]
  returns_each <- FxReturn( 1)
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1 ) )
  }
  portfolio_return <- data.table( dailyreturn = 100 * cumsum(diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}
