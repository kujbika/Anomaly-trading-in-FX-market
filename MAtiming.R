#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
######### SECTION5 - MOVING AVERAGE APPLICATION ##############
#########         © Marcell Kujbus              ##############
##############################################################

#based on Han et al(2013), I apply Moving Average trading rule to the 
#dynamics of the portfolio reallocation. I go long in the 'High' portfolio
#if the MA(lag = 10) rule portfolio-wise says so.
#If the current price is lower than the MA, no trade happens.
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
    trade[[1]] = trade[[1]] %>% mutate( !!column  := c(rep(NA,lag-1), RcppRoll::roll_mean(trade[[1]][[i]], 
                                                             n = lag, align = 'right')))
  }
  return (list(trade, idx_winners))
}

MA_trade <- function(lag = 10, f = 1, h = 1){
  ma_list <- MA_assigner( lag, f, h )
  data.all <- ma_list[[1]][[1]][ -(1 : lag ), ]; weights <- ma_list[[1]][[2]][ -( 1 : lag ), ] 
  idx_winners <- ma_list[[2]][ -( 1 :lag), ]
  Spots <- select_vars( names( data.all ), starts_with('S', ignore.case = TRUE ) )
  for (i in 1:nrow(data.all)){
    ma <- data.all[ i, ( ( ncol( data.all ) - 8 ) : ncol( data.all ) ) ]
    winners_ma <- sum( ma[ as.matrix( idx_winners )[ i, ] ], na.rm = T )
    columns_win = Spots[ as.matrix( idx_winners )[ i, ] ]
    winners_spot <- sum( data.all[ i, columns_win ], na.rm = T )
    if (winners_spot < winners_ma) weights[ i, 2 : 10] = 0
    }
  return (list(data.all, weights))
}

StrategyEvaluation_MA <- function(lfh = c(10, 1, 1, TRUE)){
  lag = lfh[1]
  f = lfh[2]
  h = lfh[3]
  with_interest = lfh[4]
  trade <- MA_trade(lag, f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  if (with_interest == FALSE) returns <- select_vars(names(trade[[1]]), starts_with('log', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  perannum <- sum( portfolio_return$dailyreturn, na.rm = T ) / nrow( portfolio_return ) * 252
  sharpe <- PerformanceAnalytics::SharpeRatio.annualized(portfolio_return)
  model <- lm(portfolio_return$dailyreturn ~ 1)
  p = as.numeric(lmtest::coeftest(model, vcov = sandwich::NeweyWest(model, verbose = T))[1,4])
  result <- paste0("p.a=",round(100 * perannum, 2),',sh=',round(sharpe,2),',p=',round(p,3))
  return (result)
}
StrategyEvaluationplot_MA <- function(lag = 10, f = 1, h = 1){
  trade <- MA_trade(lag, f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = 100 * cumsum (diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) )  )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}
