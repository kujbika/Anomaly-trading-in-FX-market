#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 3 - VOLATILITY ANOMALY    ##############
#########         � Marcell Kujbus              ##############
##############################################################

#in this subcode, I will show how to trade based on Volatility sorting dollar neutrally
#Keywords: Volatility sorting, Equal-weighted portfolio, Dynamic trading,
#currency return, FX market 

#Based on Han et al(2013), I form 3 portfolios according to the currencies historical volatility.
#The 'High' portfolio consists of the 3 currencies with the highest information uncertainty, whereas
# the 'low' portfolio is the combination of the 3 worst.
#A Volatilty investor goes long into the high, and goes short into the low portfolios.
#These portfolios are held for h = 1,3,6,9,12 months. The lookback time is f = 1,3,6,9,12 months
#1/3, 1/3, 1/3 weights will be assigned to the best 3 currencies
#whereas -1/3, -1/3, -1/3 will be assigned to the "losers".
source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")

Vol <- function(f, tseries){
  #Vol is a function that calculates f rolling standard deviation for Spot prices.
  #f denotes months, and I calculate w working days
  f = f * 21
  tseries = na.omit(tseries)
  tseries$SD = 0
  secunder <- function(idx) return ( sd( tseries[(idx - f) :idx, ][['Spot']] ) ) 
  for (i in f : nrow(tseries)) tseries[i, ][["SD"]] = secunder(i) 
  return (tseries)
}
TableMaker_Volatility <- function(f = 1, h = 1){
  #TableMaker_Volatility is a function that has two outputs as a list:
  #the first is all the data for currencies (Date, Spotprice, intrate differentials, st deviation) 
  #the second consisting of just the standard deviations.
  #Note: this function is a little bit slow
  workingtable <- Vol( f, SpotInterest( 1) )
  for (i in 2:9) workingtable = left_join( workingtable, Vol( f, SpotInterest( i) ), by = 'Date')
  sdev_idx <- c("Date", select_vars(names(workingtable), starts_with('SD', ignore.case = TRUE)))
  sdeviations <- workingtable[ -(1 : (f * 21)), sdev_idx ]
  
  return ( list( workingtable[ -(1 : (f * 21)), ], sdeviations ) )
}
Trade_Volatility <- function(f = 1, h = 1){
  #The Trade_Volatility() function has the base data table for currencies as an output,
  #and its second output is the weight allocation for all days.
  #f and h is measured in months
  workingtable <- TableMaker_Volatility(f, h)
  weights <- WeightAssigner( workingtable[[ 2 ]][ 1, ]) %>%
    slice( rep( row_number( ), (h * 21 ) ) )
  realloc <- seq(1 + h * 21, nrow(workingtable[[ 2 ]]), h * 21)
  for (i in realloc){
    newweight <- WeightAssigner( workingtable[[ 2 ]][ i, ] ) %>%
      slice( rep( row_number( ), h * 21) ) 
    weights = weights %>% rbind(newweight)
  }
  weights = workingtable[[2]][ , 1] %>% cbind(weights[ 1 : nrow( workingtable[[ 2 ]]), ])
  return ( list( workingtable[[ 1 ]], weights ) )
}
StrategyEvaluation_Volatility <- function(fh = c( 1, 1, TRUE, FALSE ) ){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]; h <- fh[2]; with_interest <- fh[3] 
  trade <- Trade_Volatility(f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  if (with_interest == FALSE)   returns <- select_vars(names(trade[[1]]), starts_with('log', ignore.case = TRUE))
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
StrategyEvaluation_plot_Volatility <- function(f = 1, h = 1){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  trade <- Trade_Volatility(f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = 100 * cumsum( diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) )  ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}

