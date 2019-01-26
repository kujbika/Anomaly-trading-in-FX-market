source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")
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
TableMaker_Vol <- function(f = 1, h = 1){
  #TableMaker_Vol is a function that has two outputs as a list:
  #the first is all the data for currencies (Date, Spotprice, intrate differentials, st deviation) 
  #the second consisting of just the standard deviations.
  #Note: this function is a little bit slow
  workingtable <- Vol( f, SpotInterest( 1) )
  for (i in 2:9) workingtable = left_join( workingtable, Vol( f, SpotInterest( i) ), by = 'Date')
  sdeviations <- workingtable[ -(1 : (f * 21)), c( 1, seq( 5, 37, 4 ) ) ]
  
  return ( list( workingtable[ -(1 : (f * 21)), ], sdeviations ) )
}
Trade_Vol <- function(f = 1, h = 1){
  #The trade_vol() function has the base data table for currencies as an output,
  #and its second output is the weight allocation for all days.
  #f and h is measured in months
  workingtable <- TableMaker_Vol(f, h)
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
StrategyEvaluation_Vol <- function(fh = c( 1, 1, TRUE, FALSE ) ){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]
  h <- fh[2]
  with_interest <- fh[3] #boolean variable
  sharpe_bool <- fh[4] #boolean variable
  trade <- Trade_Vol(f, h)
  spotlogret <<- trade[[1]][,c(1, seq(3,37,4))]
  intrate_diff <<- trade[[1]][,c(1,seq(4, 37, 4))]
  
  returns_each <- FxReturn( 1, interest = with_interest )
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1, interest = with_interest ) )
  }
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  perannum <- sum( portfolio_return$dailyreturn, na.rm = T ) / nrow( portfolio_return ) * 252
  sharpe <- perannum / ( sd( cumsum(portfolio_return$dailyreturn), na.rm = T) * sqrt( 252 ) )
  if (sharpe_bool) return (sharpe)
  return (perannum)
}
r_Vol = list(StrategyEvaluation_Vol()*100,
               StrategyEvaluation_Vol(c(3,1,T,F))*100,
               StrategyEvaluation_Vol(c(6,1,T,F)) * 100,
               StrategyEvaluation_Vol(c(12,1,T,F)) * 100)
print(r_Vol)
