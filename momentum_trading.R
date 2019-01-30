#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 1 - MOMENTUM ANOMALY      ##############
#########          Marcell Kujbus               ##############
##############################################################

#in this section, I show how to trade the Momentum anomaly dollar neutrally based on Menkhoff(2012). 
#Keywords: Momentum, Sorting, Equal-weighted portfolio, Dynamic trading, 
#FX Market, currency excess returns

#I form 3 portfolios based on lagged returns over the previous f = 1,3,6,9,12 months.
#1/3, 1/3, 1/3 weights will be assigned to the best
#3 crcies, whereas -1/3, -1/3, -1/3 will be assigned to the "losers".
#These portfolios are held for h = 1,3,6,9,12 months.

source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")
Mom <- function(f = 1, crcy_idx ){
  #Mom is a function that calculates MOM signals based on f.
  #f denotes months, and I calculate w working days
  #spotinterst means that it can work with a data comes from the function
  #SpotInterest() only
  tseries = na.omit(SpotInterest(crcy_idx))
  return (tseries %>%
                 mutate(Mom = c( rep( NA, f * 21), 
                       diff( Excessreturn.V1, lag = f * 21 ) ) ) )

}
TableMaker_Momentum <- function(f = 1, h = 1){
  #TableMaker is a function that has two outputs as a list:
  #the first is all the data for currencies (Date, Spotprice, intrate differentials, logreturn)
  #the second consisting of just the MOM values.
  workingtable <- Mom( f = f, crcy_idx = 1 )
  for (i in 2:9) workingtable = left_join( workingtable, Mom( f, i ), by = 'Date')
  mom_idx <- c("Date", select_vars(names(workingtable), starts_with('Mom', ignore.case = TRUE)))
  momentum <- workingtable[ -(1 : (f * 21)), mom_idx ]
  return ( list( workingtable[ -(1 : (f * 21)), ], momentum ) )
}
Trade_Momentum <- function(f = 1, h = 1){
  #The trade function has the base data table for currencies as an output,
  #and its second output is the weight allocation for all days.
  #f and h is measured in months
  workingtable <- TableMaker_Momentum(f, h )
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
StrategyEvaluation_Momentum <- function(fh = c( 1, 1, TRUE ) ){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating Momentum),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]
  h <- fh[2]
  with_interest <- fh[3] #boolean variable
  trade <- Trade_Momentum(f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  if (with_interest == FALSE) returns <- select_vars(names(trade[[1]]), starts_with('log', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                      as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  perannum <- sum( portfolio_return$dailyreturn, na.rm = T ) *252 / nrow( portfolio_return )
  sharpe <- PerformanceAnalytics::SharpeRatio.annualized(portfolio_return)
  model <- lm(portfolio_return$dailyreturn ~ 1)
  p = as.numeric(lmtest::coeftest(model, vcov = sandwich::NeweyWest(model, verbose = T))[1,4])
  result <- paste0("p.a=",round(100 * perannum, 2),',sh=',round(sharpe,2),',p=',round(p,3))
  return (result)
}
StrategyEvaluation_plot_Momentum <- function(f = 1, h = 1){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  trade <- Trade_Momentum(f, h)
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = 100 *cumsum( diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}







