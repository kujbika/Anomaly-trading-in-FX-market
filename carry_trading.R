#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 2 - CARRY ANOMALY         ##############
#########         © Marcell Kujbus              ##############
##############################################################


#in this subcode, I will show how to trade the Carry anomaly dollar neutrally
#Keywords: Carry, Sorting, Equal-weighted portfolio, Dynamic trading, 
#FX market, Interest rate differential

#Based on Menkhoff(2012), I form 3 portfolios according to the currencies' interest rates.
#The 'High' portfolio consists of the 3 currencies with the highest interest rates, whereas
# the 'low' portfolio is the combination of the 3 worst.
#A Carry investor goes long into the high, and goes short into the low portfolios.
#These portfolios are held for 1,3,6,9,12 months. 
#1/3, 1/3, 1/3 weights will be assigned to the best 3 currencies
#whereas -1/3, -1/3, -1/3 will be assigned to the "losers".
source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")


TableMaker_Carry <- function(f = NA, h = 1){
  #TableMaker_Carry is a function that has two outputs as a list:
  #the first is all the data for currencies (Date, Spotprice, logreturn, intrate differential) 
  #the second consisting of just the interest rate differentials (against US dollar)
  workingtable <- na.omit( SpotInterest( crcy_idx = 1 ) )
  for (i in 2:9) workingtable = left_join( workingtable, na.omit( SpotInterest( crcy_idx = i ) ), by = 'Date')
  interest_idx <- c( "Date", select_vars( names( workingtable ), starts_with('inte', ignore.case = TRUE)))
  interest = workingtable[ , interest_idx ]
  return ( list( workingtable, interest ) )
}
Trade_Carry <- function(f = NA, h = 1){
  #The Trade_Carry function has the base data table for currencies as its first output,
  #and its second output is the weight allocation for all days based on Carry.
  #f and h is measured in months
  workingtable <- TableMaker_Carry(f = f, h = h )
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
StrategyEvaluation_Carry <- function(h0 = c( NA, 1, TRUE ) ){
  #this is the actual evaluation of the Carry strategy
  #h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  h <- h0[2]
  with_interest = h0[3]
  trade <- Trade_Carry( h = h )
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
StrategyEvaluation_plot_Carry <- function(f = NA, h = 1){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  trade <- Trade_Carry( h = h )
  returns <- select_vars(names(trade[[1]]), starts_with('Exc', ignore.case = TRUE))
  returns_each <- trade[[1]][,returns] %>% mutate_all( funs(if_else(is.na(.), 0, .)))
  portfolio_return <- data.table( dailyreturn = 100 * cumsum( diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                              as.matrix( t ( returns_each ) ) ) ) ) 
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}
