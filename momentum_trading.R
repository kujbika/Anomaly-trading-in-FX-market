source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")
#in this subcode, I will show how to trade the Momentum anomaly dollar neutrally
#based on Menkhoff(2012). 
#Keywords: Momentum, Sorting, Equal-weighted portfolio, Dynamic trading, 
#FX Market, currency excess returns

#Based on Menkhoff(2012), I form 3 portfolios based on lagged returns
#over the previous f = 1,3,6,9,12 months. These portfolios are held for
#1,3,6,9,12 months. 1/3, 1/3, 1/3 weights will be assigned to the best
#3 crcies, whereas -1/3, -1/3, -1/3 will be assigned to the "losers".
Mom <- function(f, tseries){
  #Mom is a function that calculates MOM signals based on f.
  #f denotes months, and I calculate w working days
  return (na.omit(tseries) %>%
            mutate(Mom = c( rep( NA, f * 21), 
                            diff( Spot, lag = f * 21 ) ) ) )
}
TableMaker_Mom <- function(f = 1, h = 1){
  #TableMaker is a function that has two outputs as a list:
  #the first is all the data for currencies (Date, Spotprice, intrate differentials, logreturn)
  #the second consisting of just the MOM values.
  workingtable <- Mom( f, SpotInterest( 1) )
  for (i in 2:9) workingtable = left_join( workingtable, Mom( f, SpotInterest( i) ), by = 'Date')
  momentum <- workingtable[ -(1 : (f * 21)), c( 1, seq( 5, 37, 4 ) ) ]
  
  return ( list( workingtable[ -(1 : (f * 21)), ], momentum ) )
}
Trade_Mom <- function(f = 1, h = 1){
  #The trade function has the base data table for currencies as an output,
  #and its second output is the weight allocation for all days.
  #f and h is measured in months
  workingtable <- TableMaker_Mom(f, h)
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
StrategyEvaluation_Mom <- function(fh = c( 1, 1, TRUE, FALSE ) ){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating Momentum),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]
  h <- fh[2]
  with_interest <- fh[3] #boolean variable
  sharpe_bool <- fh[4] #boolean variable
  trade <- Trade_Mom(f, h)
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

r_Mom = list(StrategyEvaluation_Mom()*100,
             StrategyEvaluation_Mom(c(3,1,T,F))*100,
             StrategyEvaluation_Mom(c(6,1,T,F)) * 100,
             StrategyEvaluation_Mom(c(12,1,T,F)) * 100)

print(r_Mom)

##not important from here

StrategyEvaluation2 <- function(fh){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating Momentum),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]
  h <- fh[2]
  trade <- Trade(f, h)
  spotlogret <- trade[[1]][,c(1, seq(3,37,4))]
  intrate_diff <- trade[[1]][,c(1,seq(4, 37, 4))]
  FxReturn <- function(rowidx){
    #this function only works with the proper configuration of the table
    spot_return <- spotlogret[rowidx + 1 ,-1]
    spot_return[is.na(spot_return) == T] = 0
    colnames(spot_return) <- c('V1', 'V2', 'V3',
                               'V4', 'V5', 'V6',
                               'V7', 'V8', 'V9')
    intrate_return <- intrate_diff[rowidx, -1]
    intrate_return[is.na(intrate_return) == T] = 0
    returns_each <- data.table(as.matrix(spot_return) + as.matrix(intrate_return))
    return ( returns_each ) 
  }
  returns_each <- FxReturn( 1 )
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1) )
  }
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                       as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( cumsum(portfolio_return ))
  return (portfolio_return)
}
r = StrategyEvaluation2()




