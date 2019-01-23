source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")
#in this subcode, I will show how to trade the Momentum anomaly dollar neutrally
#based on Menkhoff(2012). 
#Keywords: Momentum, Sorting, Equal-weighted portfolio, Dynamic trading

#Based on Menkhoff(2012), I form 3 portfolios based on lagged returns
#over the previous f = 1,3,6,9,12 months. These portfolios are held for
#1,3,6,9,12 months. 1/3, 1/3, 1/3 weights will be assigned to the best
#3 crcies, whereas -1/3, -1/3, -1/3 will be assigned to the "losers".

library(dplyr)
Mom <- function(f, tseries){
  #f denotes months, and I calculate w working days
  return (na.omit(tseries) %>%
            mutate(Mom = c( rep( NA, f * 21), 
                            diff( Spot, lag = f * 21 ) ) ) )
}

TableMaker <- function(f = 1, h = 1){
  freq = round(252 / (h * 21 ) )
  workingtable <- Mom( f, SpotInterest( 1, freq ) )
  for (i in 2:9) workingtable = dplyr::left_join( workingtable, Mom( f, SpotInterest( i, freq ) ), by = 'Date')
  #log(spot) price for all currency (daily return = log(s1/s0) = log(s1) - log(s0) )
  workingtable[, seq(2, 28, 3)] = workingtable[, seq(2, 28, 3)] %>% 
                                  log( )
  #log(1+return) for all currency
  workingtable[, seq(3, 28, 3)] = workingtable[, seq(3, 28, 3)]  %>% 
                                  log1p( )
  
  moms <- workingtable[ -(1 : (f * 21)), c( 1, seq( 4, 28, 3 ) ) ]
  
  return ( list( workingtable[ -(1 : (f * 21)), ], moms ) )
}
WeightAssigner <- function(rowfromtable){
  weight <- rep(0,9)
  idx <- order( rowfromtable[, 2 : 10], na.last = NA)
  weight[ idx[ 1 : 3 ] ] <- 1/3
  weight[ tail( idx, 3 ) ] <- -1/3
  return (transpose(data.table(weight)))
}

Trade <- function(f = 1, h = 1){
  #f and h is measured in months
  workingtable <- TableMaker(f, h)
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

StrategyEvaluation <- function(fh = c(1, 1, TRUE, FALSE ) ){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating Momentum),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  f <- fh[1]
  h <- fh[2]
  with_interest <- fh[3] #boolean variable
  sharpe_bool <- fh[4] #boolean variable
  trade <- Trade(f, h)
  spotlogdiff <- data.table(trade[[1]][,-1])[,lapply(.SD,diff)][ , c( seq( 1, 27, 3 ) ) ]
      FxReturn <- function(rowidx, interest = TRUE){
        #this function only works with the proper configuration of the table
        spot_return <- spotlogdiff[rowidx + 1 ,]
        spot_return[is.na(spot_return) == T] = 0
        colnames(spot_return) <- c('V1', 'V2', 'V3',
                                    'V4', 'V5', 'V6',
                                    'V7', 'V8', 'V9')
        if (interest == FALSE) return (spot_return)
        intrate_return <- trade[[1]][rowidx + 1, seq(3,27,3)]
        
        returns_each <- data.table(as.matrix(spot_return) + as.matrix(intrate_return))
        returns_each[is.na(returns_each) == T] = 0
        colnames(returns_each) <- c('V1', 'V2', 'V3',
                                    'V4', 'V5', 'V6',
                                    'V7', 'V8', 'V9')
        return ( returns_each )
      }
  returns_each <- data.table( 0, 0, 0, 0, 0, 0, 0, 0, 0 )
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1, interest = with_interest ) )
  }
  portfolio_return <- data.table( dailyreturn = diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                      as.matrix( t ( returns_each ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  perannum <- sum( portfolio_return$dailyreturn, na.rm = T ) / nrow( portfolio_return ) * 252
  sharpe <- perannum / ( sd( cumsum(portfolio_return$dailyreturn)) * sqrt( 252 ) )
  if (sharpe_bool) return (sharpe)
  return (perannum)
}






