source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/data_handle.R")
#in this subcode, I will show how to trade the Momentum anomaly dollar neutrally
#based on Menkhoff(2012). 
#Keywords: Momentum, Sorting, Equal-weighted portfolio, Dynamic trading

#Based on Menkhoff(2012), I form 3 portfolios based on lagged returns
#over the previous f = 1,3,6,9,12 months. These portfolios are held for
#1,3,6,9,12 months. 1/3, 1/3, 1/3 weights will be assigned to the best
#3 crcies, whereas -1/3, -1/3, -1/3 will be assigned to the "losers".


#THE DIFFERENCE IS IN THE F BEING 21 OR 22
library(dplyr)
Mom <- function(f, tseries){
  #f denotes months, and I calculate w working days
  return (na.omit(tseries) %>%
            mutate(Mom = c( rep( 0, f * 21), 
                            diff( Spot, lag = f * 21 ) ) ) )
}

TableMaker <- function(f = 1, freq = 252){
  workingtable <- Mom( f, SpotInterest( 1, freq ) )
  for (i in 2:9) workingtable = dplyr::left_join( workingtable, Mom( f, SpotInterest( i, freq ) ), by = 'Date')
  #log(spot) price for all currency
  workingtable[, seq(2, 28, 3)] = workingtable[, seq(2, 28, 3)] %>% 
                                  mutate_all( funs( replace( ., is.na( . ), 1 ) ) )%>% 
                                  log( )
  #log(1+return) for all currency
  workingtable[, seq(3, 28, 3)] = (1 + workingtable[, seq(3, 28, 3)] ) %>% 
                                  mutate_all( funs( replace( ., is.na( . ), 1 ) ) )  %>% 
                                  log( )
  
  moms <- workingtable[ -(1 : (f * 21)), c( 1, seq( 4, 28, 3 ) ) ]
  
  return ( list( workingtable[ -(1 : (f * 21)), ], moms ) )
}
WeightAssigner <- function(rowfromtable){
  weight <- rep(0,9)
  idx <- order( rowfromtable[,2 : 10], na.last = NA)
  weight[idx[1:3]] = -1/3
  weight[idx[6:9]] <- 1/3
  return (transpose(data.table(weight)))
}

Trade <- function(f = 1, h = 1){
  #h is in days, f is in months!!!!
  freq <- round(252 / h)
  workingtable <- TableMaker(f, freq)
  weights <- WeightAssigner( workingtable[[ 2 ]][ 1, ]) %>%
             slice( rep( row_number( ), h ) ) 
  realloc <- seq(1 + h, nrow(workingtable[[ 2 ]]), h)
  for (i in realloc){
    newweight <- WeightAssigner( workingtable[[ 2 ]][ i, ] ) %>%
                 slice( rep( row_number( ), h ) ) 
    weights = weights %>% rbind(newweight)
  }
  weights = workingtable[[2]][ , 1] %>% cbind(weights[ 1 : nrow( workingtable[[ 2 ]]), ])
  return ( list( workingtable[[ 1 ]], weights ) )
}

StrategyEvaluation <- function(f = 1, h = 1){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating Momentum),
  #whereas h is the portfolio reallocation frequency in days(holding period)
  #in this part I assume no transaction costs.
  trade <- Trade(f, h)
  logdiff <- data.table(trade[[1]][,-1])[,lapply(.SD,diff)]
  FxReturn <- function(rowidx){
    #this function only works with the proper configuration of the table
    spot_return <- logdiff[rowidx + 1 ,c(seq(1,27,3))]
    intrate_return <- trade[[1]][rowidx , seq(3,27,3)] #/ h #szerintem le kell osztani h-val, 
    
    returns_each <- data.table(as.matrix(spot_return) + as.matrix(intrate_return))
    colnames(returns_each) <- c('V1', 'V2', 'V3',
                                'V4', 'V5', 'V6',
                                'V7', 'V8', 'V9')
    return ( returns_each )
  }
  returns_each <- data.table( 0, 0, 0, 0, 0, 0, 0, 0, 0 )
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1 ) )
  }
  portfolio_return =data.table(dailyreturn = diag(as.matrix(trade[[2]][,-1]) %*% as.matrix(t(returns_each))))
  portfolio_return = trade[[2]][,1] %>% cbind(portfolio_return)
  perannum <- sum(portfolio_return$dailyreturn, na.rm = T)/nrow(portfolio_return)*252
  return (list(portfolio_return, perannum))
}
#intrate return is not configured well yet.
