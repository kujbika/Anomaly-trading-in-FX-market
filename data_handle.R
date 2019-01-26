library(data.table)
library(dplyr)
MatlabDat <- R.matlab::readMat("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/FX.mat")$X
types = c('Spot', 'Forward','Date', 'R', 'Q')
date = as.Date(MatlabDat[ 1 ][[ 1 ]][[ 1 ]][[ 3 ]], origin = '0000-01-01')-1
MDat <- function(crcy_idx, type_idx){
  return (MatlabDat[ crcy_idx ][[ 1 ]][[ 1 ]][[ type_idx]])
}
#DataFilterer makes a query, which output is the desired 
#dataframe consisting of the currency's key type
#Note: the base currency is USD

SpotForwardFilterer <- function(crcy_idx, type){
  # crcy idx ranges between 1 and 9
  # type can be 'Spot' and 'Forward'
  pair <- MDat(crcy_idx, 6)
  prime_crc <- substr( pair, 1, 3)
  type_idx <- match( type, types ) 
  if ( prime_crc == 'USD'){
    data <-  data.frame(date, 1 / MDat(crcy_idx, type_idx) )
    colnames(data) <- c('Date', type )
  }else{
    data <-  data.frame(date, MDat(crcy_idx, type_idx) )
    colnames(data) <- c('Date', type )
  }
  if (prime_crc != 'USD' & type_idx == 4) data = data.frame(date, MDat(crcy_idx, 5))
  return ( data.table::data.table( data ) )
}

SpotInterest <- function( crcy_idx){
  #SpotInterest is a function that will give a data table with columns
  #Date, Spot price, logreturns based on spot, the interest rate differential,
  #according to return_t = i(country)_t - i(USA)_t - dlog(S_(t+1) )
  pair <- MDat(crcy_idx, 6)
  prime_crc <- substr( pair, 1, 3)
  r <- MDat(crcy_idx, 4)/ 252
  q <- MDat(crcy_idx, 5)/ 252
  spot <- SpotForwardFilterer(crcy_idx, 'Spot') %>% 
          mutate(logSpotRet = c(0,diff(log(Spot))))
  if (prime_crc == 'USD') interest = (r - q)[,7] 
  interest = (q - r)[,7]
  #this multiplying with -1 happens because sometimes the us dollar is the base
  #currency, and sometimes the other.
  return (data.table(spot, interest))
}
SpotFw <- function(crcy_idx){
  #if the CIP holds, then return_t+1 ~ logforward_t - logspot_t+1
  spot <- SpotForwardFilterer(crcy_idx, 'Spot') %>% mutate(logSpot = log(Spot))
  forward <- SpotForwardFilterer(crcy_idx, 'Forward')[,4] %>% 
    mutate(logForward = log(V4))
  tabble <- data.table(Date = spot[,1], Spot = spot$logSpot,logForward =forward$logForward)
 return (tabble) 
}
FxReturn <- function(rowidx, interest = TRUE){
  #this function only works with the proper configuration of the table
  #see:StrategyEvaluation function on momentum_trading.R
  spot_return <- spotlogret[rowidx + 1 ,-1]
  spot_return[is.na(spot_return) == T] = 0
  colnames(spot_return) <- c('V1', 'V2', 'V3',
                             'V4', 'V5', 'V6',
                             'V7', 'V8', 'V9')
  if (interest == FALSE) return (spot_return)
  intrate_return <- intrate_diff[rowidx, -1]
  intrate_return[is.na(intrate_return) == T] = 0
  returns_each <- data.table(as.matrix(spot_return) + as.matrix(intrate_return))
  return ( returns_each ) 
}

