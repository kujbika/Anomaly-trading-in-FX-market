#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########        SECTION 0 - THE DATA           ##############
#########          © Marcell Kujbus             ##############
##############################################################

#This subcode includes all the relevant functions for calculating excess returns based on the
#anomalites: Momentum, Carry and Volatility. 
#The work is based on Menhkoff et al(2012) and Han et al(2013).

library(data.table)
library(dplyr)
MatlabDat <- R.matlab::readMat("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/FX.mat")$X
types = c('Spot', 'Forward','Date', 'R', 'Q')
date = as.Date(MatlabDat[ 1 ][[ 1 ]][[ 1 ]][[ 3 ]], origin = '0000-01-01')-1
MDat <- function(crcy_idx, type_idx){
  #MDat converts the Matlab format data into R properly. 
  return (MatlabDat[ crcy_idx ][[ 1 ]][[ 1 ]][[ type_idx]])
}
SpotForwardFilterer <- function(crcy_idx, type){
  #SpotForwardFilterer makes a query, which output is the desired
  #data table that is a time series of the currency's key type.
  # crcy idx ranges between 1 and 9
  # type can be 'Spot' and 'Forward'
  #Note: the base currency is USD.
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
SpotInterest <- function( crcy_idx, with_interest = TRUE){
  #SpotInterest is a function that will yield a data table with columns
  #Date, Spot price, logreturns based on spot, the interest rate differential,
  #according to excessFXreturn_t = i(country)_t - i(USA)_t - dlog(S_(t+1) )
  pair <- MDat(crcy_idx, 6)
  prime_crc <- substr( pair, 1, 3)
  r <- MDat(crcy_idx, 4)/ 252
  q <- MDat(crcy_idx, 5)/ 252
  spot <- SpotForwardFilterer(crcy_idx, 'Spot') %>% 
          mutate(logSpotRet = c(0,diff(log(Spot))))
  if (prime_crc == 'USD') interestt = (r - q)[,7] 
  interestt = (q - r)[,7]
  #this multiplying with -1 happens because sometimes the us dollar is the base
  #currency, and sometimes the other.
  excessret = FxReturn(spotlogret = spot[,3], intrate_diff = interestt ,with_interest)
  return (data.table(spot, interestt, Excessreturn = excessret))
}

FxReturn <- function(spotlogret, intrate_diff, with_interest = TRUE){
  #this function only works with the proper configuration of the table
  #see e.g:StrategyEvaluation function on momentum_trading.R
  nc <- ncol(spotlogret)
  returns_each = data.table(as.matrix(spotlogret[2])+as.matrix(intrate_diff[1]))
  for (rowidx in 2 : length(spotlogret)){
    spot_return <- data.table(spotlogret[rowidx + 1])
    spot_return[is.na(spot_return) == T] = 0
    if (with_interest == FALSE) return (rbindlist(list(returns_each,spot_return)))
    intrate_return <- intrate_diff[rowidx]
    intrate_return[is.na(intrate_return) == T] = 0
    asofday <- data.table(as.matrix(spot_return) + as.matrix(intrate_return))
    returns_each <- rbindlist(list(returns_each, asofday))}
  return ( returns_each ) 
}
WeightAssigner <- function(rowfromtable){
  #this function assigns weights for every currency based on its anomaly order.
  weight <- rep(0,9)
  idx <- order( rowfromtable[, 2 : 10], na.last = NA)
  weight[ idx[ 1 : 3 ] ] <- -1/3
  weight[ tail( idx, 3 ) ] <- 1/3
  return (transpose(data.table(weight)))
}


