library(data.table)
MatlabDat <- R.matlab::readMat("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/FX.mat")$X
types = c('Spot', 'Forward','Date', 'R', 'Q')
date = as.Date(MatlabDat[ 1 ][[ 1 ]][[ 1 ]][[ 3 ]], origin = '0000-01-01')-1
MDat <- function(crcy_idx, type_idx){
  return (MatlabDat[ crcy_idx ][[ 1 ]][[ 1 ]][[ type_idx]])
}
#DataFilterer makes a query, which output is the desired 
#dataframe consisting of the currency's key type
#Note: the base currency is USD
DataFilterer <- function(crcy_idx, type){
  # crcy idx ranges between 1 and 9
  # choose type from the types vector
  pair <- MDat(crcy_idx, 6)
  prime_crc <- substr( pair, 1, 3)
  type_idx <- match( type, types ) 
  if ( prime_crc == 'USD' & type_idx < 4 ){
    data <-  data.frame(date, 1 / MDat(crcy_idx, type_idx) )
    colnames(data) <- c('Date', type )
  }else{
    data <-  data.frame(date, MDat(crcy_idx, type_idx) )
    colnames(data) <- c('Date', type )
  }
  return ( data.table::data.table( data ) )
}

SpotInterest <- function(crcy_idx, h = 1){
  #h is the reallocation time in months
  freq <- round(252 / (h * 21))
  tabble <- data.table(DataFilterer(crcy_idx, 'Spot'),
                       R = DataFilterer(crcy_idx, "R")[[ 8 ]] / freq)
  return (tabble)
}
#example = DataFilterer(crcy_idx = 3, type = 'Spot') #originally USDYPJ, but we need YPJUSD
#plot(example, type = 'l', main = ("USDYPJ"), ylab = 'Price', xlab = " ")

