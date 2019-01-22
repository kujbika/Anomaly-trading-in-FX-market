source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_factor.R")
TableMaker <- function(f = 1, freq = 252){
  workingtable <- Mom( f, SpotInterest( 1, freq ) )
  for (i in 2:9) workingtable = dplyr::left_join( workingtable, Mom( f, SpotInterest( i, freq ) ), by = 'Date')
  moms <- workingtable[ -(1 : (f * 22)), c( 1, seq( 4, 28, 3 ) ) ]
  return ( list( workingtable[ -(1 : (f * 22)), ], moms ) )
}
WeightAssigner <- function(rowfromtable){
  weight <- rep(0,9)
  idx <- order( rowfromtable[,2 : 10], na.last = NA)
  weight[idx[1:3]] = -1/3
  weight[idx[6:9]] <- 1/3
  return (transpose(data.table(weight)))
}

Trade <- function(workingtable){
  weights <- WeightAssigner( workingtable[ 1, ])
  for (i in 2:nrow(workingtable)) weights = weights %>%
      rbind( WeightAssigner( workingtable[ i, ] ) )
  return (weights)
}
trading <- Trade(TableMaker()[[2]])
