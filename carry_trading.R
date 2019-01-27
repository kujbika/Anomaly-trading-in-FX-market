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
  workingtable <- na.omit(SpotInterest( 1 ))
  for (i in 2:9) workingtable = left_join( workingtable, na.omit( SpotInterest( i ) ), by = 'Date')
  interest <- workingtable[ , c( 1, seq( 4, 28, 3 ) ) ]
  
  return ( list( workingtable, interest ) )
}
Trade_Carry <- function(f = NA, h = 1){
  #The Trade_Carry function has the base data table for currencies as its first output,
  #and its second output is the weight allocation for all days based on Carry.
  #f and h is measured in months
  workingtable <- TableMaker_Carry(h)
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
StrategyEvaluation_Carry <- function(h0 = c( NA, 1, TRUE, FALSE ) ){
  #this is the actual evaluation of the Carry strategy
  #h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  h <- h0[2]
  with_interest <- h0[3] #boolean variable
  sharpe_bool <- h0[4] #boolean variable
  trade <- Trade_Carry( h = h )
  spotlogret <<- trade[[1]][,c(1, seq(3,28,3))]
  intrate_diff <<- trade[[1]][,c(1,seq(4, 28, 3))]
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
StrategyEvaluation_plot_Carry <- function(f = NA, h = 1){
  #this is the actual evaluation of the MOM based strategy
  #f is the lookback time in months (for calculating sd),
  #whereas h is the portfolio reallocation frequency in months(holding period)
  #in this part I assume no transaction costs.
  trade <- Trade_Carry( h = h )
  spotlogret <<- trade[[1]][,c(1, seq(3,28,3))]
  intrate_diff <<- trade[[1]][,c(1,seq(4, 28, 3))]
  returns_each <- FxReturn( 1 )
  for (i in 2 : nrow( trade[[ 1 ]]) ){
    returns_each = returns_each %>% rbind( FxReturn( i - 1 ) )
  }
  portfolio_return <- data.table( dailyreturn = 100*cumsum(diag(as.matrix(trade[[ 2 ]][ , -1 ] ) %*%
                                                              as.matrix( t ( returns_each ) ) ) ) )
  portfolio_return <- trade[[ 2 ]][ , 1 ] %>% cbind( portfolio_return )
  return (portfolio_return)
}

#r_carry = list(StrategyEvaluation_carry()*100,
 #        StrategyEvaluation_carry(c(3,T,F))*100,
  #       StrategyEvaluation_carry(c(6,T,F)) * 100,
   #      StrategyEvaluation_carry(c(12,T,F)) * 100)
#print(r_carry)