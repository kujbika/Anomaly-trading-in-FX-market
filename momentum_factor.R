library(dplyr)
source("C:/Users/User/Documents/GitHub/MA-trade-in-FX-factor-portfolios/data_handle.R")
#in this subcode, I will show how to trade the Momentum anomaly
#based on Menkhoff(2012). 
#Keywords: Momentum, Sorting, Equal-weighted portfolio, Dynamic trading

#According to Menkhoff(2012), I form 3 portfolios based on lagged returns
#over the previous f = 1,3,6,9,12 months. These portfolios are held for
#h = 1,3,6,9,12 months. 1/3, 1/3, 1/3 weights will be assigned to the best
#3 crcies, whereas -1/3, -1/3, -1/3 will be assigned to the "losers".(dollar neutral)
FxReturn <- function(intrate, price1, price0){
  #be careful on the rates being annualized etc
  return (log( 1 + intrate1 )+
            log( ( 1 + price1 ) / ( 1 + price0 )) )
}

tseries <- DataFilterer(1, 'Spot')
Mom <- function(f, tseries){
  #f denotes months, and I calculate w working days
  return (na.omit(tseries) %>%
            mutate(Mom = c( rep( 0, f * 22), 
                            diff( Spot, lag = f * 22 ) ) ) )
}
Portfolio <- function(momvector){
  weights <- c(rep(-1/3, 3), rep(0,3), rep(1/3, 3))
  moms <- transpose(data.table(momvector)) %>% sort() %>% rbind(transpose(data.table(weights)))
}