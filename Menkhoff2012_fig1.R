#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 2 - CARRY ANOMALY         ##############
#########         © Marcell Kujbus              ##############
##############################################################

#at this step I reproduce Fig2 from Menkhoff2012


source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
library(ggplot2)

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
#r = StrategyEvaluation2()

trajectory11 <- StrategyEvaluation2( c( 1, 1 ) )
trajectory61 <- StrategyEvaluation2( c( 6, 1 ) )
trajectory121 <- StrategyEvaluation2( c( 12, 1 ) )
trajectory <- trajectory11 %>% left_join(trajectory61, by = '.') %>% 
  left_join(trajectory121, by = '.')
names(trajectory) <- c('Date', 'MOM(1,1)', 'MOM(6,1)', 'MOM(12,1)')
  

p <- ggplot(trajectory) + geom_line( aes(Date, `MOM(1,1)`), color = 'blue') +
  geom_line( aes(Date, `MOM(6,1)`), color = "red")+
  geom_line( aes(Date, `MOM(12,1)`), color = "black")+
  labs(y = 'Cumulative excess return (in %)') +
  scale_x_date(date_breaks = "2 year")+ggthemes::theme_hc() 
  #annotate('text', x = as.Date('2014-01-01'), y = 200, label = 'MOM(1,1)', colour = 'blue')+
  #annotate('text', x = as.Date('2013-05-01'), y = -310, label = 'MOM(6,1)', colour = 'red')+
  #annotate('text', x = as.Date('2013-05-01'), y = -60, label = 'MOM(12,1)', colour = 'black')+
  #annotate('rect', xmin = as.Date("2007-01-01"), xmax = as.Date('2010-01-01'),
  #         ymin = -10, ymax = 10, alpha = .2)
print(p)
