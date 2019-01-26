source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
library(ggplot2)


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
