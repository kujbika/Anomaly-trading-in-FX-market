#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 2 - CARRY ANOMALY         ##############
#########         © Marcell Kujbus              ##############
##############################################################

#at this step I reproduce Fig2 from Menkhoff2012

ChooseAnomaly <- function(anomaly = 'Carry'){
  #choose witch anomaly should be handled with MA
  rm(list=setdiff(ls(), "anomaly"))
  if (anomaly == 'Momentum') {source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
  }else if (anomaly == 'Carry'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/carry_trading.R")
  }else if (anomaly == 'Volatility'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/volatility_trading.R")
  }
}
anomaly = "Carry"
ChooseAnomaly(anomaly)
library(ggplot2)

ploty <- get(paste0('StrategyEvaluation_plot_',anomaly ))
#Note: for anomaly == "Carry" only h counts
trajectory11 <- ploty( 1, 1 )
trajectory61 <- ploty( 1, 6 )
trajectory121 <- ploty( 1, 12 )
trajectory <- trajectory11 %>% left_join(trajectory61, by = '.') %>% 
  left_join(trajectory121, by = '.')
names(trajectory) <- c('Date', '(1,1)', '(6,1)', '(12,1)')
  
p <- ggplot(trajectory) + geom_line( aes(x = Date, y = `(1,1)`), color = 'blue') +
  geom_line( aes(x = Date, y = `(6,1)` ), color = "red")+
  geom_line( aes(x = Date, y = `(12,1)` ), color = "black")+
  labs(y = 'Cumulative excess return (in %)', x = ' ')+
  scale_x_date(date_breaks = "30 months")+ggthemes::theme_hc()+
  annotate('text', x = as.Date('2016-12-01'), y = 68, label = 'MOM(1,1)', colour = 'blue')+
  annotate('text', x = as.Date('2014-11-01'), y = 60, label = 'MOM(1,6)', colour = 'red')+
  annotate('text', x = as.Date('2015-12-01'), y = 28, label = 'MOM(1,12)', colour = 'black')+
  annotate('rect', xmin = as.Date("2007-11-01"), xmax = as.Date('2010-01-01'),
   ymin = -10, ymax = 75, alpha = .2)
print(p)
