#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 5a - Figures              ##############
#########         © Marcell Kujbus              ##############
##############################################################

#at this step I reproduce Fig2 from Menkhoff2012 based on MA timing
rm(list = ls())
source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/MAtiming.R")
library(ggplot2)
anomaly = "Volatility"
ChooseAnomaly(anomaly)

#Note: for anomaly == "Carry" only h counts
trajectory11 <- StrategyEvaluationplot_MA( lag = 10, f= 1, h = 1 )
trajectory61 <- StrategyEvaluationplot_MA( lag = 10, f= 1, h = 6 )
trajectory121 <- StrategyEvaluationplot_MA( lag = 10, f= 1, h = 12 )
trajectory <- trajectory11 %>% left_join(trajectory61, by = '.') %>% 
  left_join(trajectory121, by = '.')
names(trajectory) <- c('Date', '(1,1)', '(6,1)', '(12,1)')

p <- ggplot(trajectory) + geom_line( aes(x = Date, y = `(1,1)`), color = 'blue') +
  geom_line( aes(x = Date, y = `(6,1)` ), color = "red")+
  geom_line( aes(x = Date, y = `(12,1)` ), color = "black")+
  labs(y = 'Cumulative excess return (in %)', x = ' ')+
  scale_x_date(date_breaks = "30 months")+ggthemes::theme_hc()+
  annotate('text', x = as.Date('2013-04-01'), y = 4, label = 'VolMA(10,1, 1)', fontface = 2,colour = 'blue')+
  annotate('text', x = as.Date('2016-12-01'), y = -8, label = 'VolMA(10,1,6)', fontface = 2,colour = 'black')+
  annotate('text', x = as.Date('2016-12-01'), y = 5, label = 'VolMA(10,1,12)', fontface = 2,colour = 'red')+
  annotate('rect', xmin = as.Date("2007-11-01"), xmax = as.Date('2010-01-01'),
           ymin = -20, ymax = 10, alpha = .2)+
  annotate('rect', xmin = as.Date("2014-11-15"), xmax = as.Date('2015-03-25'),
           ymin = -20, ymax = 10, alpha = .2)
  

print(p)

