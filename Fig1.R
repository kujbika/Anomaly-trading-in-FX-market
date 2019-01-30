#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 6 - Figures               ##############
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
trajectory61 <- StrategyEvaluationplot_MA( lag = 10, f= 6, h = 1 )
trajectory121 <- StrategyEvaluationplot_MA( lag = 10, f= 12, h = 1 )
trajectory <- trajectory11 %>% left_join(trajectory61, by = '.') %>% 
  left_join(trajectory121, by = '.')
names(trajectory) <- c('Date', '(1,1)', '(6,1)', '(12,1)')

trajectory11 <- StrategyEvaluation_plot_Volatility( f= 1, h = 1 )
trajectory61 <- StrategyEvaluation_plot_Volatility(f= 6, h = 1 )
trajectory121 <- StrategyEvaluation_plot_Volatility( f= 12, h = 1 )
trajectory <- trajectory11 %>% left_join(trajectory61, by = '.') %>% 
  left_join(trajectory121, by = '.')
names(trajectory) <- c('Date', '(1,1)', '(6,1)', '(12,1)')

vol <- ggplot(trajectory) + geom_line( aes(x = Date, y = `(1,1)`), color = 'blue') +
  geom_line( aes(x = Date, y = `(6,1)` ), color = "red")+
  geom_line( aes(x = Date, y = `(12,1)` ), color = "black")+
  labs(y = 'Cumulative excess return (in %)', x = ' ')+
  scale_x_date(date_breaks = "30 months")+ggthemes::theme_hc()+theme(axis.title.x=element_blank(),
                                                                      axis.text.x=element_blank())+
  annotate('text', x = as.Date('2017-03-01'), y = 0, label = 'Vol(1,1)', fontface = 2,colour = 'blue')+
  annotate('text', x = as.Date('2017-06-01'), y = 20, label = 'Vol(12,1)', fontface = 2,colour = 'black')+
  annotate('text', x = as.Date('2015-02-01'), y = 8, label = 'Vol(6,1)', fontface = 2,colour = 'red')+
  annotate('rect', xmin = as.Date("2007-11-01"), xmax = as.Date('2010-01-01'),
           ymin = -30, ymax = 22, alpha = .2)
grid.newpage()
grid.draw(rbind(ggplotGrob(vol), ggplotGrob(volma), size = "last"))

