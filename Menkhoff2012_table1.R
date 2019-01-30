#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 4 - Table1                ##############
#########         © Marcell Kujbus              ##############
##############################################################

#at this step I reproduce Table1 from Menkhoff2012


ChooseAnomaly <- function(anomaly = 'Carry'){
  #choose witch anomaly should be handled with MA
  rm(list=setdiff(ls(), "anomaly"))
  if (anomaly == 'Momentum') {source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
  }else if (anomaly == 'Carry'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/carry_trading.R")
  }else if (anomaly == 'Volatility'){source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/volatility_trading.R")
  }
}
anomaly = "Volatility"
ChooseAnomaly(anomaly)

table1_panela_excess <- expand.grid(f = c( 1, seq( 3, 12, 3 )) , 
                                   h = c( 1, seq( 3, 12, 3 ) ),
                                   excess = TRUE)
table1_panela_excess$ret <- apply( table1_panela_excess, 1, get(paste0('StrategyEvaluation_',anomaly )))

table1_panela_spot <- expand.grid(f =c( 1, seq( 3, 12, 3 )), 
                                   h = c( 1, seq( 3, 12, 3 ) ),
                                   excess = FALSE)
                                   
table1_panela_spot$ret <- apply( table1_panela_spot, 1, get(paste0('StrategyEvaluation_',anomaly )) )
####


table1_panela_excess
table1_panela_spot

rm(table1_panela_sharpe_spot); rm(table1_panela_sharpe)

#####MA timing tables
source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/MAtiming.R")
anomaly = "Volatility"
ChooseAnomaly(anomaly)

table1_panela_excessMA <- expand.grid(lag = 10, f = c( 1, seq( 3, 12, 3 )), 
                                    h = c( 1, seq( 3, 12, 3 ) ), excess = TRUE)
                                    
table1_panela_excessMA$ret <- apply( table1_panela_excessMA, 1, StrategyEvaluation_MA)

table1_panela_spotMA <- expand.grid(lag = 10, f = c( 1, seq( 3, 12, 3 )), 
                                      h = c( 1, seq( 3, 12, 3 ) ), excess = FALSE)
table1_panela_spotMA$ret <- apply( table1_panela_spotMA, 1, StrategyEvaluation_MA )





