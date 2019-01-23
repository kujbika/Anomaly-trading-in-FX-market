source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
#at this step I would like to reproduce Table1 from Menkhoff2012
####
table1_panela_excess <- expand.grid(f = c( 1, seq( 3, 12, 3 ) ), 
                                   h = c( 1, seq( 3, 12, 3 ) ),
                                   excess = TRUE,
                                   sharpe = FALSE)
table1_panela_excess$ret <- apply( table1_panela_excess, 1, StrategyEvaluation )

table1_panela_sharpe <- expand.grid(f = c( 1, seq( 3, 12, 3 ) ), 
                                   h = c( 1, seq( 3, 12, 3 ) ),
                                   excess = TRUE,
                                   sharpe = TRUE)
table1_panela_sharpe$sharpe <- apply( table1_panela_sharpe, 1, StrategyEvaluation )
####

table1_panela_spot <- expand.grid(f = c( 1, seq( 3, 12, 3 ) ),
                                  h = c( 1, seq( 3, 12, 3 ) ),
                                  excess = FALSE,
                                  sharpe = FALSE)
table1_panela_spot$ret <- apply( table1_panela_spot, 1, StrategyEvaluation)

table1_panela_sharpe_spot <- expand.grid(f = c( 1, seq(3,12,3)), 
                                   h = c( 1,seq( 3, 12, 3)),
                                   excess = FALSE,
                                   sharpe = TRUE)
table1_panela_sharpe_spot$sharpe1 <- apply( table1_panela_sharpe_spot,
                                         1, StrategyEvaluation)

####
table1_panela_excess$sharpe <- table1_panela_sharpe$sharpe
table1_panela_spot$sharpe <- table1_panela_sharpe_spot$sharpe1
table1_panela_excess
table1_panela_spot

rm(table1_panela_sharpe_spot); rm(table1_panela_sharpe)


