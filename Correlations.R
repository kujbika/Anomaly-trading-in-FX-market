#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########     SECTION 7 - Correlations          ##############
#########         © Marcell Kujbus              ##############
##############################################################

#In this subcode, I calculate pairwise correlations between
#Mom, Carry and Vol trading logreturns

#please notice that I cheated here: I rewrote manually the Strategy_Evaluation_plot_ functions
#in all the 3 codes (momentum_tradinf.R, carry_trading.R, volatility_trading.R, MAtiming.R).
#The single fact happened that I deleted '100 * cumsum()' from those.
#Reason: I want to calculate the correlation between the logreturns.
##Please set it back after usage!

####simple method
correlationCalc <- function(anomaly1, anomaly2){
  a1 <- tolower(anomaly1); a2 <- tolower(anomaly2)
  source(paste0("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/", a1, "_trading.R") )
  source(paste0("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/", a2, "_trading.R") )
  an1 <- get(paste0('StrategyEvaluation_plot_',anomaly1 )); an2 <- get(paste0('StrategyEvaluation_plot_',anomaly2 )) 
  corr <- data.table(an1(f = 1, h =1)[,2])
  vec <- c(1,1,1,6,1,12)
  for (i in seq(3, length(vec), 2)) corr = corr %>% cbind(an1(f =vec[i], h = vec[i+1])[,2])
  for (i in seq(1, length(vec), 2)) corr = corr %>% cbind(an2(f =vec[i], h = vec[i+1])[,2])
  names(corr) = c(paste0(a1,'11'),paste0(a1,'16'),paste0(a1,'112'),paste0(a2,'11'),paste0(a2,'16'),paste0(a2,'112') )
  return (cor(corr))
  }
correlationCalc('Momentum','Carry')
correlationCalc('Momentum','Volatility')
correlationCalc('Carry','Volatility')


####MA timing
correlationCalc_MA <- function(anomaly1, anomaly2){
  source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/MAtiming.R" )
  ChooseAnomaly(anomaly1)
  anomaly <<- anomaly1
  corr <- data.table(StrategyEvaluationplot_MA(f = 1, h =1)[,2])
  vec <- c(1,1,1,6,1,12)
  for (i in seq(3, length(vec), 2)) corr = corr %>% cbind(StrategyEvaluationplot_MA(f =vec[i], h = vec[i+1])[,2])
  ChooseAnomaly(anomaly2)
  anomaly <<- anomaly2
  for (i in seq(1, length(vec), 2)) corr = corr %>% cbind(StrategyEvaluationplot_MA(f =vec[i], h = vec[i+1])[,2])
  names(corr) = c(paste0(anomaly1,'MA11'),paste0(anomaly1,'MA16'),paste0(anomaly1,'MA112'),
                  paste0(anomaly2,'MA11'),paste0(anomaly2,'MA16'),paste0(anomaly2,'MA112') )
  return (cor(corr))
}
correlationCalc_MA('Momentum','Carry')
correlationCalc_MA('Momentum','Volatility')
correlationCalc_MA('Carry','Volatility')
