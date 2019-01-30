#The cross-sectional profitability of anomaly trading on the FX market#

##############################################################
#########           2019.01.26.                 ############## 
#########      SECTION7 - DOUBLE SORTING        ##############
#########         © Marcell Kujbus              ##############
##############################################################

#based on Menkhoff et al(2012), I create 9 double sorted portfolio
#from the currencies, and track their performance.
#first layer is carry, second is momentum

source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/momentum_trading.R")
source("C:/Users/User/Documents/GitHub/Anomaly-trading-in-FX-market/carry_trading.R")

TableMakerDoublesort <- function(f = 1, h = 1){
  base_table = TableMaker_Momentum(f = f, h = h)
  base_table[[1]] = na.omit(base_table[[1]])
  base_table[[2]] = na.omit(base_table[[2]])
  interest_idx <- c("Date", select_vars(names(base_table[[1]]), starts_with('inte', ignore.case = TRUE)))
  interest <- base_table[[1]][,interest_idx]
  return (list( base_table[[1]], base_table[[2]], interest) )
  }

FirstLayer <- function(f = 1, h = 1){
  workingtable <- TableMakerDoublesort(f = f, h = h )
  weights <- WeightAssigner( workingtable[[ 3 ]][ 1, ]) %>%
    slice( rep( row_number( ), (h * 21 ) ) )
  realloc <- seq(1 + h * 21, nrow(workingtable[[ 3 ]]), h * 21)
  for (i in realloc){
    newweight <- WeightAssigner( workingtable[[ 3]][ i, ] ) %>%
      slice( rep( row_number( ), h * 21) ) 
    weights = weights %>% rbind(newweight)
  }
  weights = workingtable[[3]][ , 1] %>% cbind(weights[ 1 : nrow( workingtable[[ 3 ]]), ])
  return (list(workingtable, weights))
}

SecondLayer <- function(f = 1, h = 1){
  layer1 <- FirstLayer(f = f, h = h)
  momentums <- layer1[[1]][[2]]
  weights = layer1[[2]]
  o <- order(weights[1,2:10 ])
  o1 <- momentums[1,-1][o]
  part1 <- o[order(o1[1:3])]
  part2 <- o[3+order(o1[4:6])]
  part3 <- o[6+order(o1[7:9])]
  parts <- transpose(data.table(c(part1,part2,part3, h *21))) %>% tidyr::uncount(V10)#it is now double sorted by momentum
  realloc <- seq(1 + h * 21, nrow(momentums), h * 21)
  for (i in realloc){
    o <- order(weights[i,2:10 ])
    o1 <- momentums[i,-1][o]
    part1 <- o[order(o1[1:3])]
    part2 <- o[3+order(o1[4:6])]
    part3 <- o[6+order(o1[7:9])]
    part <- transpose(data.table(c(part1,part2,part3, h * 21))) %>% tidyr::uncount(V10)
    parts = parts %>% rbind( part )
  }
  parts <- momentums[,1] %>% cbind(parts[ 1 : nrow(momentums), ])
  returns <- c('Date', select_vars(names(layer1[[1]][[1]]), starts_with('Exc', ignore.case = TRUE)))
  
  return (list(layer1[[1]][[1]][,returns], parts))
}

DoubleSort <- function(f=1, h = 1){
  orders <- SecondLayer(f = f, h = h)
  orders[[2]][,-1][, names(orders[[2]][,-1]) := lapply(.SD, as.numeric)]
  for (i in 2:10){
    for (j in 1 : nrow(orders[[2]])-1){
      colidx = 1 + as.numeric(orders[[2]][j,..i])
      orders[[2]][j,i ] = orders[[1]][j+1, colidx]
    }
  }
  orders[[2]] = orders[[2]][-nrow(orders[[2]]),]
  mean0 = function (x) sum(na.omit(x)) * 252 / nrow(x)
  for (i in 2:10){
  m = mean0(orders[[2]][,..i])
  model <- lm(as.matrix(orders[[2]][,..i]) ~ 1)
  p = as.numeric(lmtest::coeftest(model, vcov = sandwich::NeweyWest(model, verbose = T))[1,4])
  print( paste(i,m,p))
  }
  }
DoubleSort(12, 1)
