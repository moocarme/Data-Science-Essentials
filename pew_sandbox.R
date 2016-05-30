library(foreign)
trade <- read.spss('data/Pew_trade.sav', to.data.frame = T)
str(trade)