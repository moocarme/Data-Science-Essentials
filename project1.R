library(xts)
library(TTR)
library(zoo)
library(quantmod)
library(dplyr)
library(tidyr)

# Choose stock company
Nasdaq100_Symbols <- c('AAPL', 'AMZN')
stocks <- getSymbols(Nasdaq100_Symbols)


cols = list()
df <- matrix(data = NA, ncol = 6*length(stocks), nrow = dim(get(stocks[1]))[1])
for(i in 1:length(stocks)){
  in1 <- 6*(i-1)+1
  in2 <- 6*i
  df[,in1:in2] <- get(stocks[i])
  cols <- c(cols, colnames(get(stocks[i])))
}
colnames(df) <- cols
rownames(df)<- as.character.Date(index(get(stocks[1])))
df.dates <- as.data.frame(cbind(as.character.Date(index(get(stocks[1]))), df))
colnames(df.dates)[1] <- 'Date'
head(df.dates)

tidy_stocks <- as.data.frame(df.dates) %>% 
  select(Date, contains('Adjusted')) %>% 
  gather(Index, Price, -Date) 
head(tidy_stocks)

stock_returns <- tidy_stocks %>%
  mutate(Simple.return = ifelse(lag(Index)==Index,(as.numeric(Price)-as.numeric(lag(Price)))/(as.numeric(lag(Price))),0)) %>%
  mutate(Log.return = ifelse(lag(Index)==Index,(log(as.numeric(Price))-log(as.numeric(lag(Price)))),0))
head(stock_returns)

library(ggplot2)
aapl_returns <- stock_returns %>% filter(grepl('AAPL',Index)) %>% select(-Index)
head(aapl_returns)
plot1 <- ggplot(data = aapl_returns, aes(x = Date, y = as.numeric(Price), group = 1))+geom_line()+ geom_point(color="blue")
plot1 + ggtitle('Price over time')
plot2 <- ggplot(data = aapl_returns, aes(x = Date, y = Log.return, group = 1)) + geom_line() 
plot2 + ggtitle('Log return over time')
plot3 <- ggplot(data = aapl_returns, aes(Log.return)) + geom_histogram(bins = 70)
plot3 + ggtitle('Histogram of the Log return')