
library(data.table)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)

myEnv <- new.env()

tickers <- c("SPY","DIA","ONEQ","WFIVX","IWV")


for (h in tickers){
  getSymbols(h, periodicity = 'monthly', env = myEnv, from = "2009-01-01")
  
}

plist <- eapply(myEnv, Ad)
pframe <- do.call(merge, plist)
rtnXts <- pframe


for (i in 1:ncol(pframe)){
  rtnXts[,i] <- diff(log(pframe[,i]))
}

rtnTable <-data.table(date=index(rtnXts), coredata(rtnXts))
rtnTable$Idx <- rev(index(rtnTable))


etfRtn <- melt(rtnTable,id.vars= c("date","Idx"),variable.name="ETF",
               value.names = "Return",na.rm=TRUE)

etfRtn$RSI <- 0 * etfRtn$Return
etfRtn$Position <- 0 * etfRtn$Return


funds <- names(rtnTable)


#Calculate the position
for (i in 12:(nrow(etfRtn))){
  
  #Mean of positive and negative returns of the previous 12 months
  arrayN <- etfRtn[(i-12):(i-1)]
  arrayN
  
  meanNeg <- mean(arrayN$value[arrayN$value < 0])
  meanPos <- mean(arrayN$value[arrayN$value >= 0])
  
  RSI <- 100 - (100/(1+(meanPos/(-meanNeg))))
  etfRtn$RSI[i] <- RSI
  if (RSI >= 70){
    etfRtn$Position[i] <- -1
  } else if (RSI <= 30){
    etfRtn$Position[i] <- 1
  } else {
    etfRtn$Position[i] <- 0
  }
}

#Fix some things
fstRtn <- findPeaks(etfRtn$Idx)
rem <- c()


for (k in fstRtn){
  rem <- c(rem,(k-1):(k+9))
}

final <- etfRtn[-rem] 
final <- final[12:nrow(final)]

fstDay <- findValleys(final$Idx)

for (z in fstDay){
  final$value[z] <- 0 
}
 

n <- nrow(final)

#calculate the returns of the month based off the previous indicator
Returns<- data.table(final$date[2:n], final$value[2:n]* final$Position[1:n-1])
setnames(Returns, "V1", "date")
setnames(Returns, "V2", "Return")

#Get the total return for each day
Returns$date <- as.Date(Returns$date) 
monthRet <- aggregate(Returns$Return, by = list(Returns$date), sum)
setnames(monthRet, "Group.1", "date")
setnames(monthRet, "x", "Return")
monthRet$date <- as.Date(monthRet$date)

#Plots

plot(monthRet$date, monthRet$Return, type = "l", lty = 1)
mean(monthRet$Return)
kurtosis(monthRet$Return)
skewness(monthRet$Return)

mRet <- xts(monthRet[,-1], order.by=monthRet[,1])

#monthRet$Date <- as.Date(monthRet$Date)
charts.PerformanceSummary( mRet,cex.legend=0.45,colorset-rich10equal,geometric=TRUE, main="IWV-ONEQ-WFIVX-DIA-SPY Performance")
