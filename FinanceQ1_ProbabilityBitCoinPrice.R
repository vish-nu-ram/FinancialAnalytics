library(dplyr)
library(Hmisc)

PriceCSV <- read.csv("R_Feed.csv")
head(PriceCSV)

PriceCSV$days<-PriceCSV$days-1
#Creating a lag value for prices 
PriceCSV$lag <- Lag(PriceCSV$coinbase, +1)
PriceCSV$Return <- ((PriceCSV$coinbase - PriceCSV$lag)/PriceCSV$lag)*100

PriceCSV$cummilativePrices <- PriceCSV$days/366

sortValues<- as.data.frame(sort(PriceCSV$Return))

FinalDF<- PriceCSV[-1,]

FinalDF<- cbind(FinalDF,sortValues$`sort(PriceCSV$Return)`)

colnames(FinalDF)[colnames(FinalDF) == 'sortValues$`sort(PriceCSV$Return)`'] <- 'SortedReturn'
head(FinalDF)

plot(FinalDF$SortedReturn,FinalDF$cummilativePrices)
lines(predict(lo), col='red', lwd=2)

for(i in 1:nrow(FinalDF))
{
  if(FinalDF$SortedReturn[i] >= -9){
    v1<-i
    v2<- i-1
    break;
  }
}

InterpolatedForValue <-approx(c(FinalDF$SortedReturn[v1],FinalDF$SortedReturn[v2]), c(FinalDF$cummilativePrices[v1],FinalDF$cummilativePrices[v2]),xout=-9)
InterpolatedForValue <- InterpolatedForValue$y

probabilityResult <- 1-((1-InterpolatedForValue)^5)
probabilityResult

