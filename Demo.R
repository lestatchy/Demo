

# library("data.table")

library(readxl)
Cash <- read_excel("UST10y_update2.xlsx",sheet = "Cash")
Swap <- read_excel("UST10y_update2.xlsx",sheet = "Swap", skip = 1)
Libor <- read_excel("UST10y_update2.xlsx",sheet = "Libor 1W")
Tsy <- read_excel("UST10y_update2.xlsx",sheet = "Tsy")
Pos <- read_excel("UST10y_update2.xlsx",sheet = "Replication", range = "A10:B979")
Pos[1,1] = as.Date("1999-01-05")
Dat = Reduce(function(x, y) merge(x, y, by="Date"), list(Cash,Swap,Libor,Tsy))
colnames(Dat) = c("Date","Cash","Swap","Libor","Tsy")
Dat = merge(x = Pos, y = Dat, by = "Date", all.x = TRUE)
colnames(Dat)[2] = "Position"
library(TTR)
# HistorcalAvg = cumsum(Dat$Position) / seq_along(Dat$Position)
HistorcalAvg = runMean(Dat$Position, cumulative = TRUE)
HistorcalSTD = runSD(Dat$Position, cumulative = TRUE)
Dat = cbind(Dat,HistorcalAvg)
Dat = cbind(Dat,HistorcalSTD)
UB = Dat$HistorcalAvg+Dat$HistorcalSTD
LB = Dat$HistorcalAvg-Dat$HistorcalSTD
Dat = cbind(Dat,UB)
Dat = cbind(Dat,LB)
saveRDS(Dat,file = 'Dat.rds')
OutSample = Dat[Dat$Date>"2009-01-01",]
Strgy = data.frame(Date = OutSample$Date, Position = rep(0,nrow(OutSample)),Gain = rep(0,nrow(OutSample)),TR = rep(1,nrow(OutSample)) )
for (i in seq(1,nrow(Strgy)-1)) {
  x = OutSample$Position[i]
  lb = OutSample$LB[i]
  ub = OutSample$UB[i]
  pos = ifelse(x<lb,1,ifelse(x>ub,-1,Strgy$Position[i]))
  gain = -pos*OutSample$Libor[i]/10000+pos*(OutSample$Swap[i+1]/OutSample$Swap[i]-1)
  totalRet = Strgy$TR[i]*(1+gain)
  Strgy$Position[i+1] = pos
  Strgy$Gain[i+1] = gain
  Strgy$TR[i+1] = totalRet
}

Gains <- function(start,end){
  # start = "2009-01-01"
  # end = "2010-01-01"
  OutSample = Dat[as.Date(Dat$Date)>=start & as.Date(Dat$Date)<=end,]
  Strgy = data.frame(Date = OutSample$Date, Position = rep(0,nrow(OutSample)),Gain = rep(0,nrow(OutSample)),TR = rep(1,nrow(OutSample)) )
  for (i in seq(1,nrow(Strgy)-1)) {
    x = OutSample$Position[i]
    lb = OutSample$LB[i]
    ub = OutSample$UB[i]
    pos = ifelse(x<lb,1,ifelse(x>ub,-1,Strgy$Position[i]))
    gain = -pos*OutSample$Libor[i]/10000+pos*(OutSample$Swap[i+1]/OutSample$Swap[i]-1)
    totalRet = Strgy$TR[i]*(1+gain)
    Strgy$Position[i+1] = pos
    Strgy$Gain[i+1] = gain*52*100
    Strgy$TR[i+1] = totalRet
  }
  return(Strgy)
}

