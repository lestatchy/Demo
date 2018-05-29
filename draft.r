Dates = Strgy$Date
# colnames(BootstrapData) = c('start','end','# of weeks','gain')
# BootstrapData$start = as.Date(BootstrapData$start)
# BootstrapData$end = as.Date(BootstrapData$end)
L = length(Dates)
BootstrapData = data.frame(start = Dates[1],end = Dates[1],weeks = 0, gain = 0,Q5 = 0, Q95 = 0, sd = 0, sr = 0)


for (i in 1:(L-52)) {
  start = Dates[i]
  for (j in (i+52):L) {
    end = Dates[j]
    dur = as.numeric(difftime(end,start,units = "weeks"))
    tmp = Gains(as.Date(start),as.Date(end))
    m = mean(tmp$Gain)
    v = sd(tmp$Gain)/sqrt(52)
    qt = quantile(tmp$Gain,c(0.05,0.95))
    Row = data.frame(start = start, end = end, weeks = dur, gain = m, Q5 = qt[1], Q95 = qt[2],sd = v, sr = m/v)
    BootstrapData = rbind(BootstrapData,Row)
  }
}
colnames(BootstrapData) = c('start','end','# of weeks','gain','Percent5','Percent95','std.dev','Sharpe')
BootstrapData = BootstrapData[-1,]
saveRDS(BootstrapData,file = 'BootstrapData.rds')
BootstrapData = readRDS("BootstrapData.rds")

# NoWeek = unique(BootstrapData$`# of weeks`)


library(data.table)
library(plyr)
DT <- data.table(BootstrapData)
colnames(DT)[3] = "weeks"
a = rev(count(DT,"weeks"))

x1 = DT[, mean(gain), by = weeks]
x2 = DT[, mean(std.dev), by = weeks]
# 
x3 = DT[, mean(Percent5), by = weeks]
x4 = DT[, mean(Percent95), by = weeks]
x5 = DT[,mean(Sharpe),by = weeks]
Lo = x1$V1-x2$V1
Hi = x1$V1+x2$V1
# `5%` = x3$V1
# `95%` = x4$V1

Dat1 = cbind(x1,Lo)
Dat1 = cbind(Dat1,Hi)
Dat1 = cbind(Dat1,x3$V1)
Dat1 = cbind(Dat1,x4$V1)
Dat1 = cbind(Dat1,x5$V1)
# Dat1 = cbind(x1,`5%`)
# Dat1 = cbind(Dat1,`95%`)
colnames(Dat1)[2:7] = c("mean","low","high","qt5","qt95","sharpe")
saveRDS(Dat1,"trendData.rds")

library("ggplot2")

ggplot(BootstrapData, aes(x=gain)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20) +
  xlab("Distribution of Weekly Gain") +
  geom_vline(aes(xintercept=mean(gain, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  geom_density(alpha=.2, fill="#FF6666")


ggplot(data=Dat1, aes(x=weeks, y=mean, ymin=low, ymax=high, fill='blue'))+ 
  geom_line() + geom_ribbon(alpha=.5)


maxs = data.frame(Date = testData$Date[1],Max = 0)
mins = data.frame(Date = testData$Date[1],Min = 0)

begin = as.Date("2010-1-1")
testData = Strgy[as.Date(Strgy$Date)>begin,]
testData$TR = testData$TR/testData$TR[1]
L = nrow(testData)
max = testData$TR[1]
min = max
for(i in 1:(L-1)){
  temp = testData$TR[i]
  forward = testData$TR[i+1]
  if(temp > forward & temp > max){
    max = temp
    newrowmax = data.frame(Date = testData$Date[i],Max = max)
    maxs = rbind(maxs,newrowmax)
    min = max
  }else if(temp < forward & temp < min){
    min = temp
    newrowmin = data.frame(Date = testData$Date[i],Min = min)
    mins = rbind(mins,newrowmin)
  }
}
maxs = maxs[-1,]
mins = mins[-1,]
drawndowns = data.frame(Date = testData$Date[1],TR = 0, DD = 0)
LL = nrow(maxs)
for(i in 2:LL){
  t1 = maxs$Date[i-1]
  t2 = maxs$Date[i]
  x = testData[testData$Date>t1 & testData$Date<t2,]
  min = min(x$TR)
  temp = x[x$TR == min,]
  newrow = data.frame(Date = temp$Date,TR = temp$TR,DD = (min-maxs$Max[i])/maxs$Max[i])
  drawndowns = rbind(drawndowns,newrow)
}
drawndowns = drawndowns[-1,]


ggplot(data=testData, aes(x=Date, y=TR)) +
  geom_line(color="red")+
  geom_point(data=maxs, aes(x=Date, y=Max), colour="green", size=1)+
  geom_point(data=drawndowns, aes(x=Date, y=TR), colour="red", size=1)


ggplot(data = drawndowns, aes(x=DD))+
  geom_histogram(colour="black", fill="white", bins = 10)




# 
# x = Strgy
# x$TR = x$TR/x$TR[1]
# gg = ggplot(data=x, aes(x=Date, y=TR)) +
#   geom_line(color="red")+
#   geom_point()
# ggplotly(gg)
# 
# , test = paste('Date: ',as.Date(Date),'<br>Value: ', TR)