Dates = Strgy$Date
# colnames(BootstrapData) = c('start','end','# of weeks','gain')
# BootstrapData$start = as.Date(BootstrapData$start)
# BootstrapData$end = as.Date(BootstrapData$end)
L = length(Dates)
BootstrapData = data.frame(start = Dates[1],end = Dates[1],weeks = 0, gain = 0)


for (i in 1:(L-52)) {
  start = Dates[i]
  for (j in (i+52):L) {
    end = Dates[j]
    dur = as.numeric(difftime(end,start,units = "weeks"))
    tmp = Gains(as.Date(start),as.Date(end))
    Row = data.frame(start = start, end = end, weeks = dur, gain = mean(tmp$Gain))
    BootstrapData = rbind(BootstrapData,Row)
  }
}
colnames(BootstrapData) = c('start','end','# of weeks','gain')
BootstrapData = BootstrapData[-1,]
saveRDS(BootstrapData,file = 'BootstrapData.rds')
BootstrapData = readRDS("BootstrapData.rds")
BootstrapData$gain = BootstrapData$gain*5200
# NoWeek = unique(BootstrapData$`# of weeks`)


library(data.table)
library(plyr)
DT <- data.table(BootstrapData)
colnames(DT)[3] = "weeks"
a = rev(count(DT,"weeks"))

x1 = DT[, mean(gain), by = weeks]
x2 = DT[, sd(gain), by = weeks]
# 
# x3 = DT[, quantile(gain,probs = seq(0, 1, 0.05))[2], by = `# of weeks`]
# x4 = DT[, quantile(gain,probs = seq(0, 1, 0.05))[20], by = `# of weeks`]
Lo = x1$V1-3*x2$V1
Hi = x1$V1+3*x2$V1
# `5%` = x3$V1
# `95%` = x4$V1

Dat1 = cbind(x1,Lo)
Dat1 = cbind(Dat1,Hi)
# Dat1 = cbind(x1,`5%`)
# Dat1 = cbind(Dat1,`95%`)
colnames(Dat1)[2:4] = c("mean","low","high")
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


