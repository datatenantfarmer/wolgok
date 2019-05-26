setwd("/Users/chanheelee/Desktop")

out1<-read.csv("non_out.csv", header=T)
out1$time<-time

time<-seq(1,168)
int<-out1$FORECAST-out1$L95
out1$int<-int
out1<-out1[-(1:36),]
library(ggplot2)


clim <- subset(out1, select = c("time", "FORECAST", "int", "spr"))
# ???????????? ???????????? ?????? 
ggplot(clim, aes(x = time, y = FORECAST)) + 
  geom_line(aes(y = spr), linetype="dashed")+
  geom_line(aes(y = FORECAST - int), linetype = "dotted") +
  geom_line(aes(y = FORECAST + int), linetype = "dotted") +
  geom_line()

legend("topright", legend=c("Forecast","Actual Spread","95% confidence interval"), lty=c(1,2,3))

