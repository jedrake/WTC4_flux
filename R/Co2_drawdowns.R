#----------------------------------------------------------------------
#- Download and plot the CO2 drawdowns we did in chambers 7-12 on 25 July 2016
#----------------------------------------------------------------------

source("R/loadLibraries.R")
library(RColorBrewer)

d <- downloadTOA5("WTC_AUTO_ALL_CH01MIN_R_20160731.dat")

starttime <- as.POSIXct("2016-07-25 10:30:00", tz="UTC")
endtime <- as.POSIXct("2016-07-25 20:30:00", tz="UTC")

dat <- subset(d,DateTime>starttime & DateTime < endtime)

windows(50,80);par(mfrow=c(2,1),mar=c(5,6,1,1))
palette(c("black",brewer.pal(11,"Spectral")))
plotBy(CO2L~DateTime|chamber,data=dat,type="o",legend=F,ylim=c(0,3000),ylab="Local [CO2]")
legend("topright",legend=1:12,pch=16,col=palette()[1:12],ncol=2,title="Chamber")
plotBy(CO2CChamb~DateTime|chamber,data=dat,type="o",legend=F,ylim=c(0,3000),ylab="Central [CO2]")
