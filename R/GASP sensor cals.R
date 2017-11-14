#-----------------------------------------------------------------------------------------------------------
#- Evaluate the GASP sensors, based on a measurement of PAR.
#  Craig and John set up 6 GASP sensors and a good PAR sensor on a CR1000 in the open field adjacent
#   To the WTCs on Friday, June 24, 2016. The simply logged data and let PAR vary
#-----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")
library(HIEv)
library(reshape2)
library(plotBy)

#-----------------------------------------------------------------------------------------------------------
#- read in the data
dat.raw <- readTOA5("data/CR1000_com1_Table1_GASP_test.dat")
dat <- subset(dat.raw,DateTime>as.POSIXct("2016-06-24 12:20:00", tz="UTC"))

plot(DiffVolt_Avg.4.~DateTime,data=dat)

# best ones: 2, 6, 4, 5

dat2 <- dat[,c(1,5,7,8,9,11)]
names(dat2) <- c("DateTime","G3","G5","G6","G7","PAR")

dat.long <- melt(dat2,id=c("DateTime","PAR"))
dat.long$logvalue<- sqrt(dat.long$value)

#- it looks like we need a second-order polynomial for each sensor

plotBy(value~PAR|variable,data=dat.long,type="p",legend=T,ylab="GASP voltage",xlab="PAR (umol m-2 s-1)")
plotBy(PAR~DateTime|variable,data=dat.long,type="l",legend=F)


plotBy(PAR~value|variable,data=dat.long)
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- fit second order polynomial for each sensor (variable)
lm1 <- lm(PAR~0+value+I(value^2),data=subset(dat.long,variable=="G3"))
lm2 <- lm(PAR~0+value+I(value^2),data=subset(dat.long,variable=="G5"))
lm3 <- lm(PAR~0+value+I(value^2),data=subset(dat.long,variable=="G6"))
lm4 <- lm(PAR~0+value+I(value^2),data=subset(dat.long,variable=="G7"))

newdat <- expand.grid(variable=levels(dat.long$variable),value=seq(from=0,to=3,length.out=101))
newdat$pred <- predict(lm1,newdata=newdat)

plotBy(pred~value|variable,data=newdat)
#-----------------------------------------------------------------------------------------------------------
