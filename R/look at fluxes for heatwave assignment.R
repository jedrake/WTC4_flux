library(plantecophys)
library(doBy)
library(plotBy)
library(HIEv)


se <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}


adderrorbars <- function(x,y,SE,direction,barlen=0.04,...){
  
  if(length(direction)>1)stop("direction must be of length one.")
  if(direction == "updown")
    direction <- c("up","down")
  else if(direction == "rightleft" | direction == "leftright")direction <- c("left","right")
  
  if("up" %in% direction)
    arrows(x0=x, x1=x, y0=y, y1=y+SE, code=3, angle=90, length=barlen,...)
  if("down" %in% direction) 
    arrows(x0=x, x1=x, y0=y, y1=y-SE, code=3, angle=90, length=barlen,...)
  if("left" %in% direction) 
    arrows(x0=x, x1=x-SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)
  if("right" %in% direction)
    arrows(x0=x, x1=x+SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)  
  
}


#-----------------------------------------------------------------------------------------------------------
#- Plot the treatment averages prior to the heatwavw
wtc1 <- read.csv("C:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20161028-20161115_L0.csv")

wtc1$DateTime <- as.POSIXct(wtc1$DateTime,format="%Y-%m-%d %T",tz="GMT")
wtc1$VPD <- RHtoVPD(RH=wtc1$RH_al,TdegC=wtc1$Tair_al)

starttime <- as.POSIXct("2016-10-29 00:00:00",tz="GMT")
wtc <- subset(wtc1,DateTime>starttime & DoorCnt == 0)

#-- create hourly averages for fluxes
wtc$DateTime_hr <- nearestTimeStep(wtc$DateTime,nminutes=60,align="floor")

#- average PAR, VPD, FLuxCO2 and FLuxH2O for plotting
wtc.m <- summaryBy(PAR+VPD+Tair_al+FluxCO2+FluxH2O~DateTime_hr+chamber+T_treatment,data=subset(wtc,DoorCnt==0),
                   FUN=mean,keep.names=T)


#- merge in the heatwave treatments
linkdf <- data.frame(chamber = levels(as.factor(wtc$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

wtc.m <- merge(wtc.m,linkdf,by="chamber")
wtc.m$combotrt <- factor(paste(wtc.m$T_treatment,wtc.m$HWtrt,sep="_"))


#- average and SEs for each treatment
wtc.m2 <- summaryBy(.~DateTime_hr+combotrt,data=subset(wtc.m,as.Date(DateTime_hr) < as.Date("2016-11-06")),FUN=c(mean,se))

windows(60,60)
par(mfrow=c(4,1),mar=c(2,6,1,6),oma=c(0,0,4,0),cex.lab=1.6)
palette(c("blue","black","red","orange"))
#-----------------------------------------------------------------------------------------------------------
#- plot Time courses


#- plot PAR and VPD
plot(PAR.mean~DateTime_hr,data=wtc.m2,type="l",lwd=2,lty=2,col="grey",ylab="PPFD")
par(new = T)
plotBy(Tair_al.mean~DateTime_hr|combotrt, data=wtc.m2,type="l",lwd=2, axes=F, xlab=NA, ylab=NA,
     ylim=c(5,45),legend=F)
axis(side = 4,ylab="Tair",col="red",col.axis="red")
legend(x=starttime-10000,y=64,xpd=NA,legend=levels(wtc.m2$combotrt),lwd=2,col=palette()[1:4],ncol=2,cex=1.5,bty="n")
title(ylab="Tair",col="red",xpd=NA,line=-50,col="red")


plotBy(VPD.mean~DateTime_hr|combotrt, data=wtc.m2,type="l",lwd=2, legend=F,ylab="VPD",
     ylim=c(0,6))
axis(side = 4,ylab="",col="black",col.axis="black")

plotBy(FluxCO2.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(-0.05,0.3),ylab="Flux CO2");abline(h=0)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$FluxCO2.mean,SE=wtc.m2$FluxCO2.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
plotBy(FluxH2O.mean~DateTime_hr|combotrt,data=wtc.m2,legend=F,type="l",lty=1,lwd=3,ylim=c(0,0.06),ylab="Flux H2O");abline(h=0)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m2$DateTime_hr,y=wtc.m2$FluxH2O.mean,SE=wtc.m2$FluxH2O.se,
             direction="updown",col=wtc.m2$combotrt,barlen=0,lwd=0.5)
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- plot a subset of the flux data, for a media release
windows();par(mar=c(5,7,3,1))
palette(c("blue","red"))

wtc.m3 <- subset(wtc.m2,combotrt %in% c("elevated_C","elevated_HW") & as.Date(DateTime_hr) %in% as.Date(c("2016-11-3","2016-11-4")))
plotBy(FluxCO2.mean~DateTime_hr|combotrt,data=wtc.m3,legend=F,type="l",lty=1,lwd=3,ylim=c(-0.06,0.3),ylab="Net photosynthesis",xlab="",cex.lab=2.5,
       panel.first=shadeNight(wtc.m3$DateTime_hr));abline(h=0)
axis(side = 4,ylab="",col="black",col.axis="black")
adderrorbars(x=wtc.m3$DateTime_hr,y=wtc.m3$FluxCO2.mean,SE=wtc.m3$FluxCO2.se,
             direction="updown",col=wtc.m3$combotrt,barlen=0,lwd=0.5)
legend("bottomleft",legend=c("Control","Heatwave"),lty=1,lwd=3,col=c("blue","red"),bg="white")
abline(v=as.POSIXct("2016-11-04 00:00:00",tz="GMT"))
text(x=as.POSIXct("2016-11-03 10:30:00",tz="GMT"),y=0.33,labels="Last day of heatwave",cex=1.5,xpd=NA)
text(x=as.POSIXct("2016-11-04 11:30:00",tz="GMT"),y=0.33,labels="Equivalent conditions",cex=1.5,xpd=NA)

#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- plot the temperature dependence of respiration, with time as colors, for each chamber
windows(60,40)
par(mfrow=c(3,4),mar=c(0,0,0,0))
wtc.l <- split(wtc.m,wtc.m$chamber)
palette(c("blue","blue","red","red","red","red","black","black","black"))
for(i in 1:length(wtc.l)){
  dat <- subset(wtc.l[[i]],PAR<4)
  plotBy(FluxCO2~Tair_al|factor(as.Date(DateTime_hr)),data=dat,ylim=c(-0.04,0),xlim=c(5,35),legend=F)
  legend("bottom",legend=dat$chamber[1],bty="n")
  abline(h=0)
}
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
# read in the porometer data, measured by john and angelica on 3 Nov 2016
pordat1 <- read.csv("data/WTC_TEMP-PARRA_CM_GX-PORO_20161103.csv")
pordat1$poro <- rowMeans(pordat1[,c("poro1","poro2","poro3")])
pordat1$combotrt <- factor(paste(pordat1$T_treatment,pordat1$HW_treatment))
windows(40,20)
boxplot(poro~combotrt,data=pordat1,ylab="gs (mmol m-2 s-1)")
boxplot(poro~HW_treatment,data=pordat1,ylab="gs (mmol m-2 s-1)")

#-----------------------------------------------------------------------------------------------------------
