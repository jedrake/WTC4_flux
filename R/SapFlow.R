#-----------------------------------------------------------------------------------------------------------
#- Process the HRM and heatpulser sapflow data and compare to the chamber fluxes
#-----------------------------------------------------------------------------------------------------------

library(doBy)
library(colorRamps)
library(plantecophys)
library(reshape)
library(plotBy)
library(HIEv)
library(dplyr)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read and process HRM sapflow data

# read in raw text file
hrmdat1 <- read.csv("data/20161017.TXT",skip=12,stringsAsFactors=F)
hrmdat1 <- hrmdat1[-1,]

#- read in the second file
hrmdat2 <- read.csv("data/20161021.TXT",stringsAsFactors=F)
names(hrmdat2) <- names(hrmdat1)
hrmdat2 <- hrmdat2[-1,]

#- read in the third file
hrmdat3 <- read.csv("data/20161026.TXT",skip=1,stringsAsFactors=F)
names(hrmdat3) <- names(hrmdat1)
hrmdat3 <- hrmdat3[-1,]

#- read in the fourth file
hrmdat4 <- read.csv("data/20161027.TXT",skip=1,stringsAsFactors=F)
names(hrmdat4) <- names(hrmdat1)
hrmdat4 <- hrmdat4[-1,]

#- read in the fifth file
hrmdat5 <- read.csv("data/20161102.TXT",skip=2,stringsAsFactors=F)
names(hrmdat5) <- names(hrmdat1)
hrmdat5 <- hrmdat5[-1,]

#- read in the sixth file
hrmdat6 <- read.csv("data/20161104.TXT",skip=2,stringsAsFactors=F)
names(hrmdat6) <- names(hrmdat1)
hrmdat6 <- hrmdat6[-1,]

#- read in the seventh file
hrmdat7 <- read.csv("data/20161110.TXT",skip=4,stringsAsFactors=F)
names(hrmdat7) <- names(hrmdat1)
hrmdat7 <- hrmdat7[-1,]

#- read in the eigth file
hrmdat8 <- read.csv("data/20161121.TXT",skip=4,stringsAsFactors=F)
names(hrmdat8) <- names(hrmdat1)
hrmdat8 <- hrmdat8[-1,]

hrmdat <- rbind(hrmdat1,hrmdat2,hrmdat3,hrmdat4,hrmdat5,hrmdat6,hrmdat7,hrmdat8)

# remove a bunch of crap
hrmdat[,26] <- NULL
hrmdat <- hrmdat[-1,]
hrmdat <- hrmdat[-1,]
hrmdat <- hrmdat[-1,]
hrmdat <- hrmdat[-1,]
hrmdat <- hrmdat[-1,]
hrmdat <- hrmdat[-1,]

names(hrmdat)[1] <- "DateTime"
toremove <- c(grep(pattern="maximum",hrmdat$DateTime),grep(pattern="minimum",hrmdat$DateTime),
              grep(pattern="average",hrmdat$DateTime))
hrmdat <- hrmdat[-toremove,]

# format the date time variable
hrmdat$DateTime <- as.POSIXct(hrmdat$DateTime,format="%d/%m/%y  %T",tz="GMT")

# convert the rest of the data to numeric
cols.num <- 2:25
hrmdat[cols.num] <- sapply(hrmdat[cols.num],as.numeric)

# remove the deep data
hrmdat2 <- hrmdat[c(1,2,4,6,8,10,12,14,16,18,20,22,24)]

# convert to long format
hrmlong <- melt(hrmdat2,measure.vars=names(hrmdat2)[2:13],variable_name="SensorID")#-- Read in the sensor ID conversion table
names(hrmlong)[3] <- "SFD"


#- add chamber ID's
linkdf <- data.frame(SensorID=levels(hrmlong$SensorID),
                     chamber=c("C06","C03","C04","C01","C10","C07",
                               "C11","C05","C09","C12","C02","C08"))
hrmlong2 <- merge(hrmlong,linkdf)

write.csv(hrmlong2,"WTC_TEMP-PARRA_SAPFLOW-HEATRATIO_20161013-20161121_R.csv",row.names=F)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read and process the heatpulser data
#hp1 <- read.csv("data/WTC4_sapflux_heatpulser_20161027.csv")
#hp2 <- read.csv("data/WTC4_sapflux_heatpulser_20161102.csv")
#p3 <- read.csv("data/WTC4_sapflux_heatpulser_20161104.csv")
hp4 <- read.csv("data/WTC3_sapflux_newest.csv")
linkdf2 <- data.frame(Tree=1:12,
                     chamber=c("C01","C02","C03","C04","C05","C06",
                               "C07","C08","C09","C10","C11","C12"))
hp.all <- hp4
hp.dat <-  merge(hp.all,linkdf2,by="Tree")
hp.dat$chamber <- factor(hp.dat$chamber)
hp.dat$DateTime <- as.POSIXct(hp.dat$DateTime,format="%Y-%m-%d %T",tz="GMT")

#- NA-fill data greater than 175 (this probably could be improved)
hp.dat$sapvel[which(hp.dat$sapvel>175)] <- NA


#- reformat for HIEving
hp.toexport <- hp.dat[,c(26,3:25)]
write.csv(hp.toexport,"WTC_TEMP-PARRA_SAPFLOW-HEATPULSE_20161021-20161121_R.csv")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the water flux data for comparison
wtc <- read.csv("C:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20161028-20161103_L0.csv")

wtc$DateTime <- as.POSIXct(wtc$DateTime,format="%Y-%m-%d %T",tz="GMT")
wtc$VPD <- RHtoVPD(RH=wtc$RH_al,TdegC=wtc$Tair_al)

#wtc.m <- summaryBy(PAR+VPD~DateTime,data=wtc,FUN=mean,keep.names=T)


#- Calculate sapflow rates (cm hr-1) from teh WTC fluxes and the most recent estimate of stem diameter.
#  Diameter data were from 2016-10-12.
diams <- data.frame(chamber=levels(wtc$chamber),
                    diam = c(56.2, 47.7, 48.1, 63.7, 47.8, 61.8, 46.8, 62.1, 49.2, 47.8, 39.0, 63.1))
diams$SA <- pi*((diams$diam/2-5)/10)^2


starttime <- as.POSIXct("2016-10-30 00:00:00",tz="GMT")#min(hrmlong2$DateTime)
#starttime <- as.POSIXct("2016-10-25 00:00:00",tz="GMT")#min(hrmlong2$DateTime)
endtime <- as.POSIXct("2016-11-04 00:00:00",tz="GMT")#min(hrmlong2$DateTime)endtime <- max(hrmlong2$DateTime)

wtc2 <- subset(wtc,DateTime>starttime & DateTime < endtime)

wtc3 <- merge(wtc2,diams,by="chamber")
wtc3$ITE <- with(wtc3,FluxCO2/FluxH2O)
#- calculate SFD_WTC
wtc3$SFD_wtc <- with(wtc3,FluxH2O*3600*18/SA)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- calculate hourly averages of each datastream and merge

#HRM
hrm1 <- subset(hrmlong2,DateTime>starttime & DateTime < endtime & SFD >-10,select=c("DateTime","chamber","SFD"))
hrm1$DateTime_hr <- nearestTimeStep(hrm1$DateTime,nminutes=30,align="floor")

hrm <- data.frame(summarize(group_by(hrm1, DateTime_hr, chamber), 
                                 SFD_hr=mean(SFD,na.rm=T)))

#heatpulser
hp3 <- subset(hp.dat,DateTime>starttime & DateTime < endtime,select=c("DateTime","chamber","sapvel"))
hp3$DateTime_hr <- nearestTimeStep(hp3$DateTime,nminutes=30,align="floor")

hp <- data.frame(summarize(group_by(hp3, DateTime_hr, chamber), 
                            SFD_hp=mean(sapvel,na.rm=T)))


#whole-tree fluxes
wtc4 <- subset(wtc3,DateTime>starttime & DateTime < endtime,select=c("DateTime","chamber","T_treatment","PAR","Tair_al","VPD","SFD_wtc"))
wtc4$DateTime_hr <- nearestTimeStep(wtc4$DateTime,nminutes=30,align="floor")

wtc <- data.frame(summarize(group_by(wtc4, DateTime_hr, chamber,T_treatment), 
                           SFD_wtc=mean(SFD_wtc,na.rm=T),
                           PAR=mean(PAR,na.rm=T),
                           Tair_al=mean(Tair_al,na.rm=T),
                           VPD=mean(VPD,na.rm=T)))


#- merge the three dataframes
dat1 <- merge(hrm,hp,by=c("DateTime_hr","chamber"))
dat2 <- merge(dat1,wtc,by=c("DateTime_hr","chamber"))

linkdf <- data.frame(chamber = levels(as.factor(dat1$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
dat <- merge(dat2,linkdf,by="chamber")
dat$combotrt <- factor(paste(dat$T_treatment,dat$HWtrt,sep="_"))

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------























#-----------------------------------------------------------------------------------------------------------
#- plot Time courses of the environment and sapflux density

dat.met <- summaryBy(PAR+VPD~DateTime_hr,data=dat,FUN=mean,keep.names=T)
dat.flux <- summaryBy(.~DateTime_hr+combotrt,data=dat,FUN=mean,keep.names=T,na.rm=T)
palette(c("blue","black","red","orange"))

windows(60,70)
par(mfrow=c(4,1),mar=c(2,6,1,6),oma=c(2,0,4,0),cex.lab=1.6,las=2)
#- plot PAR and VPD
plot(PAR~DateTime_hr,data=dat.met,type="l",lwd=2,col="black",xlim=c(starttime,endtime))
par(new = T)
plotBy(VPD~DateTime_hr|combotrt, data=dat.flux,type="l",lwd=2, axes=F,legend=F, xlab=NA, ylab=NA,xlim=c(starttime,endtime),
     ylim=c(0,6))
axis(side = 4,ylab="VPD",col="red",col.axis="red")
title(ylab="VPD",col.lab="red",line=-43)
legend(x=starttime-10000,y=8,xpd=NA,legend=levels(dat$combotrt),lwd=2,col=palette()[1:4],ncol=2)

#- plot the three estimates of sap-flux density
plotBy(SFD_wtc~DateTime_hr|combotrt,data=dat.flux,legend=F,type="l",lty=1,lwd=2,ylim=c(0,300),
       xlim=c(starttime,endtime),ylab="SFD_wtc (cm hr-1)")
plotBy(SFD_hp~DateTime_hr|combotrt,data=dat.flux,legend=F,type="l",lwd=2,ylim=c(0,100),
       xlim=c(starttime,endtime),ylab="SFD_hp (cm hr-1)")
plotBy(SFD_hr~DateTime_hr|combotrt,data=dat.flux,legend=F,type="l",lwd=2,ylim=c(0,40),
       xlim=c(starttime,endtime),ylab="SFD_hr (cm hr-1)")

#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- bi-plots of hourly averages
palette(blue2green2red(12))
windows(60,20)
par(mfrow=c(1,3),mar=c(4,6,1,2),oma=c(2,0,4,0),cex.lab=1.6,las=2)
plotBy(SFD_hr~SFD_wtc|chamber,data=dat,pch=16,legendwhere="bottomright");abline(0,1)
plotBy(SFD_hp~SFD_wtc|chamber,data=dat,pch=16,legend=F);abline(0,1)
plotBy(SFD_hr~SFD_hp|chamber,data=dat,pch=16,legend=F);abline(0,1)
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- make a pdf for each chamber
dat.l <- split(dat,dat$chamber)



pdf(file="output/SFD_comparison_2016-10-26.pdf")
par(mfrow=c(2,1),mar=c(2,6,1,6),oma=c(2,0,4,0),cex.lab=1.6,las=2)
for(i in 1:length(dat.l)){
  toplot <- dat.l[[i]]
  
  #- plot PAR and VPD
  plot(PAR~DateTime_hr,data=toplot,type="l",lwd=2,col="black",xlim=c(starttime,endtime))
  par(new = T)
  plot(VPD~DateTime_hr, data=toplot,type="l",lwd=2,col="red", axes=F, xlab=NA, ylab=NA,xlim=c(starttime,endtime),
       ylim=c(0,6))
  axis(side = 4,ylab="VPD",col="red",col.axis="red")
  title(ylab="VPD",col.lab="red",line=-26)
  legend("topleft",legend=paste("Chamber",toplot$chamber[1],sep="= "),bty="n")
  #- plot the three estimates of sap-flux density
  plot(SFD_wtc~DateTime_hr,data=toplot,legend=F,type="l",lty=1,col="black",lwd=2,ylim=c(0,300),
         xlim=c(starttime,endtime),ylab="SFD (cm hr-1)")
  lines(SFD_hp~DateTime_hr,data=toplot,legend=F,type="l",col="red",lwd=2)
  lines(SFD_hr~DateTime_hr,data=toplot,legend=F,type="l",col="blue",lwd=2)
  legend("topright",legend=c("WTC","HP","HR"),lwd=2,col=c("black","red","blue"))
}
dev.off()
#-----------------------------------------------------------------------------------------------------------



write.csv(dat,"output/Sapflow_20161104.csv",row.names=F)

