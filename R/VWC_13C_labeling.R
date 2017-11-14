
source("R/loadLibraries.R")


startDate <- as.Date("2016-08-01")
endDate <- as.Date("2016-09-01")

#-----------------------------------------------------------------------------------------------------------
#- download the soil volumetric water content data from HIEv, for WTC4

files <- searchHIEv("WTC_AUTO_C[0-9]{1,2}_SOILVARS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{1,2}_SOILVARS", startDate=startDate, endDate=endDate,
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",maxnfiles=100,
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_VWC.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- as.factor(ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient"))
d$Source <- d$RECORD <- NULL
#d$VW_Avg <- rowMeans(d[,c("VW_Avg.1.","VW_Avg.2.","VW_Avg.3.")])

#- get the extra TDR data too
XTRAfiles <- searchHIEv("WTC_AUTO_C[0-9]{1,2}_XTRATDR")
d2 <- downloadTOA5("WTC_AUTO_C[0-9]{1,2}_XTRATDR", startDate=startDate, endDate=endDate,
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",maxnfiles=100,
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_XTRAVWC.rdata")
d2$chamber <- as.factor(substr(d2$Source,start=10,stop=12)) # extract the chamber number from the filename
d2$T_treatment <- as.factor(ifelse(as.numeric(substr(as.character(d2$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient"))
d2$Source <-d2$RECORD <- NULL

soildat <- merge(d,d2,by=c("Date","DateTime","chamber","T_treatment"),all.x=T)
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- Do some data manipulation
soildat$VW_surface <- rowMeans(soildat[,c("VW_Avg.1.","VW2_Avg.1.","VW2_Avg.2.")],na.rm=F)

#- remove all the bad soil temperature data...
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#calculate daily means, then treatment means
dfr.day <- summaryBy(.~Date+chamber+T_treatment,data=soildat,FUN=mean,keep.names=T,na.rm=T)


#- merge in the heatwave treatments
linkdf <- data.frame(chamber = levels(as.factor(soildat$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
dfr.day2 <- merge(dfr.day,linkdf,by="chamber")
dfr.day2$combotrt <- factor(paste(dfr.day2$T_treatment,dfr.day2$HWtrt,sep="_"))


dfr.day.treat <- summaryBy(.~Date+T_treatment,data=subset(dfr.day2,chamber !="C11"), # remove C11, as it's soil temperature data are crazy!
                           FUN=c(mean,se),keep.names=T,na.rm=T)
dfr.day.a <- subset(dfr.day.treat,T_treatment=="ambient")
dfr.day.e <- subset(dfr.day.treat,T_treatment=="elevated")


#- plot timecourse for treatments
windows(80,60);par(mfrow=c(2,1),cex.lab=1.5,mar=c(6,6,1,1))

palette(c("black",brewer.pal(11,"Spectral")))

#- Single surface sensor
plotBy(VW_Avg.1..mean~Date|T_treatment,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),col=c("blue","red"),
       ylab="VWC- single surface",lwd=3)
adderrorbars(x=combotrt$Date,y=combotrt$VW_Avg.1..mean,SE=combotrt$VW_Avg.1..standard.error,
             direction="updown",col="blue",barlen=0)
#adderrorbars(x=dfr.day.e$Date,y=dfr.day.e$VW_Avg.1..mean,SE=dfr.day.e$VW_Avg.1..standard.error,
#             direction="updown",col="red",barlen=0)
legend("top",lwd=3,lty=1,legend=levels(dfr.day$T_treatment),col=c("blue","red"),ncol=2)





windows(80,70);par(mfrow=c(3,1),cex.lab=1.5,cex.axis=1.5,mar=c(6,6,1,1))
palette(c("blue","black","red","orange"))

#- average of three surface sensors
plotBy(VW_surface.mean~Date|T_treatment,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),col=c("blue","red"),
       ylab="VWC- three surface",lwd=3)
legend("topleft",legend=levels(dfr.day.treat$T_treatment),lty=1,lwd=2,ncol=2,col=c("blue","red"))
adderrorbars(x=dfr.day.a$Date,y=dfr.day.a$VW_surface.mean,SE=dfr.day.a$VW_surface.se,
             direction="updown",col="blue",barlen=0)
adderrorbars(x=dfr.day.e$Date,y=dfr.day.e$VW_surface.mean,SE=dfr.day.e$VW_surface.se,
             direction="updown",col="red",barlen=0)



#- mid depth
plotBy(VW_Avg.2..mean~Date|T_treatment,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),col=c("blue","red"),
       ylab="VWC- mid",lwd=3)
adderrorbars(x=dfr.day.a$Date,y=dfr.day.a$VW_Avg.2..mean,SE=dfr.day.a$VW_Avg.2..se,
             direction="updown",col="blue",barlen=0)
adderrorbars(x=dfr.day.e$Date,y=dfr.day.e$VW_Avg.2..mean,SE=dfr.day.e$VW_Avg.2..se,
             direction="updown",col="red",barlen=0)

#- deep
plotBy(VW_Avg.3..mean~Date|T_treatment,data=dfr.day.treat,type="l",legend=F,ylim=c(0,0.3),col=c("blue","red"),
       ylab="VWC- deep",lwd=3)
adderrorbars(x=dfr.day.a$Date,y=dfr.day.a$VW_Avg.3..mean,SE=dfr.day.a$VW_Avg.3..se,
             direction="updown",col="blue",barlen=0)
adderrorbars(x=dfr.day.e$Date,y=dfr.day.e$VW_Avg.3..mean,SE=dfr.day.e$VW_Avg.3..se,
             direction="updown",col="red",barlen=0)


#-----------------------------------------------------------------------------------------------------------


#- look at temperature data
#- daily chamber means
plotBy(SoilTempProbe_Avg.1.~Date|chamber,data=dfr.day,type="o",legend=F)#,ylim=c(0,35))
plotBy(SoilTempProbe_Avg.1.~Date|T_treatment,data=dfr.day,type="o",legend=F,ylim=c(0,35))

legend("top",pch=16,lty=1,legend=levels(dfr.day$chamber),col=palette()[1:12],ncol=6)

#- daily treatment means
windows(80,100);par(mfrow=c(5,1),cex.lab=1.5,cex.axis=1.5,mar=c(6,6,1,1),oma=c(0,4,0,0))
ylims=c(10,17)
plotBy(SoilTempProbe_Avg.1..mean~Date|T_treatment,col=c("blue","red"),data=dfr.day.treat,type="o",ylim=ylims,
       panel.first=adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$SoilTempProbe_Avg.1..mean,
                                SE=dfr.day.treat$SoilTempProbe_Avg.1..se,
                                direction="updown",col="blue",barlen=0))
plotBy(SoilTempProbe_Avg.2..mean~Date|T_treatment,col=c("blue","red"),data=dfr.day.treat,type="o",ylim=ylims,
       panel.first=adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$SoilTempProbe_Avg.2..mean,
                                SE=dfr.day.treat$SoilTempProbe_Avg.2..se,
                                direction="updown",col="blue",barlen=0))
plotBy(SoilTempProbe_Avg.3..mean~Date|T_treatment,col=c("blue","red"),data=dfr.day.treat,type="o",ylim=ylims,
       panel.first=adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$SoilTempProbe_Avg.3..mean,
                                SE=dfr.day.treat$SoilTempProbe_Avg.3..se,
                                direction="updown",col="blue",barlen=0))
plotBy(SoilTempProbe_Avg.4..mean~Date|T_treatment,col=c("blue","red"),data=dfr.day.treat,type="o",ylim=ylims,
       panel.first=adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$SoilTempProbe_Avg.4..mean,
                                SE=dfr.day.treat$SoilTempProbe_Avg.4..se,
                                direction="updown",col="blue",barlen=0))
plotBy(SoilTempProbe_Avg.5..mean~Date|T_treatment,col=c("blue","red"),data=dfr.day.treat,type="o",ylim=ylims,
       panel.first=adderrorbars(x=dfr.day.treat$Date,y=dfr.day.treat$SoilTempProbe_Avg.5..mean,
                                SE=dfr.day.treat$SoilTempProbe_Avg.5..se,
                                direction="updown",col="blue",barlen=0))
