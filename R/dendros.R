
source("R/loadLibraries.R")
library(RColorBrewer)
library(readxl)



#-----------------------------------------------------------------------------------------------------------
#- download the WTC4 dendrometer from HIEv

files <- searchHIEv("WTC_AUTO_C[0-9]{1,2}_DENDRO")
d1 <- downloadTOA5("WTC_AUTO_C[0-9]{1,2}_DENDRO", startDate="2016-10-28", endDate="2016-11-30",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",maxnfiles=100,
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_DENDRO.rdata")
d1$chamber <- as.factor(substr(d1$Source,start=10,stop=12)) # extract the chamber number from the filename
d1$T_treatment <- as.factor(ifelse(as.numeric(substr(as.character(d1$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient"))
d1$Source <- d1$RECORD <- NULL

d1 <- subset(d1,DateTime > as.POSIXct("2016-07-14 16:30:00",tz="UTC"))

#- find most recent diam data for C01 and C10 to reset the dendros.
d2 <- subset(d1,DateTime > as.POSIXct("2016-9-25 00:00:00",tz="UTC") & chamber %in% c("C01",'C10'))

#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- get the hand-measured data at 65 cm
d65_1 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/65cm Dia data/WTC4_65cm_dia_20160713_measure_5.xlsx",
                    skip=2)
d65_1$Date <- as.Date("2016-07-13")
d65_2 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/65cm Dia data/WTC4_65cm_dia_20160727_measure_6.xlsx",
                    skip=2)
d65_2$Date <- as.Date("2016-07-27")
d65_3 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/65cm Dia data/WTC4_65cm_dia_20160810_measure_7.xlsx",
                    skip=2)
d65_3$Date <- as.Date("2016-08-10")
d65_3$Difference <- NULL
names(d65_3) <- names(d65_2)
#- add chamber in the correct format
growth_hand <- rbind(d65_1,d65_2,d65_3)
growth_hand[,4] <- NULL
names(growth_hand)[3] <- "diam"
growth_hand$chamber <- as.factor(paste0("C", sprintf("%02.0f", growth_hand$Ch_No)))

#- get the hand measured data from the other measurements, which use all of the heights
dall_1 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/WTC4_Ground_21_20160720.xlsx",
                    skip=2)
dall_1$Date <- as.Date("2016-07-20")
dall_2 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/WTC4_Ground_22_20160802.xlsx",
                     skip=2)
dall_2$Date <- as.Date("2016-8-2")

growth_hand2 <- rbind(dall_1[,c("Ch_No","Pot_No","65","Date")],dall_2[,c("Ch_No","Pot_No","65","Date")])

#- add chamber in the correct format
names(growth_hand2)[3] <- "diam"
growth_hand2$chamber <- as.factor(paste0("C", sprintf("%02.0f", growth_hand2$Ch_No)))

#- merge both types of manual growth measurements
growth1 <- rbind(growth_hand2,growth_hand)
growth <-  summaryBy(diam~chamber+Date,FUN=mean,na.rm=T,data=growth1,keep.names=T)
#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
#- remove all negative data, normalize to the maximum for each chamber
d <- subset(d1,Dendro_Avg>20)


pdf(file="output/WTC4_dendros_eachChamber02112016.pdf")
#- break into a list, plot each chamber separately for Sebastian
d.l <- split(d,d$chamber)
#d.hand.l <- split(growth,growth$chamber)

for (i in 1:length(d.l)){
  initval <- d.l[[i]]$Dendro_Avg[1]
  d.l[[i]]$Dendro_Diff <- with(d.l[[i]],Dendro_Avg-initval)
  
  #- remove any strongly negative values (C03)
  #d.l[[i]] <- subset(d.l[[i]],Dendro_Diff>-0.5)
  
  plotBy(Dendro_Diff~DateTime,data=d.l[[i]],ylab="Diameter difference (mm)",type="l",lwd=2,legend=F,ylim=c(-0.5,0.5))
  abline(h=0)
  legend("topleft",bty="n",cex=1.5,legend=d.l[[i]]$chamber[1])
  legend("bottomright",bty="n",cex=1,legend=paste("init diam = ",initval,sep=""))
  
  #- add the manual measurements
  #d.hand.l[[i]]$Manual_Diff <- with(d.hand.l[[i]],diam-initval)
  #d.hand.l[[i]]$DateTime <- as.POSIXct(d.hand.l[[i]]$Date)
  #points(Manual_Diff~DateTime,data=d.hand.l[[i]])
}
dev.off()
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------

#- plot raw data
windows(80,60);par(mfrow=c(2,1),cex.lab=1.5,mar=c(6,6,1,1))
palette(c("black",brewer.pal(11,"Spectral")))

plotBy(Dendro_Avg~DateTime|chamber,data=d,legend=F,lty=1,lwd=3,type="o")
legend("bottomleft",pch=16,lty=1,legend=levels(d$chamber),col=palette()[1:12],ncol=6)
plotBy(FullBr_Avg~DateTime|chamber,data=d,legend=F,lty=1,lwd=3,type="o")

#- zoom in
windows()
plotBy(Dendro_Avg~DateTime|chamber,data=d,legend=F,lty=1,lwd=3,type="l",ylim=c(25,50))

#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- plot normalized data
windows(80,60);par(mfrow=c(2,1),cex.lab=1.5,mar=c(6,6,1,1))
palette(c("black",brewer.pal(11,"Spectral")))

plotBy(Dendro_Norm~DateTime|chamber,data=d,legend=F,lty=1,lwd=3,type="l",ylim=c(0.98,1.0))
legend("bottomright",pch=16,lty=1,legend=levels(d$chamber),col=palette()[1:12],ncol=6)

#- average by treatment, then plot again
d$DateTime_hr <- nearestTimeStep(d$DateTime,nminutes=60,align="floor")
d.m1 <- summaryBy(Dendro_Norm~DateTime_hr|chamber+T_treatment,data=d,FUN=c(mean),keep.names=T)
d.m <- summaryBy(Dendro_Norm~DateTime_hr|T_treatment,data=d.m1,FUN=c(mean,standard.error),keep.names=T)

plotBy(Dendro_Norm.mean~DateTime_hr|T_treatment,data=d.m,legend=F,lty=1,lwd=3,type="l",ylim=c(0.98,1.0),
       panel.first=adderrorbars(x=d.m$DateTime_hr,y=d.m$Dendro_Norm.mean,
                                SE=d.m$Dendro_Norm.standard.error,direction="updown"))
legend("bottomright",pch=16,lty=1,legend=levels(d.m$T_treatment),col=palette()[1:12],ncol=2,bty="n")

#-----------------------------------------------------------------------------------------------------------
