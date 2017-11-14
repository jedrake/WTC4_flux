
source("R/loadLibraries.R")
library(RColorBrewer)
library(scales)
library(colorRamps)
library(plantecophys)

#-----------------------------------------------------------------------------------------------------------
#- download the leaf temperature data from HIEv (i.e., the actual thermocouples!)
#   Recall that these thermocouples were installed in chambers 1, 2, 5, 9, 10, and 12
#   Thermocouples were installed in all chambers on 27 June 2016

##- Also note that Angelica, Sebastian, and I moved the IR sensors on Thursday 23rd June 2016. The sensors
#    are now mounted on the northern side of each chamber and look in at an area of dense canopy.
#  Angelica and I moved the sensors again on 12 Sept 2016. The trees had grown, so we moved the sensors up.

files <- searchHIEv("WTC_AUTO_C[0-9]{2}_LEAFTEMPS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{2}_LEAFTEMPS", startDate="2016-6-27", endDate="2016-11-25",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache_Tleaf.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")

d2 <- subset(d,DateTime > as.POSIXct("2016-06-27 15:10:00",tz="UTC") )#& chamber %in% c("C01","C06","C05","C09","C10","C12"))
d2$chamber <- factor(d2$chamber)                                    
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- NA fill some crazy measurements
#- NA fill the third thermocouple in chambers 1 and 10 (this is now a GASP sensor)
d2[which(d2$chamber=="C01" | d2$chamber=="C10"),"LeafT_Avg.3."] <- NA

d2[which(d2$chamber=="C06" & as.Date(d2$DateTime)==as.Date("2016-06-28") & d2$LeafT_Avg.2. > 30),"LeafT_Avg.2."] <- NA

#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- write out a csv of the data
d3 <- subset(d,DateTime > as.POSIXct("2016-07-2 00:00:00",tz="UTC") )#& chamber %in% c("C01","C06","C05","C09","C10","C12"))

leafT <- d3[,c("DateTime","chamber","LeafT_Avg.1.","LeafT_Avg.2.")]
names(leafT)[3:4] <- c("LeafT1","LeafT2")

#- NA-fill some bad data
tofill <- which(as.Date(leafT$DateTime)==as.Date("2016-07-1") & leafT$LeafT1 < 0)
tofill2 <- which(as.Date(leafT$DateTime)==as.Date("2016-07-1") & leafT$LeafT1 >30)
leafT[c(tofill,tofill2),"LeafT1"] <- NA

tofill3 <- which(as.Date(leafT$DateTime)<as.Date("2016-08-30") & as.Date(leafT$DateTime)>as.Date("2016-08-24") & leafT$chamber=="C05")
leafT[tofill3,"LeafT2"] <- NA



#write.csv(leafT,"Output/WTC_TEMP-PARRA_LEAFT-THERMOCOUPLE_20160702-20161125_L0.csv",row.names=F)
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation

#- search for and download the air variable dataset from the WTCs. Contains the leaf temperature data.
#- Note that I use the cachefile to reduce the time it takes to re-run this code.
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
IRT <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2016-1-1", endDate="2016-11-25",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache.rdata")
IRT$chamber <- as.factor(substr(IRT$Source,start=10,stop=12)) # extract the chamber number from the filename
IRT$T_treatment <- ifelse(as.numeric(substr(as.character(IRT$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
IRT$Tdiff <- with(IRT,TargTempC_Avg-SBTempC_Avg)
IRT$DateTime_hr <- nearestTimeStep(IRT$DateTime, nminutes = 15, align = "floor")
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- write out a csv of the infrared temperature measurements
IRTtowrite <- IRT[,c("DateTime","chamber","PPFD_Avg","TargTempC_Avg")]

#write.csv(IRTtowrite,"Output/WTC_TEMP-PARRA_LEAFT-IR_2016010-20161125_L0.csv",row.names=F)

#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#- merge the IR temperatures with the leaf temperatures
IRTsub <- IRT[,c("DateTime","T_treatment","TargTempC_Avg","PPFD_Avg","SBTempC_Avg","chamber")]
IRTsub$DateTime <- nearestTimeStep(IRTsub$DateTime,nminutes=15,align="floor")

IRTsub.m <- data.frame(dplyr::summarize(dplyr::group_by(IRTsub, DateTime, chamber,T_treatment), 
                                        TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                        PPFD_Avg=mean(PPFD_Avg,na.rm=T)))

d2sub <- d2[,c("DateTime","chamber","T_treatment","LeafT_Avg.1.","LeafT_Avg.2.")]
d2sub$DateTime <- nearestTimeStep(d2sub$DateTime,nminutes=15,align="floor")

d2sub.m <- data.frame(dplyr::summarize(dplyr::group_by(d2sub, DateTime, chamber,T_treatment), 
                                       LeafT_Avg.1.=mean(LeafT_Avg.1.,na.rm=T),
                                       LeafT_Avg.2.=mean(LeafT_Avg.2.,na.rm=T)))
d3 <- merge(d2sub.m,IRTsub.m,by=c("DateTime","chamber","T_treatment"),all.y=T)
d3$chamber <- factor(d3$chamber)
d3$Tleaf_mean <- rowMeans(d3[,c("LeafT_Avg.1.","LeafT_Avg.2.")],na.rm=T)
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- Download the within chamber met data. Note that I upload these data monthly by extracting them from the
#  trendlogs. This takes a long time and is a pain, or I would do it more frequently
downloadHIEv(searchHIEv("WTC_TEMP-PARRA_CM_WTCMET-MIN"),
             topath="C:/Repos/wtc4_flux/data/fromHIEv",
             cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4_MET_cache.rdata")

#- read in the files. They are large, so this takes a few moments
metfiles <- list.files(path="data/fromHIEv/",pattern="WTCMET-MIN",full.names=T)
metdat <- do.call(rbind,lapply(metfiles,read.csv))
metdat$DateTime <- as.POSIXct(metdat$DateTime,format="%Y-%m-%d %T",tz="UTC")
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- merging the within-chamber met datasets and the IR temperature datasets became difficult 
#  "Error: cannot allocate vector of size 142.5 Mb"
#  So, calculate 15-minutely averages, and then merge those
d3$DateTime <- nearestTimeStep(d3$DateTime,nminutes=15,align="floor")
d3.m <- data.frame(dplyr::summarize(dplyr::group_by(d3, DateTime, chamber,T_treatment), 
                  LeafT_Avg.1.=mean(LeafT_Avg.1.,na.rm=T),
                  LeafT_Avg.2.=mean(LeafT_Avg.2.,na.rm=T),
                  TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                  PPFD_Avg=mean(PPFD_Avg,na.rm=T),
                  Tleaf_mean=mean(Tleaf_mean,na.rm=T)))

metdat$DateTime <- nearestTimeStep(metdat$DateTime,nminutes=15,align="floor")
metdat.m <- data.frame(dplyr::summarize(dplyr::group_by(metdat, DateTime, chamber), 
                                        Tair_SP=mean(Tair_SP,na.rm=T),
                                        RH_al=mean(RH_al,na.rm=T),
                                        DP_al=mean( DP_al,na.rm=T),
                                        Tsub_al=mean(Tsub_al,na.rm=T),
                                        RH_SP=mean(RH_SP,na.rm=T),
                                        Tair_al=mean(Tair_al,na.rm=T)))
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- merge the within-chamber met data with the leaf temperature datasets
metdat_sum <- metdat.m[,c("DateTime","Tair_al","chamber")]

d4 <- merge(d3.m,metdat_sum,by=c("DateTime","chamber"))
d4$Tdiff_IR <- with(d4,TargTempC_Avg-Tair_al)
d4$Tdiff_TC <- with(d4,Tleaf_mean-Tair_al)
d4$chamber <- factor(d4$chamber,levels=levels(metdat$chamber))
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the leaf light dataset

files <- list.files(path="data/leafLight/",pattern="LEAFLIGHT_R.dat",full.names=TRUE)

#- remove the backup files, and the files from the first campaign
files <- files[-grep("backup",files)]
files <- files[-grep("CAMPAIGN1",files)]


ll.l <- list()
for(i in 1:length(files)){
  ll.l[[i]] <- readTOA5(files[i])
  ll.l[[i]]$chamber <- substr(files[i],start=25,stop=27)
}
ll <- do.call(rbind,ll.l)
ll2 <- subset(ll,DateTime > as.POSIXct("2016-06-28 11:50:00",tz="UTC"))[,c("DateTime","GASP_Avg.1.","GASP_Avg.2.","chamber")]

#- apply calibrations for each GASP
cals <- data.frame(chamber=c("C01","C01","C10","C10"),sensor=c(1,2,1,2),
                   int=rep(0,4),
                   slope = c(483.45,505.70,780.74,530.2),
                   curve = c(-19.3,-7,-114.71,-22.7))

#- apply the GASP cals
ll2$GASP_PAR_2 <- ll2$GASP_PAR_1 <- NA
C01inds <- which(ll2$chamber=="C01")
C10inds <- which(ll2$chamber=="C10")

ll2$GASP_PAR_1[C01inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.1.[C01inds]+cals[1,5]*ll2$GASP_Avg.1.[C01inds]
ll2$GASP_PAR_2[C01inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.2.[C01inds]+cals[1,5]*ll2$GASP_Avg.2.[C01inds]
ll2$GASP_PAR_1[C10inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.1.[C10inds]+cals[1,5]*ll2$GASP_Avg.1.[C10inds]
ll2$GASP_PAR_2[C10inds] <- cals[1,3]+cals[1,4]*ll2$GASP_Avg.2.[C10inds]+cals[1,5]*ll2$GASP_Avg.2.[C10inds]

#- first campaign was from 2016-06-28 to 2016-07-14. Second campaign was from 2016-09-20 to 2016-10-06
c1 <- subset(ll2,DateTime> as.POSIXct("2016-06-28 00:00:00",tz="UTC") & DateTime < as.POSIXct("2016-07-14 23:59:00",tz="UTC"))
c1$campaign <- 1

c2 <- subset(ll2,DateTime> as.POSIXct("2016-09-20 00:00:00",tz="UTC") & DateTime < as.POSIXct("2016-10-06 23:59:00",tz="UTC"))
c2$campaign <- 2

ll3 <- rbind(c1,c2)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- merge the GASP data with the rest of it
d5 <- merge(d4,ll3,by=c("chamber","DateTime"),all.x=T)

#- get Tdiffs for each thermocoupled
d6 <- subset(d5,is.na(GASP_Avg.1.)==F)
d6$chamber <- factor(d6$chamber)

d6$LeafT_1_diff <- with(d6,LeafT_Avg.1.-Tair_al)
d6$LeafT_2_diff <- with(d6,LeafT_Avg.2.-Tair_al)

plotBy(LeafT_1_diff~GASP_PAR_1|chamber,data=d6,pch=3,legend=F,xlim=c(0,2200),ylim=c(-1.5,12),
       xlab="GASP PAR (umol m-2 s-1)",ylab=expression(T[leaf]~"-"~T[air]~(degree*C)))
plotBy(LeafT_2_diff~GASP_PAR_2|chamber,data=d6,pch=1,legend=F,add=T)
legend("topright",fill=palette()[1:2],legend=c("C01","C10"))
abline(h=0)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#- plot a focal day
toplot.all <- subset(d4,as.Date(DateTime)==as.Date("2016-10-28"))
toplot.l <- split(toplot.all,toplot.all$chamber)

pdf(file="output/Tleaf_comparison_2016-10-28.pdf")
par(mfrow=c(2,2))
for(i in 1:length(toplot.l)){
  toplot <- toplot.l[[i]]
  
  #- loop over each chamber, print a pdf
  #palette(c("black","forestgreen",brewer.pal(4,"Spectral")))
  palette(blue2green2red(12))
  
  #- plot PAR
  plotBy(PPFD_Avg~DateTime,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(0,1500),col="black",
         ylab="PAR")
  title(main=paste(toplot$chamber[1],as.Date(toplot$DateTime)[1],sep=" "))
  
  
  #- plot Temperatures
  plotBy(TargTempC_Avg~Tleaf_mean|chamber,data=toplot,legendwhere="topleft",type="p",xlim=c(0,35),ylim=c(0,35),pch=1,
         legend=F,col="black",
         xlab=expression(T[leaf]~(avg~of~2~thermocouples)),ylab=expression(T[leaf]~(IR)))
  abline(0,1)
  
  #- plot temperatures
  plotBy(TargTempC_Avg~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(0,35),lwd=2,col="black",
         ylab=expression("T"~(degree*C)))
  plotBy(LeafT_Avg.1.~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",lty=2,add=T,col="black",
         ylab="Tleaf")
  plotBy(LeafT_Avg.2.~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",add=T,lty=2,col="black")
  plotBy(Tair_al~DateTime|chamber,data=toplot,legendwhere="topright",type="l",add=T,lty=1,lwd=2,col="grey",legend=F)
  legend("topleft",lty=c(1,2,1),bty="n",legend=c("IR","TC","Tair"),col=c("black","black","grey"))
  
  #- plot Tdiff vs. PAR
  plotBy(Tdiff_IR~PPFD_Avg|chamber,data=toplot,legend=F,type="p",xlim=c(0,1500),ylim=range(toplot.all$Tdiff_TC,na.rm=T),pch=16,col="black",
         xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
  plotBy(Tdiff_TC~PPFD_Avg|chamber,data=toplot,legend=F,type="p",xlim=c(0,1500),pch=1,add=T,col="black",
         xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
  abline(h=0)
  legend("topleft",pch=c(1,16),legend=c("Thermocouples","IR sensor"),col="black",bty="n")
}
dev.off()









#-----------------------------------------------------------------------------------------------------------
#- plot a focal for GASP and Tleaf data
toplot.all <- subset(d6,as.Date(DateTime)==as.Date("2016-10-05") & hour(DateTime)>=6 & hour(DateTime)<=17)
toplot.l <- split(toplot.all,toplot.all$chamber)

pdf(file="output/GASP_Tleaf_comparison_2016-10-05.pdf")
par(mfrow=c(3,1),mar=c(6,6,1,1),cex.lab=1.5)
for(i in 1:length(toplot.l)){
  toplot <- toplot.l[[i]]
  
  #- loop over each chamber, print a pdf
  palette(c("black","forestgreen",brewer.pal(4,"Spectral")))
  
  #- plot PAR
  plotBy(PPFD_Avg~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(0,2000),
         ylab="PAR",col="black")
  plotBy(GASP_PAR_1~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",
         ylab="PAR",col=c("blue"),add=T)
  plotBy(GASP_PAR_2~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",
         ylab="PAR",col="red",add=T)
  title(main=paste(toplot$chamber[1],as.Date(toplot$DateTime)[1],sep=" "))
  legend("topleft",lty=1,col=c("black","blue","red"),legend=c("PAR","GASP1","GASP2"),bty="n")
  
  
  #- plot temperatures
  plotBy(TargTempC_Avg~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",ylim=c(0,25),lwd=2,
         ylab=expression("T"~(degree*C)),col="black")
  plotBy(LeafT_Avg.1.~DateTime|chamber,data=toplot,legend=F,legendwhere="topright",type="l",lty=1,add=T,
         ylab="Tleaf",col="blue")
  plotBy(LeafT_Avg.2.~DateTime|chamber,col="red",data=toplot,legend=F,legendwhere="topright",type="l",add=T,lty=1)
  plotBy(Tair_al~DateTime|chamber,data=toplot,legendwhere="topright",type="l",add=T,lty=1,lwd=2,col="grey",legend=F)
  legend("topleft",lty=c(1,1,1,1),bty="n",legend=c("IR","TC1","TC2","Tair"),col=c("black","blue","red","grey"))
  
  #- plot Tdiff vs. PAR
  plotBy(LeafT_1_diff~GASP_PAR_1|chamber,data=toplot,legend=F,type="p",xlim=c(0,2000),ylim=c(0,10),pch=1,col="blue",
         xlab=expression(GASP~PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
  plotBy(LeafT_2_diff~GASP_PAR_2|chamber,data=toplot,legend=F,type="p",xlim=c(0,1200),pch=1,add=T,col="red",
         xlab=expression(GASP~PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(T[leaf]-T[air]~(degree*C)))
  abline(h=0)
  legend("topleft",pch=c(1,1),legend=c("TC1","TC2"),col=c("blue","red"),bty="n")
}
dev.off()









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- average across chambers and such, plot IR temperatures, TC temperatures for treatments
head(d5)

#- a nice dataset from june
#starttime <- as.POSIXct("2016-06-28 00:00:00",format="%Y-%m-%d %T",tz="UTC")
#endtime <- as.POSIXct("2016-07-04 00:00:00",format="%Y-%m-%d %T",tz="UTC")

#- beginning of heat wave stuff
starttime <- as.POSIXct("2016-2-10 00:00:00",format="%Y-%m-%d %T",tz="UTC")
endtime <- as.POSIXct("2016-11-25 00:00:00",format="%Y-%m-%d %T",tz="UTC")

d5 <- subset(d5,DateTime>starttime & DateTime < endtime)

#- hourly averages across treatments
d5$DateTime_hr <- nearestTimeStep(d5$DateTime,nminutes=30,align="floor")
dat.hr1 <- summaryBy(.~DateTime_hr+chamber+T_treatment,data=d5,FUN=mean,na.rm=T,keep.names=T)
dat.hr <- summaryBy(.~DateTime_hr+T_treatment,data=dat.hr1,FUN=c(mean,standard.error),na.rm=T,keep.names=F)

dat.hr$TargTempC_Avg.hi <- dat.hr$TargTempC_Avg.mean+dat.hr$TargTempC_Avg.standard.error
dat.hr$TargTempC_Avg.low <- dat.hr$TargTempC_Avg.mean-dat.hr$TargTempC_Avg.standard.error
dat.hr$Tleaf.hi <- dat.hr$Tleaf_mean.mean+dat.hr$Tleaf_mean.standard.error
dat.hr$Tleaf.low <- dat.hr$Tleaf_mean.mean-dat.hr$Tleaf_mean.standard.error


dat.hr_a <- subset(dat.hr,T_treatment=="ambient")
dat.hr_e <- subset(dat.hr,T_treatment=="elevated")







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- make a figure of mean maximum temperatures observed before, during, and after the heatwave
starttime2 <- as.POSIXct("2016-10-28 00:00:00",format="%Y-%m-%d %T",tz="UTC")
endtime2 <- as.POSIXct("2016-11-7 00:00:00",format="%Y-%m-%d %T",tz="UTC")

d7 <- subset(d4,DateTime>starttime2 & DateTime < endtime2)
d7$Date <- as.Date(d7$DateTime)

#- merge in the heatwave treatments
linkdf <- data.frame(chamber = levels(as.factor(d7$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08

d7 <- merge(d7,linkdf,by="chamber")

#- get maximum temperatures
tmax <- summaryBy(TargTempC_Avg+Tleaf_mean+Tair_al~chamber+HWtrt+Date,data=d7,FUN=max,na.rm=TRUE,keep.names=T)
tmax.m <- summaryBy(TargTempC_Avg+Tleaf_mean+Tair_al~HWtrt+Date,data=tmax,FUN=c(mean,standard.error))
#tmax.m$combotrt <- factor(paste(tmax.m$T_treatment,tmax.m$HWtrt,sep="_"))

palette(c("blue","red"))
#- plot maximum temperatures
windows();par(mar=c(6,7,1,1),cex.lab=2,cex.axis=1.5)
plotBy(Tair_al.mean~Date|HWtrt,data=tmax.m,legend=F,type="l",lty=3,lwd=3,ylim=c(15,50),
       ylab=expression(Maximum~temperature~(degree*C)))
adderrorbars(x=tmax.m$Date,y=tmax.m$Tair_al.mean,SE=tmax.m$Tair_al.standard.error,
             direction="updown",col=tmax.m$HWtrt,barlen=0,lwd=0.5)
plotBy(TargTempC_Avg.mean~Date|HWtrt,data=tmax.m,legend=F,type="l",lty=2,lwd=3,ylab="",add=T)
adderrorbars(x=tmax.m$Date,y=tmax.m$TargTempC_Avg.mean,SE=tmax.m$TargTempC_Avg.standard.error,
             direction="updown",col=tmax.m$HWtrt,barlen=0,lwd=0.5)
plotBy(Tleaf_mean.mean~Date|HWtrt,data=tmax.m,legend=F,type="l",lty=1,lwd=3,ylab="",add=T)
adderrorbars(x=tmax.m$Date,y=tmax.m$Tleaf_mean.mean,SE=tmax.m$Tleaf_mean.standard.error,
             direction="updown",col=tmax.m$HWtrt,barlen=0,lwd=0.5)

legend("bottom",legend=c("C-Air","C-IR","C-TC","HW-Air","HW-IR","HW-TC"),
       lty=c(3,2,1,3,2,1),lwd=3,col=c(rep("blue",3),rep("red",3)),ncol=2,
       title="Temperature measurement",cex=1.5)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- read in the flux data (processed by the wtc4_flux_processing R project)
fluxdat <- read.csv("c:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20160228-20161123_L0.csv")
fluxdat$DateTime <- as.POSIXct(fluxdat$DateTime,format="%Y-%m-%d %T",tz="UTC")
fluxdat$VPD <- RHtoVPD(RH=fluxdat$RH_al,TdegC=fluxdat$Tair_al)

#- subset to just the data corresponding to teh dates in the d5 dataframe
starttime <- min(d5$DateTime)
endtime <- max(d5$DateTime)
fluxdat2 <- subset(fluxdat,DateTime>starttime & DateTime < endtime)

#- 30-min averages across treatments
fluxdat2$DateTime_hr <- nearestTimeStep(fluxdat2$DateTime,nminutes=30,align="floor")
fluxdat.hr1 <- summaryBy(FluxCO2+FluxH2O+Tair_al+PAR+VPD~DateTime_hr+chamber+T_treatment,
                         data=subset(fluxdat2,DoorCnt==0),FUN=mean,na.rm=T,keep.names=T)
fluxdat.hr <- summaryBy(.~DateTime_hr+T_treatment,data=fluxdat.hr1,FUN=c(mean,standard.error),na.rm=T,keep.names=F)

#- extract highs and lows for polygon plots
fluxdat.hr$FluxCO2.hi <- fluxdat.hr$FluxCO2.mean+fluxdat.hr$FluxCO2.standard.error
fluxdat.hr$FluxCO2.low <- fluxdat.hr$FluxCO2.mean-fluxdat.hr$FluxCO2.standard.error
fluxdat.hr_a <- subset(fluxdat.hr,T_treatment=="ambient")
fluxdat.hr_e <- subset(fluxdat.hr,T_treatment=="elevated")




windows(35,25)
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,2,2,2))
par( mar=c(0.3,2,0.3,0.8), oma=c(5,6,0.5,2.5))
#- plot light
plotBy(PPFD_Avg.mean~DateTime_hr|T_treatment,data=subset(dat.hr,T_treatment=="ambient"),
       type="l",col=c("black"),lty=1,legend=F,ylim=c(0,2000))
title(ylab=expression(atop(PPFD,
                           ~(mu*mol~m^-2~s^-1))),xpd=NA,cex.lab=1.5)
#- plot fluxes
plotBy(FluxCO2.mean~DateTime_hr|T_treatment,data=fluxdat.hr_a,type="l",col=c("blue"),lty=1,lwd=2,legend=F,ylim=c(-0.05,0.3))
plotBy(FluxCO2.mean~DateTime_hr|T_treatment,data=fluxdat.hr_e,type="l",col=c("red"),
       add=T,lty=1,lwd=2,legend=F,ylim=c(-0.05,0.3))
abline(h=0)
#polygon(x = c(fluxdat.hr_a$DateTime_hr, rev(fluxdat.hr_a$DateTime_hr)), 
#        y = c(fluxdat.hr_a$FluxCO2.low,rev(fluxdat.hr_a$FluxCO2.hi)) , col = alpha("blue",0.5), border = F)
#polygon(x = c(fluxdat.hr_e$DateTime_hr, rev(fluxdat.hr_e$DateTime_hr)), 
#        y = c(fluxdat.hr_e$FluxCO2.low,rev(fluxdat.hr_e$FluxCO2.hi)) , col = alpha("red",0.5), border = F)
title(ylab=expression(atop(CO[2]~Flux,
                           (mmol~s^-1))),xpd=NA,cex.lab=1.5)
legend("topleft",legend=c("Ambient","Warmed"),cex=1.1,bty="n",
       lty=c(1,1),lwd=c(2,2),seg.len = 4,col=c("blue","red"))

#- plot ambient
plotBy(Tair_al.mean~DateTime_hr|T_treatment,data=dat.hr_a,type="l",col=c("blue","red"),lty=3,lwd=2,legend=F,ylim=c(0,40))
plotBy(TargTempC_Avg.mean~DateTime_hr|T_treatment,data=dat.hr_a,type="l",col=c("blue","red"),lty=1,legend=F,add=T,lwd=2)
#polygon(x = c(dat.hr_a$DateTime_hr, rev(dat.hr_a$DateTime_hr)), 
#              y = c(dat.hr_a$TargTempC_Avg.low,rev(dat.hr_a$TargTempC_Avg.hi)) , col = alpha("blue",0.5), border = F)
plotBy(Tleaf_mean.mean~DateTime_hr|T_treatment,data=dat.hr_a,type="l",col=c("forestgreen","orange"),lty=1,legend=F,add=T,lwd=2)
#polygon(x = c(dat.hr_a$DateTime_hr, rev(dat.hr_a$DateTime_hr)), 
#        y = c(dat.hr_a$Tleaf.low,rev(dat.hr_a$Tleaf.hi)) , col = alpha("forestgreen",0.5), border = F)
legend("bottomleft",legend=c("Tair","Infrared","Thermocouple"),cex=1.1,bty="n",
       lty=c(3,1,1),lwd=c(2,2,2),seg.len = 4,col=c("blue","blue","forestgreen"))
title(ylab=expression(atop(Temperature,
                           "in"~ambient~(degree*C))),xpd=NA,cex.lab=1.5)

#- plot warmed
plotBy(Tair_al.mean~DateTime_hr|T_treatment,data=dat.hr_e,type="l",col=c("red"),lty=3,lwd=2,legend=F,ylim=c(0,40))
plotBy(TargTempC_Avg.mean~DateTime_hr|T_treatment,data=dat.hr_e,type="l",col=c("red"),lty=1,legend=F,add=T,lwd=2)
#polygon(x = c(dat.hr_e$DateTime_hr, rev(dat.hr_e$DateTime_hr)), 
#        y = c(dat.hr_e$TargTempC_Avg.low,rev(dat.hr_e$TargTempC_Avg.hi)) , col = alpha("red",0.5), border = F)
plotBy(Tleaf_mean.mean~DateTime_hr|T_treatment,data=dat.hr_e,type="l",col=c("orange"),lty=1,legend=F,add=T,lwd=2)
#polygon(x = c(dat.hr_e$DateTime_hr, rev(dat.hr_e$DateTime_hr)), 
#        y = c(dat.hr_e$Tleaf.low,rev(dat.hr_e$Tleaf.hi)) , col = alpha("orange",0.5), border = F)
legend("bottomleft",legend=c("Tair","Infrared","Thermocouple"),cex=1.1,bty="n",
       lty=c(3,1,1),lwd=c(2,2,2),seg.len = 4,col=c("red","red","orange"))
title(ylab=expression(atop(Temperature,
                           "in"~warmed~(degree*C))),xpd=NA,cex.lab=1.5)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
# as above, but just plot the ambient treatment for a few days (Sept 23rd and 24th)
windows(15,25)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,2,2))
par( mar=c(2,4,0.3,0.8), oma=c(5,6,3,2.5),cex.axis=1.5,las=1)

toplot <- subset(fluxdat.hr,T_treatment=="ambient" & DateTime_hr > as.POSIXct("2016-09-23 00:00:00",tz="UTC"))
toplot2 <- subset(dat.hr_a,T_treatment=="ambient" & DateTime_hr > as.POSIXct("2016-09-23 00:00:00",tz="UTC"))

#- plot light
plotBy(PAR.mean~DateTime_hr|T_treatment,data=subset(toplot,T_treatment=="ambient"),ylab="",
       type="l",col=c("black"),lty=1,legend=F,lwd=3,ylim=c(0,2000))
title(ylab=expression(atop(PPFD,
                           ~(mu*mol~m^-2~s^-1))),xpd=NA,cex.lab=2.5,line=4)

#- plot ambient temperature
plotBy(Tair_al.mean~DateTime_hr|T_treatment,data=toplot2,type="l",col=c("red","red"),lty=3,lwd=3,legend=F,ylim=c(0,30),
       ylab="")
plotBy(TargTempC_Avg.mean~DateTime_hr|T_treatment,data=toplot2,type="l",col=c("red","red"),lty=1,legend=F,add=T,lwd=3,
       ylab="")
polygon(x = c(toplot2$DateTime_hr, rev(toplot2$DateTime_hr)), 
              y = c(toplot2$TargTempC_Avg.low,rev(toplot2$TargTempC_Avg.hi)) , col = alpha("red",0.5), border = F)
legend("bottomleft",legend=c("Air","Leaf"),cex=1.5,bty="n",
       lty=c(3,1),lwd=c(2,2),seg.len = 4,col=c("red","red"))
title(ylab=expression(Temperature~(degree*C)),xpd=NA,cex.lab=2.5,line=4)

#- plot fluxes
plotBy(FluxCO2.mean~DateTime_hr|T_treatment,data=toplot,type="l",col=c("blue"),lty=1,lwd=3,legend=F,ylim=c(-0.05,0.2),
       ylab="")
abline(h=0)
polygon(x = c(toplot$DateTime_hr, rev(toplot$DateTime_hr)), 
        y = c(toplot$FluxCO2.low,rev(toplot$FluxCO2.hi)) , col = alpha("blue",0.5), border = F)
title(ylab=expression(atop(CO[2]~Flux,
                           (mmol~s^-1))),xpd=NA,cex.lab=2.5,line=4)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- merge fluxdata.hr and IRT (fluxes and temperatures).
#   This is for the "thermoregulation" manuscript


head(fluxdat.hr1)
head(IRT)
IRT$DateTime_hr <- nearestTimeStep(IRT$DateTime,nminutes=30,align="floor")
#IRT.hr1 <- summaryBy(TargTempC_Avg~DateTime_hr+chamber+T_treatment,
#                         data=subset(IRT,DateTime>starttime & DateTime<endtime),FUN=mean,na.rm=T,keep.names=T)

IRT.hr1 <- data.frame(dplyr::summarize(dplyr::group_by(IRT, DateTime_hr, chamber,T_treatment), 
                                        TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                        PPFD_Avg=mean(PPFD_Avg,na.rm=T)))

combodat <- merge(fluxdat.hr1,IRT.hr1,by=c("DateTime_hr","T_treatment","chamber"))
combodat$week <- factor(week(combodat$DateTime_hr))
combodat$weekfac <- factor(paste(combodat$chamber,combodat$week,sep="-"))



#-----------------------------------------------------------------------------------------------------------
#- plot all of the leaf temperature vs. air temperature data
windows(75,75);par(mar=c(7,7,1,2),mfrow=c(1,1))
palette(c("blue","red"))
plotBy(TargTempC_Avg~Tair_al|T_treatment,data=subset(combodat,PPFD_Avg>500),pch=1,xlim=c(0,50),ylim=c(0,50),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1)
lmTall <- lm(TargTempC_Avg~Tair_al,data=subset(combodat,PPFD_Avg>500))
abline(lmTall,lty=2)
legend("bottomright",paste("Slope = ",round(coef(lmTall)[[2]],2),sep=""),bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(T[leaf]~(degree*C)),xpd=NA,cex.lab=2)

#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#- calculate assimilation weighted leaf temperature

#- instead of using weeks, cut the 259 days of observations into 259 bins of 1-day each
combodat$bin4days <- as.factor(cut(as.Date(combodat$DateTime_hr),breaks=259,labels=1:259))
combodat$weekfac <- factor(paste(combodat$chamber,combodat$bin4days,sep="-"))

#- calculate weighted average leaf temperatures, for each bin
combodat.list <- split(combodat,combodat$weekfac)
chamber <-meanAirT<- meanLeafT <-  weightedMeanLeafT <- Date <- bin <- T_treatment <- VPD <- list()
for(i in 1:length(combodat.list)){
  tocalc <- subset(combodat.list[[i]], PAR> 20 & FluxCO2>0)
  
  # zero fill negative fluxes
  tona <- which(tocalc$FluxCO2 < 0)
  tocalc$FluxCO2[tona] <- 0
  
  chamber[[i]] <- as.character(tocalc$chamber[1])
  bin[[i]] <- as.character(tocalc$bin4days[1])
  T_treatment[[i]] <- as.character(tocalc$T_treatment[1])
  meanAirT[[i]] <- mean(tocalc$Tair_al)
  meanLeafT[[i]] <- mean(tocalc$TargTempC_Avg)

  VPD[[i]] <- mean(tocalc$VPD)
  Date[[i]] <- as.Date(tocalc$DateTime_hr)[1]
  
  weightedMeanLeafT[[i]] <- weighted.mean(tocalc$TargTempC_Avg,tocalc$FluxCO2)
}

#output_meanT <- data.frame(chamber=levels(fluxdat2$chamber),T_treatment=factor(rep(c("ambient","elevated"),6)))
output_meanT <- data.frame(bin4days=levels(combodat$weekfac))
output_meanT$chamber <- do.call(rbind,chamber)
output_meanT$T_treatment <- do.call(rbind,T_treatment)
output_meanT$bin <- do.call(rbind,bin)
output_meanT$Date <- as.Date(do.call(rbind,Date),origin="1970-01-01")
output_meanT$meanAirT <- do.call(rbind,meanAirT)
output_meanT$meanLeafT <- do.call(rbind,meanLeafT)
output_meanT$weightedMeanLeafT <- do.call(rbind,weightedMeanLeafT)
output_meanT$VPD <- do.call(rbind,VPD)



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
windows(100,75);par(mar=c(7,7,1,2),mfrow=c(1,2))
palette(c("blue","red"))
mindate <- min(output_meanT$Date,na.rm=T)#as.Date("2016-05-01")
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=1,xlim=c(5,35),ylim=c(5,35),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1)
lmT <- lm(meanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
abline(lmT,lty=2)
legend("bottomright",paste("Slope = ",round(coef(lmT)[[2]],2),sep=""),bty="n")
legend("topleft",pch=c(1,1),col=c("blue","red"),legend=c("Ambient","Warmed"))


#plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=output_meanT,pch=16,add=T,legend=F)
#legend("topleft",c("A: Tleaf","W: Tleaf","A: Weighted-Tleaf","W: Weighted-Tleaf"),
#       pch=c(1,1,16,16),col=c("blue","red"),cex=1.1)
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(T[leaf]~(degree*C)~(measured)),xpd=NA,cex.lab=2)

#-
plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=1,xlim=c(5,35),ylim=c(5,35),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1)
lm1 <- lm(weightedMeanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
abline(lm1,lty=2)
legend("bottomright",paste("Slope = ",round(coef(lm1)[[2]],2),sep=""),bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(Assimilation~weighted~T[leaf]~(degree*C)),xpd=NA,cex.lab=2)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





windows(35,25)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,1,1))
par( mar=c(0.5,6,0.5,1.5), oma=c(5,6,0.5,2.5),cex.axis=2)
ylims=c(5,40)
#- Air temperatures
boxplot(meanAirT~T_treatment,data=output_meanT,col=c("blue","red"),ylim=ylims)
title(ylab=expression(T[air]~(degree*C)),cex.lab=3)
#- Leaf temperature
boxplot(meanLeafT~T_treatment,data=output_meanT,col=c("blue","red"),ylim=ylims)
title(ylab=expression(T[leaf]~(degree*C)),cex.lab=3)
#- Weighted leaf temperature
boxplot(weightedMeanLeafT~T_treatment,data=output_meanT,col=c("blue","red"),ylim=ylims)
title(ylab=expression(T[leaf]~"weighted by"~CO[2]~"flux"~(degree*C)),cex.lab=3)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- Richard proposed three time periods to extract cellulose from. These are:
#27-4-2016 -> 5-7-2016
#5-7-2016->14-9-2016
#28-9-2016->23-11-2016

# What temperatures do these correspond to?
period1start <- as.Date("2016-04-27")
period1end <- as.Date("2016-07-05")
period2start <- as.Date("2016-07-05")
period2end <- as.Date("2016-9-14")
period3start <- as.Date("2016-09-28")
period3end <- as.Date("2016-11-23")

head(fluxdat)

#- average ambient temperature in first period
mean(fluxdat[which(as.Date(fluxdat$DateTime) > period1start & as.Date(fluxdat$DateTime) < period1end & fluxdat$T_treatment=="ambient" & fluxdat$PAR>10),"Tair_al"],na.rm=T)
mean(fluxdat[which(as.Date(fluxdat$DateTime) > period2start & as.Date(fluxdat$DateTime) < period2end & fluxdat$T_treatment=="ambient"& fluxdat$PAR>10),"Tair_al"],na.rm=T)
mean(fluxdat[which(as.Date(fluxdat$DateTime) > period3start & as.Date(fluxdat$DateTime) < period3end & fluxdat$T_treatment=="ambient"& fluxdat$PAR>10),"Tair_al"],na.rm=T)

fluxdat$Date <- as.Date(fluxdat$DateTime)
fluxdat.day <- summaryBy(Tair_al~Date,data=subset(fluxdat,T_treatment=="ambient"),FUN=mean,keep.names=T)
plot(Tair_al~Date,data=fluxdat.day,type="o",ylab="Daytime air temperature (deg C)",cex.lab=2)
abline(v=c(period1start,period1end,period2start,period2end,period3start,period3end))
text(x=period1start+20,y=25,"period1")
text(x=period2start+20,y=25,"period2")
text(x=period3start+20,y=25,"period3")


period1startnew <- as.Date("2016-06-07")
period1endnew <- as.Date("2016-07-19")
period2startnew <- as.Date("2016-08-15")
period2endnew <- as.Date("2016-9-26")
period3startnew <- as.Date("2016-10-12")
period3endnew <- as.Date("2016-11-23")
plot(Tair_al~Date,data=fluxdat.day,type="o",ylab="Daytime air temperature (deg C)",cex.lab=2)
abline(v=c(period1startnew,period1endnew,period2startnew,period2endnew,period3startnew,period3endnew))
text(x=period1startnew+20,y=25,"period1")
text(x=period2startnew+20,y=25,"period2")
text(x=period3startnew+20,y=25,"period3")


mean(fluxdat[which(as.Date(fluxdat$DateTime) > period1startnew & as.Date(fluxdat$DateTime) < period1endnew & fluxdat$T_treatment=="ambient" & fluxdat$PAR>10),"Tair_al"],na.rm=T)
mean(fluxdat[which(as.Date(fluxdat$DateTime) > period2startnew & as.Date(fluxdat$DateTime) < period2endnew & fluxdat$T_treatment=="ambient"& fluxdat$PAR>10),"Tair_al"],na.rm=T)
mean(fluxdat[which(as.Date(fluxdat$DateTime) > period3startnew & as.Date(fluxdat$DateTime) < period3endnew & fluxdat$T_treatment=="ambient"& fluxdat$PAR>10),"Tair_al"],na.rm=T)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------













#---------------------
# windows(20,16)
# layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
#        widths=c(1,1), heights=c(1,1))
# par( mar=c(5,2,0.3,0.8), oma=c(0.5,6,0.5,2.5))
# plotBy(Tdiff_IR~PPFD_Avg|T_treatment,data=subset(dat.hr,PPFD_Avg>10),col=c("blue","red"),pch=16,
#        legend=F,ylim=c(-0.5,5),axes=F,xlab="")
# plotBy(Tdiff_TC~PPFD_Avg|T_treatment,data=subset(dat.hr,PPFD_Avg>10),col=c("blue","red"),pch=1,add=T,legend=F)
# magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1)
# title(xlab=expression(PPFD~(mu*mol~m^-2~s^-1)),ylab=expression(Delta*Temperature~(degree*C)),xpd=NA,cex.lab=1.2)
# legend("topleft",pch=c(1,16),legend=c("IR","TC"))
# 
# plotBy(Tdiff_TC~Tdiff_IR|T_treatment,data=subset(dat.hr,PPFD_Avg>10),col=c("blue","red"),pch=1,
#        axes=F,xlab="",legend=F)
# magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1)
# abline(0,1)
# title(xlab=expression(Delta*T[IR]~(degree*C)),ylab=expression(Delta*T[thermocouples]~(degree*C)),xpd=NA,cex.lab=1.2)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- summarize teh GASP data over time
head(d5)
dg <- subset(d5,is.na(GASP_PAR_1)==F)
dg$dTleaf_1 <- with(dg, LeafT_Avg.1.-Tair_al)
dg$dTleaf_2 <- with(dg, LeafT_Avg.2.-Tair_al)

#- create 5-minute averages
dg$DateTime_hr <- nearestTimeStep(dg$DateTime,nminutes=15,align="floor")
dg.m <- summaryBy(.~DateTime_hr+chamber+T_treatment,data=dg,FUN=mean,keep.names=T)



#----- 
#- plot summary of GASP data in time
windows(20,16)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1,2,2))
par( mar=c(0.3,2,0.3,0.8), oma=c(5,6,1.5,2.5),cex.lab=2)
#- plot global PAR
plotBy(PPFD_Avg~DateTime_hr|T_treatment,data=subset(dg.m,T_treatment=="ambient" & chamber=="C01"),
       type="l",col=c("black"),lty=1,legend=F,ylim=c(0,1500))
title(ylab=expression(atop(Chamber~PPFD,
                           (mu*mol~m^-2~s^-1))),xpd=NA,cex.lab=2)
#- plot leaf-scale PAR
plotBy(GASP_PAR_1~DateTime_hr|T_treatment,data=dg.m,type="l",col=c("blue","red"),lty=1,lwd=1,legend=F,ylim=c(0,2000))
plotBy(GASP_PAR_2~DateTime_hr|T_treatment,data=dg.m,type="l",col=c("forestgreen","orange"),lty=1,lwd=1,legend=F,add=T)
title(ylab=expression(Leaf~PPFD~(mu*mol~m^-2~s^-1)),xpd=NA,cex.lab=2)
#- plot leaf dT's
plotBy(dTleaf_1~DateTime_hr|T_treatment,data=dg.m,type="l",col=c("blue","red"),lty=1,lwd=1,legend=F,ylim=c(-0.5,10))
plotBy(dTleaf_2~DateTime_hr|T_treatment,data=dg.m,type="l",col=c("forestgreen","orange"),lty=1,lwd=1,legend=F,add=T)
title(ylab=expression(Delta~T[leaf]~(degree*C)),xpd=NA,cex.lab=2)
abline(h=0,col="grey")



#----- 
#- plot summary of dT vs. PAR
windows(20,16)
layout(matrix(c(1), 1, 1, byrow = TRUE), 
       widths=c(1), heights=c(1))
par( mar=c(6,7,0.3,0.8), oma=c(0.5,0.5,0.5,0.5))
plotBy(dTleaf_1~GASP_PAR_1|T_treatment,data=dg.m,type="p",col=c("blue","red"),lty=1,lwd=1,legend=F,
       axes=F,xlab="",ylab="",ylim=c(-0.5,10),xlim=c(0,2000))
plotBy(dTleaf_2~GASP_PAR_2|T_treatment,data=dg.m,type="p",col=c("forestgreen","orange"),lty=1,lwd=1,legend=F,add=T)
magaxis(side=c(1,2),labels=c(1,1),las=1,frame.plot=T)
title(xlab=expression(Leaf~PPFD~(mu*mol~m^-2~s^-1)),xpd=NA,cex.lab=1.2)
title(ylab=expression(Delta~T[leaf]~(degree*C)),xpd=NA,cex.lab=1.2)


#- plot again, but colored by teh hour of the day
COL <- blue2red(24)
dg.m$hour<- hour(dg.m$DateTime_hr)
plotBy(dTleaf_1~GASP_PAR_1|hour,data=subset(dg.m,chamber=="C01"),type="p",col=COL,pch=16,lwd=1,legend=F,
       axes=F,xlab="",ylab="",ylim=c(-0.5,10),xlim=c(0,2000))
#plotBy(dTleaf_2~GASP_PAR_2|hour,data=subset(dg.m,chamber=="C10"),type="p",col=COL,pch=16,lwd=1,legend=F,add=T,
#      axes=F,xlab="",ylab="",ylim=c(-0.5,10),xlim=c(0,2000))

magaxis(side=c(1,2),labels=c(1,1),las=1,frame.plot=T)
title(xlab=expression(Leaf~PPFD~(mu*mol~m^-2~s^-1)),xpd=NA,cex.lab=1.2)
title(ylab=expression(Delta~T[leaf]~(degree*C)),xpd=NA,cex.lab=1.2)
legend("topleft",levels(as.factor(dg.m$hour)),col=COL,ncol=4,pch=16,title="Hour")
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

