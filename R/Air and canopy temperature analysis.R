  
source("R/loadLibraries.R")


#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation

#- search for and download the air variable dataset from the WTCs. Contains the leaf temperature data.
#- Note that I use the cachefile to reduce the time it takes to re-run this code.
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
d <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2016-1-1", endDate="2016-11-25",
                  topath="C:/Repos/wtc4_flux/data/fromHIEv",
                  cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache.rdata")
d$chamber <- as.factor(substr(d$Source,start=10,stop=12)) # extract the chamber number from the filename
d$T_treatment <- ifelse(as.numeric(substr(as.character(d$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
d$Tdiff <- with(d,TargTempC_Avg-SBTempC_Avg)
d$DateTime_hr <- nearestTimeStep(d$DateTime, nminutes = 15, align = "floor")

#- write out a csv for Richard
#towrite <- d[,c("DateTime","chamber","T_treatment","PPFD_Avg","SBTempC_Avg","TargTempC_Avg")]
#write.csv(towrite,"output/WTC4_AIRVARS.csv",row.names=F)
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
metdat$DateTime_hr <- nearestTimeStep(metdat$DateTime, nminutes = 15, align = "floor")

key <- data.frame(T_treatment=rep(c("ambient","elevated"),6),
                  chamber=levels(d$chamber))
metdat <- merge(metdat,key,by="chamber")
metdat <- metdat[with(metdat,order(chamber,DateTime)),]

#- write out a csv for Richard
#towrite <- metdat[,c("DateTime","chamber","T_treatment","Tair_al","RH_al")]
#write.csv(towrite,"output/WTC4_WTCMET.csv",row.names=F)
#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- notes
#- some data will need to be cleaned on June 4th (huge rainstorm and power failure)

#- Also note that Angelica, Sebastian, and I moved the IR sensors on Thursday 23rd June 2016. The sensors
#    were mounted on the northern side of each chamber and look in at an area of dense canopy.
#  Angelica and I moved the sensors again on 12 Sept 2016. The trees had grown, so we moved the sensors up.
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- Average the leaf and body temperatures for each hour, plot difference relative to PFPD.
require(dplyr)
t3 <- as.data.frame(summarize(group_by(d, chamber, T_treatment,DateTime_hr), 
                                 PPFD=mean(PPFD_Avg),
                                 TargTempC_Avg=mean(TargTempC_Avg),
                                  SBTempC_Avg=mean(SBTempC_Avg)))
t3$Tdiff <- with(t3,TargTempC_Avg-SBTempC_Avg)

# #- average temperature from ROS-MET
# met.hr <- as.data.frame(summarize(group_by(met, DateTime_hr), 
#                                   AirTC_Avg=mean(AirTC_Avg),
#                                   PPFD_Avg=mean(PPFD_Avg)))

#- average temperature from WTC-MET
met.hr <- as.data.frame(summarize(group_by(metdat, chamber, T_treatment, DateTime_hr), 
                                  Tair_al=mean(Tair_al),
                                  RH_al=mean(RH_al)))

t4 <- merge(t3,met.hr,by=c("chamber","T_treatment","DateTime_hr"))
t4$Tdiff_met <- with(t4,TargTempC_Avg-Tair_al)

library(plantecophys)
t4$VPD <- RHtoVPD(RH=t4$RH_al,TdegC=t4$Tair_al)

#- It looks like the sensor body temperature is an insufficient measure of air temperature
#- leaves tend to be a lot cooler than air temperature, particularly at high PPFD... strange
toplot <- subset(t4,as.Date(DateTime_hr) > as.Date("2016-3-21"))

windows(60,40);par(mfrow=c(1,1),cex.lab=2.5,mar=c(6,6,1,1))
# plotBy(Tdiff~PPFD|T_treatment,data=subset(toplot,PPFD>0.4),type="p",ylim=c(-12,12),xlim=c(0,1500),
#       xlab="Incident PAR (umol m-2 s-1)",ylab="(IR-T) - (IR-body T)")
# abline(h=0)
plotBy(Tdiff_met~PPFD|T_treatment,data=subset(toplot,PPFD>0.4),type="p",ylim=c(-5,10),xlim=c(0,2000),
      xlab="Incident PAR (umol m-2 s-1)",ylab="Tleaf-Tair")
abline(h=0)

plotBy(TargTempC_Avg~Tair_al|T_treatment,data=t4,pch=1,col=c("black","red"),cex=0.5,
       xlab=expression(T[air]~(degree*C)),
       ylab=expression(T[IR]~(degree*C)))
abline(0,1,lwd=3)
abline(5,0.7,lwd=3,lty=2)
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- make plot over time
require(calibrate)
toplot <- subset(t4,as.Date(DateTime_hr) > as.Date("2016-10-15")  & as.Date(DateTime_hr) < as.Date("2016-11-20") )
                 #& T_treatment=="ambient")

windows(80,60);par(mfrow=c(2,1),mar=c(2,6,1,1),oma=c(2,1,1,1),cex.lab=1.5)
plotBy(PPFD~DateTime_hr|T_treatment,data=toplot,type="l",ylim=c(0,2000),legend=F,
       ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),xlab="")
plotBy(Tair_al~DateTime_hr|T_treatment,data=toplot,type="l",legend=F,ylim=c(0,40),lwd=3,
       ylab=expression(T~(degree*C)))
plotBy(TargTempC_Avg~DateTime_hr|T_treatment,data=toplot,type="l",legend=F,lwd=1,add=T,col=c("blue","orange"),
       ylab=expression(T~(degree*C)))
legend("bottomleft",lwd=2,col=c("black","red","blue","orange"),legend=c("Tair(A)","Tair(W)","IR(A)","IR(W)"),ncol=2)


#-----------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------
# Histogram Colored (blue and red)
amb <- subset(t4,T_treatment=="ambient" & as.Date(DateTime_hr)>as.Date("2016-6-21"))
wrm <- subset(t4,T_treatment=="elevated" & as.Date(DateTime_hr)>as.Date("2016-6-21"))

windows(80,160);par(mfrow=c(3,1),mar=c(2,6,1,1),oma=c(4,1,1,1),cex.lab=1.5)

hist(amb$Tair_al, col=rgb(0,0,1,0.5),xlim=c(0,40), freq=F,ylim=c(0,0.1),main="Ambient")
hist(amb$TargTempC_Avg, col=rgb(1,0,0,0.5),xlim=c(0,40), freq=F,ylim=c(0,0.1),add=T)
legend("topright",fill=c(rgb(0,0,1,0.5),rgb(1,0,0,0.5)),legend=c("T-air","T-IR"))
box()
hist(wrm$Tair_al, col=rgb(0,0,1,0.5),xlim=c(0,40), freq=F,ylim=c(0,0.1),main="Warmed")
hist(wrm$TargTempC_Avg, col=rgb(1,0,0,0.5),xlim=c(0,40), freq=F,ylim=c(0,0.1),add=T)
box()
title(xlab=expression(Temperature~(degree*C)),outer=T,cex.lab=2)

hist(amb$Tdiff_met, col=rgb(0,0,1,0.5),xlim=c(-5,10), freq=F,ylim=c(0,1))
hist(wrm$Tdiff_met, col=rgb(1,0,0,0.5),xlim=c(-5,10), freq=F,ylim=c(0,1),add=T)

#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- plot a focal day (June 25th)
require(calibrate);require(doBy);require(RColorBrewer)
toplot <- subset(t4,as.Date(DateTime_hr) == as.Date("2016-10-26"))
#toplot <- summaryBy(.~DateTime_hr+T_treatment,data=toplot,FUN=mean,keep.names=T,na.rm=T)
palette(c("black",brewer.pal(11,"Spectral")))

windows(80,130);par(mfrow=c(3,1),mar=c(2,6,1,1),oma=c(2,1,1,1),cex.lab=1.5)
plotBy(PPFD~DateTime_hr|chamber,data=toplot,type="l",ylim=c(0,2000),legend=F,
       ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),xlab="")
legend("topright",lwd=2,col=palette()[1:12],lty=c(1),legend=levels(toplot$chamber),ncol=2)

title(main=as.Date(toplot$DateTime_hr[1]))
plotBy(Tair_al~DateTime_hr|chamber,data=toplot,type="l",legend=F,ylim=c(0,35),lwd=2,
       ylab=expression(T~(degree*C)))
plotBy(TargTempC_Avg~DateTime_hr|chamber,data=toplot,type="l",legend=F,lwd=2,lty=2,add=T,
       ylab=expression(T~(degree*C)))
#legend("topright",lwd=2,col=c("black","red"),lty=c(1,1,2,2),legend=c("T-air (A)","T-air (E)","T-IR (A)","T-IR (E)"),ncol=2)
plotBy(Tdiff_met~DateTime_hr|chamber,data=toplot,type="l",legend=F,lwd=2,lty=1,add=F,ylim=c(-2,5),
       ylab=expression(T-IR~-~T-Air~(degree*C)));abline(h=0)



#-----------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------------------------
#- simulate the leaf temperature of these environmental condutions
toplot <- subset(t4,as.Date(DateTime_hr) == as.Date("2016-6-14") &  PPFD > 4)

sim <- PhotosynEB(Tair=toplot$Tair_al,PPFD=toplot$PPFD,VPD=toplot$VPD,
                              Wind=1.5,Wleaf=0.02,g1=3)
fails <- which(sim$failed==T)

toplot$Tleafsim <- sim$Tleaf2
toplot$Tleafsim[fails] <- NA
toplot$Tleafsimdif <- with(toplot,Tleafsim-Tair_al)


#- replot a focal day (May 27h), with energy balance simulations
require(calibrate);require(doBy)
toplot.m <- summaryBy(.~DateTime_hr+T_treatment,data=toplot,FUN=mean,keep.names=T,na.rm=T)


windows(80,130);par(mfrow=c(3,1),mar=c(2,6,1,1),oma=c(2,1,1,1),cex.lab=1.5)
plotBy(PPFD~DateTime_hr|T_treatment,data=toplot.m,type="l",ylim=c(0,1500),legend=F,
       ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),xlab="")
title(main=as.Date(toplot$DateTime_hr[1]))
plotBy(Tair_al~DateTime_hr|T_treatment,data=toplot.m,type="l",legend=F,ylim=c(5,30),lwd=2,
       ylab=expression(T~(degree*C)))
plotBy(TargTempC_Avg~DateTime_hr|T_treatment,data=toplot.m,type="l",legend=F,lwd=2,lty=2,add=T,
       ylab=expression(T~(degree*C)))
plotBy(Tleafsim~DateTime_hr|T_treatment,data=toplot.m,type="l",legend=F,lwd=2,lty=3,add=T,
       ylab=expression(T~(degree*C)))
legend("bottomright",lwd=2,col=c("black","red"),lty=c(1,1,2,2,3,3),
       legend=c("T-air (A)","T-air (E)","T-IR (A)","T-IR (E)","T-mod (A)","T-mod (E)"),ncol=3)
plotBy(Tdiff_met~DateTime_hr|T_treatment,data=toplot.m,type="l",legend=F,lwd=2,lty=1,add=F,ylim=c(-2,8),
       ylab=expression(T-IR~-~T-Air~(degree*C)));abline(h=0)
plotBy(Tleafsimdif~DateTime_hr|T_treatment,data=toplot.m,type="l",legend=F,lwd=2,lty=2,add=T,ylim=c(-2,8),
       ylab=expression(T-IR~-~T-Air~(degree*C)));abline(h=0)
legend("top",lwd=2,col=c("black","red"),lty=c(1,1,2,2),
       legend=c("dT-meas (A)","dT-meas (E)","dT-mod (A)","dT-mod (E)"),ncol=2)

#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- pairs plot
library(plantecophys)
library(rgl)
toplot <- subset(t4,PPFD>10 & RH_al<100 & as.Date(t4$DateTime_hr) > as.Date("2016-04-05"))
toplot$VPD <- RHtoVPD(RH=toplot$RH_al,TdegC=toplot$Tair_al)

pairs(toplot[,c("PPFD","Tdiff_met","TargTempC_Avg","Tair_al","RH_al","VPD")])
plot3d(x=toplot$PPFD,y=toplot$VPD,z=toplot$Tdiff_met,col=palette()[as.factor(toplot$T_treatment)])






#----------------------------------------------------------------------------------------------------------------
#- plot heat-map style hexagons for Tdiff relative to PAR and VPD


#creates a scale of colors
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  #v <- (values - -3)/10 
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

toplot <- subset(t4,as.Date(DateTime_hr) > as.Date("2016-7-1"))

all <- toplot

#- create hex bins
library(hexbin)
hall <- hexbin(all$PPFD, all$VPD,xbins=20,IDs=TRUE)



#average values of points inside hexbins 
meanHexBinall<-data.frame(mean=hexTapply(hall, all$Tdiff_met, mean)) 

# #- find the NUMBER of points per hexbin. Remove hexagons with fewer than 5 points?
# NumHexBinall<-data.frame(number=hexTapply(hall, all$Tdiff_met, length)) 
# torm <- as.numeric(which(NumHexBinall$number < 10))
# meanHexBinall[torm,] <- NA
# meanHexBinall <- meanHexBinall[complete.cases(meanHexBinall),]

colsall <- myColorRamp(c("blue","green","yellow", "red"), meanHexBinall$mean)


#-- plot 
windows()

## setup coordinate system of the plot
par(cex.lab=2,cex.axis=2)
Pa <- plot(hall, type="n",legend=FALSE,xlab="PPFD",ylab="VPD",main="")

##add hexagons (in the proper viewport):
pushHexport(Pa$plot.vp)

#plots hexbins based on colors of third column
#grid.hexagons(hall, style= "lattice", border = gray(.9), pen = colsall,  minarea = 1, maxarea = 1)

grid.hexagons(hall, style= "lattice", border = gray(.9), pen = colsall,  minarea = 1, maxarea = 1)

#- Add a legend
windows()
values <- seq(-3,6,length=10)
colors <- myColorRamp(c("blue","green","yellow", "red"), values)
xloc <- rep(1,10)
yloc <- c(1,2,3,4,5,6,7,8,9,10)
plot(yloc~xloc,pch=18,cex=5,col=colors,ylim=c(0,10),xaxt="n",yaxt="n",xlab="",ylab="")
text(xloc+0.1,yloc,labels=values,cex=1.5)
#dev.copy2pdf(file="/output/Figure6_legend.pdf")
#----------------------------------------------------------------------------------------------------------------







#----------------------------------------------------------------------------------------------------------------
#- use energy balance. Does this make sense?


#creates a scale of colors
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  #v <- (values - -3)/10 
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}


#- simulate leaf energy balance across a range of VPDs and PPFDs
dat <- expand.grid(PPFD=seq(0,1500,length.out=31),VPD=seq(0,2,length.out=31))
sim  <- PhotosynEB(Tair=20,PPFD=dat$PPFD,VPD=dat$VPD,Wind=0.5,Wleaf=0.02,g1=4)
dat$Tleaf <- sim$Tleaf2
dat$Tleafdiff <- with(dat,Tleaf-20)

#- create hexbins
hall_sim <- hexbin(dat$PPFD, dat$VPD,xbins=20,IDs=TRUE)

cells_a <- hcell2xy(hall_sim)
hexd_a <- (h_a@xbnds[2] - h_a@xbnds[1])/h_a@xbins
nhex_a <- h_a@ncells
d_a <- getdiams(cells_a)

#average values of points inside hexbins 
meanHexBinall<-data.frame(mean=hexTapply(hall_sim, dat$Tleafdiff, mean)) 

colsall <- myColorRamp(c("blue","green","yellow", "red"), meanHexBinall$mean)


#-- plot 
windows()
par(mar=c(6,6,6,1))

# Plot hex
plot(cells_a, type='n',xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,2))
for(i in 1:nhex_a){
  Hexagon(cells_a$x[i], cells_a$y[i], xdiam=d_a$xdiam*2, ydiam=d_a$ydiam,
          border=NA,#border="grey",
          col=colsall[i])
}
magaxis(side=c(1,2,3,4),las=1,labels=c(1,0,0,0),cex.axis=1.5)
axis(side=2,at=c(0,1,2),labels=c(0,1,2),las=1,cex.axis=1.5,tick=F)
title(xlab=expression(PPFD~(mu*mol~m^-2~s^-1)), ylab=expression(VPD~(kPa)),cex.lab=1.75,line=4,xpd=NA)

# add legend to top of the first two panels
values <- c(-1,0,1,2,3,5)
legcolors <- myColorRamp(c("blue","green","yellow", "red"), values)[1:6]
xloc <- seq(0,1200,length.out=length(values))
yloc <- rep(2.2,length(values))
points(yloc~xloc,pch=18,cex=4,col=legcolors,xaxt="n",yaxt="n",xlab="",ylab="",xpd=NA)
text(xloc,yloc+3,labels=values[1:6],cex=1.5,xpd=NA)
text(xloc+100,yloc,labels=values,cex=1.5,xpd=NA)
text(100,yloc+0.2,expression(T[leaf]-T[air]*", "(degree*C)),xpd=NA,cex=1.5)



# 
# ## setup coordinate system of the plot
# par(cex.lab=2,cex.axis=2)
# Pa <- plot(hall_sim, type="n",legend=FALSE,xlab="PPFD",ylab="VPD",main="")
# 
# ##add hexagons (in the proper viewport):
# pushHexport(Pa$plot.vp)
# 
# #plots hexbins based on colors of third column
# #grid.hexagons(hall, style= "lattice", border = gray(.9), pen = colsall,  minarea = 1, maxarea = 1)
# 
# grid.hexagons(hall_sim, style= "lattice", border = gray(.9), pen = colsall,  minarea = 1, maxarea = 1)
#----------------------------------------------------------------------------------------------------------------
