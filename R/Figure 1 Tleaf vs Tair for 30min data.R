#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- The goal here is to plot ALL of the data at a short time-scale for Tleaf vs. Tair.
#  This differs from some other code that merges specific time-frames of data for specific purposes
#  (such as to combine the thermocouples and infrared datasets)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")





#-----------------------------------------------------------------------------------------------------------
#- download the AIRVARS data (PPFD and IR-T data), do some manipulation
files <- searchHIEv("WTC_AUTO_C[0-9]{2}_AIRVARS")
IRT <- downloadTOA5("WTC_AUTO_C[0-9]{2}_AIRVARS", startDate="2015-10-28", endDate="2016-11-28",
                    topath="C:/Repos/wtc4_flux/data/fromHIEv",
                    cachefile="C:/Repos/wtc4_flux/data/fromHIEv/wtc4cache.rdata")
IRT$chamber <- as.factor(substr(IRT$Source,start=10,stop=12)) # extract the chamber number from the filename
IRT$T_treatment <- ifelse(as.numeric(substr(as.character(IRT$chamber),start=2,stop=3)) %% 2 == 0, "elevated","ambient")
IRT$Tdiff <- with(IRT,TargTempC_Avg-SBTempC_Avg)
IRT$DateTime_hr <- nearestTimeStep(IRT$DateTime, nminutes = 15, align = "floor")
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#- load the trendlogs
source("C:/Repos/wtc4_flux_processing/R/getWTCtrendlog.R")
load("C:/Repos/wtc4_flux_processing/data/TrendlogChDF.RData")
load("C:/Repos/wtc4_flux_processing/data/TrendlogRefDF.RData")
Trend.dat <- subset(TrendlogChDF,as.Date(DateTime)>=as.Date("2015-10-28") & as.Date(DateTime)<=as.Date("2016-11-28"))
Trend.dat$DateTime_hr <- nearestTimeStep(Trend.dat$DateTime, nminutes = 15, align = "floor")
#-----------------------------------------------------------------------------------------------------------


#--- okay, the datasets above now need to be merged
IRT.dat.m <- data.frame(dplyr::summarize(dplyr::group_by(IRT, DateTime_hr, chamber), 
                                         TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                         PPFD_Avg=mean(PPFD_Avg,na.rm=T)))


Trend.dat <- Trend.dat[,c("chamber","DateTime_hr","Tair_al","RH_al")]
Trend.dat.m <- data.frame(dplyr::summarize(dplyr::group_by(Trend.dat, DateTime_hr, chamber), 
                                           Tair_al =mean(Tair_al ,na.rm=T),
                                           RH_al=mean(RH_al,na.rm=T)))
Trend.dat.m$VPD <- RHtoVPD(RH=Trend.dat.m$RH_al,TdegC=Trend.dat.m$Tair_al)

dat2 <- merge(Trend.dat.m,IRT.dat.m,by=c("chamber","DateTime_hr"))


#-- pull out some clear outliers
dat2$Tdiff <- dat2$TargTempC_Avg - dat2$Tair_al
tonafill <- c(which(dat2$Tdiff < -6),which(dat2$Tdiff > 10))
dat2$TargTempC_Avg[tonafill] <- NA
dat2$Tdiff[tonafill] <- NA

#-----------------------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#-- Figure 2

#- plot leaf temperatures relative to air temperatures for each 30-minute period
windows(100,35)
par(mfrow=c(1,3),las=1,cex.lab=1.75,mar=c(6,6,1,1),oma=c(0,0,6,0))
lims <- c(-2,45)
parlimit=0

#- set the color palette
greys.ramp <- colorRampPalette(c("whitesmoke","blue", "red"),alpha=0.7)

#- create color densities
dc.al <- densCols(dat2$TargTempC_Avg,dat2$Tair_al,
                  colramp=greys.ramp)
#- plot covariance between temperature measurements
plot(TargTempC_Avg~Tair_al,data=dat2,ylim=lims,xlim=lims,pch=16,
     col=dc.al,
     xlab=expression(Air~temperature~(T[air]*","~degree*C)),
     ylab=expression(Infrared~leaf~temperature~(T[L-IR]*","~degree*C)))
abline(0,1,col="black",lty=2)
lm3 <- lm(TargTempC_Avg~Tair_al+I(Tair_al^2),data=subset(dat2,PPFD_Avg>parlimit))
predictions <- predict.lm(lm3,newdat=data.frame(Tair_al=seq(-2,45,length.out=101)),se.fit=T,interval="confidence")

lines(predictions[[1]][,1] ~ seq(-2,45,length.out=101),
      lwd=2.5,col="black")
legend("topleft",legend=letters[1],cex=1.4,bty="n")

#- slope of line from Michaletz?
#abline(5,0.75,lty=3,lwd=2,col="grey")

#- create a legend
dc.leg <- densCols(rep(c(1,rep(20,20),rep(100,100),rep(1000,1000)),2),
                  colramp=greys.ramp)
points(x=c(0,5,10,15.5),y=rep(49,4),col="darkgrey",bg=c(dc.leg[1],dc.leg[2],dc.leg[50],dc.leg[1000]),
       pch=21,xpd=NA,cex=2)
graphics::text(x=c(-6,-1,4,10,16)+2.5,y=rep(49,4),labels=c("#",1,10,100,1000),xpd=NA,cex=1.5)

#---- create second plot for Temperature difference
# Make a simple histogram

plot <- hist(dat2$Tdiff,xlab=expression(Leaf~to~air~temperature~difference~(T[L-IR]~"-"~T[air]*","~degree*C)),
             main="",prob=T)
box()
legend("topleft",legend=letters[2],cex=1.4,bty="n")

#- how often is Tleaf within 1 deg C of Tair?
within1 <- nrow(subset(dat2,Tdiff >= -1 & Tdiff <= 1))
nrow(dat2)
within1/nrow(dat2)
range(dat2$Tdiff,na.rm=T)

# #- create color densities
# xlims <- c(-5,45)
# ylims <- c(-5,10)
# dc.diff <- densCols(subset(dat2,PPFD_Avg>parlimit)$Tdiff,subset(dat2,PPFD_Avg>parlimit)$Tair_al,
#                   colramp=greys.ramp)
# #- plot covariance between temperature measurements
# plot(Tdiff~Tair_al,data=subset(dat2,PPFD_Avg>parlimit),ylim=ylims,xlim=xlims,pch=16,
#      col=dc.diff,
#      xlab=expression(Air~temperature~(T[air]*","~degree*C)),
#      ylab=expression(Leaf~to~air~temperature~difference~(T[L-IR]~"-"~T[air]*","~degree*C)))
# abline(0,0,col="grey",lty=2)
# lm4 <- lm(Tdiff~Tair_al+I(Tair_al^2),data=subset(dat2,PPFD_Avg>parlimit))
# predictions <- predict.lm(lm4,newdat=data.frame(Tair_al=seq(-2,45,length.out=101)),se.fit=T,interval="confidence")
# legend("topleft",legend=letters[2],cex=1.4,bty="n")
# 
# lines(predictions[[1]][,1] ~ seq(-2,45,length.out=101),lwd=2.5,col="black")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot hexagons for Tdiff

#creates a scale of colors
myColorRamp_raw <- function(colors, values) {
  #v <- (values - min(values))/diff(range(values))
  v <- (values + 5)/11
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
library(hexbin)
source("R/hexagon_plotting_functions.R")




#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#- plot heat-map style hexagons for Tdiff relative to PAR and Transpiration



#- minimum number of observations per bin to be displayed
countmin <- 20

#- number of bins in each direction
nbins <- 40 # 10



#-------------- 
#average values of points inside hexbins
combodat_light <- subset(dat2, is.na(Tdiff)==F & VPD>0 & PPFD_Avg>=0)

h_a <- hexbin(combodat_light$PPFD_Avg, combodat_light$VPD,xbins=nbins,IDs=TRUE)
#xbnds=c(7.9,31),ybnds=c(1.2,38)) # note the dimensions only work for some xbins values (21)

cells_a <- hcell2xy(h_a)
hexd_a <- (h_a@xbnds[2] - h_a@xbnds[1])/h_a@xbins
nhex_a <- h_a@ncells
d_a <- getdiams(cells_a)
#d[2] <- 0.005

rampcolors <- c("black","blue","yellow","red")


meanHexBinamb<-data.frame(mean=hexTapply(h_a, combodat_light$Tdiff, mean)) 

#- find the bins with fewer than the critical number of observations
lengthHexBinamb<-data.frame(mean=hexTapply(h_a, combodat_light$Tdiff, length)) 
hexCols_a[which(lengthHexBinamb<countmin)] <- NA

hexCols_a <- myColorRamp_raw(rampcolors, meanHexBinamb$mean)
hexCols_a[which(lengthHexBinamb<countmin)] <- NA



# Plot hex
plot(cells_a, type='n',xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,4))
for(i in 1:nhex_a){
  Hexagon(cells_a$x[i], cells_a$y[i], xdiam=d_a$xdiam*2, ydiam=d_a$ydiam,
          border=NA,#border="grey",
          col=hexCols_a[i])
}
magaxis(side=c(1,2,3,4),las=1,labels=c(1,0,0,0),cex.axis=1.5)
axis(side=2,at=c(0,1,2,3,4),labels=c(0,1,2,3,4),las=1,cex.axis=1.5,tick=F)
title(xlab=expression(PPFD~(mu*mol~m^-2~s^-1)), ylab=expression(VPD~(kPa)),cex.lab=1.75,line=4,xpd=NA)

# add legend to top of the first two panels
values <- c(-1,0,1,2,3)
legcolors <- myColorRamp_raw(rampcolors, values)[1:6]
xloc <- seq(500,1700,length.out=length(values))
yloc <- rep(4.4,length(values))
points(yloc~xloc,pch=18,cex=4,col=legcolors,xaxt="n",yaxt="n",xlab="",ylab="",xpd=NA)
text(xloc,yloc+3,labels=values[1:6],cex=1.5,xpd=NA)
text(xloc+100,yloc,labels=values,cex=1.5,xpd=NA)
text(100,yloc,expression(T[L-IR]-T[air]*", "(degree*C)),xpd=NA,cex=1.5)
legend("topright",legend=letters[3],cex=1.4,bty="n")
