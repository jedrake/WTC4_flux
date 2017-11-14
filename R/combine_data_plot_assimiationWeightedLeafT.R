#-----------------------------------------------------------------------------------------------------------
#- Combine leaf temperatures and flux measurements to calculate
#    assimilation-weighted leaf temperatures.
#  Some of this code was copied over and simplified from "leaf thermocouples.R"
#-----------------------------------------------------------------------------------------------------------

source("R/loadLibraries.R")
library(merTools)



#-----------------------------------------------------------------------------------------------------------
#- merge the IR temperatures with the leaf temperatures


#- Read in the the leaf temperature data (i.e., the actual thermocouples!)
#   Recall that these thermocouples were installed in chambers 1, 2, 5, 9, 10, and 12
#   Thermocouples were installed in all chambers on 27 June 2016
leafT <- as.data.frame(data.table::fread("Output/WTC_TEMP-PARRA_LEAFT-THERMOCOUPLE_20160702-20161125_L0.csv"))
leafT$DateTime <- as.POSIXct(leafT$DateTime,format="%Y-%m-%d %T",tz="GMT")

#- read in the AIRVARS data (PPFD and IR-T data)
IRT <-as.data.frame(data.table::fread("Output/WTC_TEMP-PARRA_LEAFT-IR_2016010-20161125_L0.csv"))
IRT$DateTime <- as.POSIXct(IRT$DateTime,format="%Y-%m-%d %T",tz="GMT")

IRTsub <- IRT[,c("DateTime","TargTempC_Avg","PPFD_Avg","chamber")]
IRTsub$DateTime <- nearestTimeStep(IRTsub$DateTime,nminutes=15,align="floor")

IRTsub.m <- data.frame(dplyr::summarize(dplyr::group_by(IRTsub, DateTime, chamber), 
                                        TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                        PPFD_Avg=mean(PPFD_Avg,na.rm=T)))

leafTsub <- leafT[,c("DateTime","chamber","LeafT1","LeafT2")]
leafTsub$DateTime <- nearestTimeStep(leafTsub$DateTime,nminutes=15,align="floor")

leafTsub.m <- data.frame(dplyr::summarize(dplyr::group_by(leafTsub, DateTime, chamber), 
                                       LeafT_Avg.1.=mean(LeafT1,na.rm=T),
                                       LeafT_Avg.2.=mean(LeafT2,na.rm=T)))
d3 <- merge(leafTsub.m,IRTsub.m,by=c("DateTime","chamber"),all.y=T)
chamber_n <- as.numeric(substr(d3$chamber,start=2,stop=3))
d3$T_treatment <- ifelse(chamber_n %% 2 == 0, "elevated","ambient")

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
#- read in the flux data (processed by the wtc4_flux_processing R project)
fluxdat <- read.csv("c:/Repos/wtc4_flux_processing/output/WTC_TEMP-PARRA_WTCFLUX_20160228-20161123_L0.csv")
fluxdat$DateTime <- as.POSIXct(fluxdat$DateTime,format="%Y-%m-%d %T",tz="UTC")
fluxdat$VPD <- RHtoVPD(RH=fluxdat$RH_al,TdegC=fluxdat$Tair_al)

#- subset to just the data corresponding to the dates in the d4 dataframe
starttime <- min(d4$DateTime)
endtime <- max(d4$DateTime)
fluxdat2 <- subset(fluxdat,DateTime>starttime & DateTime < endtime)

#- 30-min averages across treatments
fluxdat2$DateTime_hr <- nearestTimeStep(fluxdat2$DateTime,nminutes=30,align="floor")
fluxdat.hr1 <- summaryBy(FluxCO2+FluxH2O+Tair_al+PAR+VPD~DateTime_hr+chamber+T_treatment,
                         data=subset(fluxdat2,DoorCnt==0),FUN=mean,na.rm=T,keep.names=T)
fluxdat.hr <- summaryBy(.~DateTime_hr+T_treatment,data=fluxdat.hr1,FUN=c(mean,standard.error),na.rm=T,keep.names=F)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- merge fluxdata.hr and IRT (fluxes and temperatures).
#   This is for the "thermoregulation" manuscript


head(fluxdat.hr1)
head(IRT)
#IRT$DateTime_hr <- nearestTimeStep(IRT$DateTime,nminutes=30,align="floor")
d4$DateTime_hr <- nearestTimeStep(d4$DateTime,nminutes=30,align="floor")
#IRT.hr1 <- summaryBy(TargTempC_Avg~DateTime_hr+chamber+T_treatment,
#                         data=subset(IRT,DateTime>starttime & DateTime<endtime),FUN=mean,na.rm=T,keep.names=T)

d4.hr1 <- data.frame(dplyr::summarize(dplyr::group_by(d4, DateTime_hr, chamber,T_treatment), 
                                       TargTempC_Avg=mean(TargTempC_Avg,na.rm=T),
                                       PPFD_Avg=mean(PPFD_Avg,na.rm=T),
                                       LeafT_Avg.1.=mean(LeafT_Avg.1.,na.rm=T),
                                       LeafT_Avg.2.=mean(LeafT_Avg.2.,na.rm=T)))
                                      

combodat <- merge(fluxdat.hr1,d4.hr1,by=c("DateTime_hr","T_treatment","chamber"))
combodat$week <- factor(week(combodat$DateTime_hr))
combodat$weekfac <- factor(paste(combodat$chamber,combodat$week,sep="-"))



#-----------------------------------------------------------------------------------------------------------
#- plot all of the leaf temperature vs. air temperature data
# windows(75,75);par(mar=c(7,7,1,2),mfrow=c(1,1))
# 
# greys.ramp <- colorRampPalette(c("lightgrey", "black"))
# parlimit <- 500
# #- create color densities
# dc1 <- densCols(subset(combodat,PPFD_Avg>parlimit)$TargTempC_Avg,subset(combodat,PPFD_Avg>parlimit)$Tair_al,
#                 colramp=greys.ramp)
# 
# #palette(c("blue","red"))
# plot(TargTempC_Avg~Tair_al,data=subset(combodat,PPFD_Avg>parlimit),
#      col=dc1,
#      pch=1,xlim=c(0,50),ylim=c(0,50),legend=F,axes=F,
#        xlab="",ylab="")
# abline(0,1,lty=2)
# abline(-5,1,lty=2)
# abline(+5,1,lty=2)
# abline(+10,1,lty=2)
# textxy(X=0,Y=14,labs=expression("+10"~degree*C),cex=0.75)
# textxy(X=0,Y=8,labs=expression("+5"~degree*C),cex=0.75)
# textxy(X=0,Y=0,labs=expression("+0"~degree*C),cex=0.75)
# textxy(X=4,Y=-0.5,labs=expression("-5"~degree*C),cex=0.75)
# lmTall <- lm(TargTempC_Avg~Tair_al,data=subset(combodat,PPFD_Avg>parlimit))
# abline(lmTall,lty=1,lwd=1.7)
# legend("bottomright",paste("Slope = ",round(coef(lmTall)[[2]],2),sep=""),bty="n")
# magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
# title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
# title(ylab=expression(Infrared~leaf~temperature~(T[l-IR]~","~~degree*C)),xpd=NA,cex.lab=2)
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
# SMATR statistical analysis of Tleaf vs. Tair
#sma1 <- sma(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment,
#            data=output_meanT) # treats each observation as independent, which inflates the statistical power

#-------------
#- random effects ANCOVA for Tleaf vs. Tair. Some evidence that the warmed treatment had a lower slope
#    but both slope 95% CI's included 1.0.
lme1 <- lmer(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
anova(lme1) # some evidence of difference in slope, but not terribly strong (p = 0.02)
confint(lme1)

modelout <- data.frame(summary(lme1)$coefficients)
ambCI <- c(modelout$Estimate[[2]]-modelout$Std..Error[[2]]*1.96,modelout$Estimate[[2]]+modelout$Std..Error[[2]]*1.96)
eleCI <- c((modelout$Estimate[[2]]+modelout$Estimate[[4]])-(modelout$Std..Error[[4]]*1.96),
           (modelout$Estimate[[2]]+modelout$Estimate[[4]])+(modelout$Std..Error[[4]]*1.96))


lme1.test <- lmer(meanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme1.test2 <- lmer(meanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme1,lme1.test,lme1.test2) # simplest model is preferred
AIC(lme1,lme1.test,lme1.test2) # models have very similar AICs
confint(lme1.test2)

visreg(lme1,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme1.test,xvar="T_treatment")


#-------------
#- random effects ANCOVA for assimilation-weighted Tleaf vs. Tair
lme2 <- lmer(weightedMeanLeafT~T_treatment+meanAirT+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
lme2.test <- lmer(weightedMeanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme2.test2 <- lmer(weightedMeanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme2,lme2.test,lme2.test2)
AIC(lme2,lme2.test,lme2.test2) # simpler model is preferred from AIC and logLik bases

anova(lme2.test2) # some evidence of difference in slope, but not terribly strong (p = 0.05)


modelout2 <- data.frame(summary(lme2.test2)$coefficients)
confint(lme2.test2)
#ambCI <- c(modelout2$Estimate[[3]]-modelout2$Std..Error[[3]]*1.96,modelout2$Estimate[[3]]+modelout2$Std..Error[[3]]*1.96)
#eleCI <- c((modelout2$Estimate[[3]]+modelout2$Estimate[[4]])-(modelout2$Std..Error[[4]]*1.96),
#           (modelout2$Estimate[[3]]+modelout2$Estimate[[4]])+(modelout2$Std..Error[[4]]*1.96))

visreg(lme2.test2,xvar="meanAirT",overlay=T)
visreg(lme2,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme2,xvar="T_treatment")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- set up plot of Tleaf vs. Tair and assimilation-weighted Tleaf vs. Tair
windows(100,75)
par(mar=c(7,7,1,2),mfrow=c(1,2))
palette(c("blue","red"))
pchs=3
cexs=0.5

#------
#- plot Tleaf vs. Tair
mindate <- min(output_meanT$Date,na.rm=T)#as.Date("2016-05-01")
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),
       pch=pchs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,cex=cexs,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)


# lmT <- lm(meanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lmT,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lmT)[[2]],2),sep=""),bty="n")
legend("bottomright",pch=c(pchs,pchs),col=c("blue","red"),legend=c("Ambient","Warmed"))
legend("topleft",legend=letters[1],cex=1.2,bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(T[leaf]~(degree*C)~(measured)),xpd=NA,cex.lab=2)

#------
#- plot assimilation-weighted Tleaf vs. Tair
plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=pchs,cex=cexs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)

# lm1 <- lm(weightedMeanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lm1,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lm1)[[2]],2),sep=""),bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(Assimilation~weighted~T[leaf]~(degree*C)),xpd=NA,cex.lab=2)
legend("topleft",legend=letters[2],cex=1.2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- plot three example days. Low T, moderate T, extremely high T. Show divergent diurnal timecourses

lowTday <- as.Date("2016-09-30")
lowTdat <- subset(combodat,as.Date(DateTime_hr)==lowTday)

lowTdat.m1 <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr+T_treatment+chamber,FUN=mean,keep.names=T,data=lowTdat)
lowTdat.m <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr+T_treatment,FUN=c(mean,se),data=lowTdat.m1)

times <- subset(lowTdat.m,T_treatment=="ambient")$DateTime_hr # extract times for shadeNight


windows(60,100)
#par(mar=c(7,7,1,2),mfrow=c(2,1))
par(mar=c(2,2,1,2),oma=c(4,5,4,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2), 2, 2, byrow = F), 
       widths=c(1,1), heights=c(1,2))
times <- subset(lowTdat.m,T_treatment=="ambient")$DateTime_hr # extract times for shadeNight
with(subset(lowTdat.m,T_treatment=="ambient"),plot(DateTime_hr,PAR.mean,type="l",ylim=c(0,2000),
                                                   xlab="",ylab="",
                                                   panel.first=shadeNight(times)))
title(ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),line=3.5,xpd=NA)
par(new = T)
with(subset(lowTdat.m,T_treatment=="ambient"),plot(DateTime_hr,TargTempC_Avg.mean,
                                                   type="l",pch=16,xlab="",ylab="",col="red",ylim=c(0,45),axes=F))
axis(side=4,col="red",col.axis="red")
title(ylab=expression(T[l-IR]~(degree*C)),line=-26,xpd=NA,col.lab="red")

with(subset(lowTdat.m,T_treatment=="ambient"),plot(DateTime_hr,FluxCO2.mean,
                                                   type="b",pch=16, col="black",ylim=c(-0.05,0.2),legend=F,ylab="",
                                                   panel.first=shadeNight(times)))
adderrorbars(x=subset(lowTdat.m,T_treatment=="ambient")$DateTime_hr,
             y=subset(lowTdat.m,T_treatment=="ambient")$FluxCO2.mean,
             SE=subset(lowTdat.m,T_treatment=="ambient")$FluxCO2.se,direction="updown")
abline(h=0)
axis(side = 4)
title(ylab=expression(Net~CO[2]~flux~(mmol~CO[2]~s^-1)),line=3.5,xpd=NA)






modTday <- as.Date("2016-10-30")
modTdat <- subset(combodat,as.Date(DateTime_hr)==modTday)

modTdat.m1 <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr+T_treatment+chamber,FUN=mean,keep.names=T,data=modTdat)
modTdat.m <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr+T_treatment,FUN=c(mean,se),data=modTdat.m1)
times <- subset(modTdat.m,T_treatment=="ambient")$DateTime_hr # extract times for shadeNight

windows(60,100)
#par(mar=c(7,7,1,2),mfrow=c(2,1))
par(mar=c(2,2,1,2),oma=c(4,5,4,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2), 2, 2, byrow = F), 
       widths=c(1,1), heights=c(1,2))
times <- subset(modTdat.m,T_treatment=="ambient")$DateTime_hr # extract times for shadeNight
with(subset(modTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,PAR.mean,type="l",ylim=c(0,2000),
                                                   xlab="",ylab="",
     panel.first=shadeNight(times)))
title(ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),line=3.5,xpd=NA)
par(new = T)
with(subset(modTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,TargTempC_Avg.mean,
        type="l",pch=16,xlab="",ylab="",col="red",ylim=c(0,45),axes=F))
axis(side=4,col="red",col.axis="red")
title(ylab=expression(T[l-IR]~(degree*C)),line=-26,xpd=NA,col.lab="red")

with(subset(modTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,FluxCO2.mean,
       type="b",pch=16, col="black",ylim=c(-0.05,0.2),legend=F,ylab="",
       panel.first=shadeNight(times)))
adderrorbars(x=subset(modTdat.m,T_treatment=="ambient")$DateTime_hr+3600,
                        y=subset(modTdat.m,T_treatment=="ambient")$FluxCO2.mean,
                        SE=subset(modTdat.m,T_treatment=="ambient")$FluxCO2.se,direction="updown")
abline(h=0)
axis(side = 4)
title(ylab=expression(Net~CO[2]~flux~(mmol~CO[2]~s^-1)),line=3.5,xpd=NA)






hotTday <- as.Date("2016-11-01")
hotTdat <- subset(combodat,as.Date(DateTime_hr)==hotTday)
linkdf <- data.frame(chamber = levels(as.factor(hotTdat$chamber)),
                     HWtrt = c("C","C","HW","HW","C","C","HW","C","HW","HW","C","HW"))#swapped C12 and C08
hotTdat <- merge(hotTdat,linkdf)
hotTdat.m1 <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr+T_treatment+chamber,FUN=mean,keep.names=T,
                        data=subset(hotTdat,HWtrt=="HW"))
hotTdat.m <- summaryBy(FluxCO2+PAR+TargTempC_Avg~DateTime_hr,FUN=c(mean,se),data=hotTdat.m1)
times <- subset(hotTdat.m,T_treatment=="ambient")$DateTime_hr # extract times for shadeNight


windows(60,100)
#par(mar=c(7,7,1,2),mfrow=c(2,1))
par(mar=c(2,2,1,2),oma=c(4,5,4,4),cex.lab=1.6,las=1,cex.axis=1.2)
layout(matrix(c(1,2), 2, 2, byrow = F), 
       widths=c(1,1), heights=c(1,2))
with(subset(hotTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,PAR.mean,type="l",ylim=c(0,2000),
                                                   xlab="",ylab="",
                                                   panel.first=shadeNight(times)))
title(ylab=expression(PPFD~(mu*mol~m^-2~s^-1)),line=3.5,xpd=NA)
par(new = T)
with(subset(hotTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,TargTempC_Avg.mean,
                                                   type="l",pch=16,xlab="",ylab="",col="red",ylim=c(0,45),axes=F))
axis(side=4,col="red",col.axis="red")
title(ylab=expression(T[l-IR]~(degree*C)),line=-26,xpd=NA,col.lab="red")

with(subset(hotTdat.m,T_treatment=="ambient"),plot(DateTime_hr+3600,FluxCO2.mean,
                                                   type="b",pch=16, col="black",ylim=c(-0.05,0.2),legend=F,ylab="",
                                                   panel.first=shadeNight(times)))
adderrorbars(x=subset(hotTdat.m,T_treatment=="ambient")$DateTime_hr+3600,
             y=subset(hotTdat.m,T_treatment=="ambient")$FluxCO2.mean,
             SE=subset(hotTdat.m,T_treatment=="ambient")$FluxCO2.se,direction="updown")
abline(h=0)
axis(side = 4)
title(ylab=expression(Net~CO[2]~flux~(mmol~CO[2]~s^-1)),line=3.5,xpd=NA)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------




























#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-- Repeat the calculation of assimilation-weighted leaf temperature, but on a weekly timescale
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#- calculate assimilation weighted leaf temperature

#- create weekly bins
combodat$bin4days <- as.factor(week(combodat$DateTime_hr))
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
# SMATR statistical analysis of Tleaf vs. Tair
#sma1 <- sma(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment,
#            data=output_meanT) # treats each observation as independent, which inflates the statistical power

#-------------
#- random effects ANCOVA for Tleaf vs. Tair. Some evidence that the warmed treatment had a lower slope
#    but both slope 95% CI's included 1.0.
lme1 <- lmer(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
anova(lme1) # some evidence of difference in slope, but not terribly strong (p = 0.02)
confint(lme1)

modelout <- data.frame(summary(lme1)$coefficients)
ambCI <- c(modelout$Estimate[[2]]-modelout$Std..Error[[2]]*1.96,modelout$Estimate[[2]]+modelout$Std..Error[[2]]*1.96)
eleCI <- c((modelout$Estimate[[2]]+modelout$Estimate[[4]])-(modelout$Std..Error[[4]]*1.96),
           (modelout$Estimate[[2]]+modelout$Estimate[[4]])+(modelout$Std..Error[[4]]*1.96))


lme1.test <- lmer(meanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme1.test2 <- lmer(meanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme1,lme1.test,lme1.test2) # simplest model is preferred
AIC(lme1,lme1.test,lme1.test2) # models have very similar AICs
confint(lme1.test2)

visreg(lme1,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme1.test,xvar="T_treatment")


#-------------
#- random effects ANCOVA for assimilation-weighted Tleaf vs. Tair
lme2 <- lmer(weightedMeanLeafT~T_treatment+meanAirT+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
lme2.test <- lmer(weightedMeanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme2.test2 <- lmer(weightedMeanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme2,lme2.test,lme2.test2)
AIC(lme2,lme2.test,lme2.test2) # simpler model is preferred from AIC and logLik bases

anova(lme2.test2) # some evidence of difference in slope, but not terribly strong (p = 0.05)


modelout2 <- data.frame(summary(lme2.test2)$coefficients)
confint(lme2.test2)
#ambCI <- c(modelout2$Estimate[[3]]-modelout2$Std..Error[[3]]*1.96,modelout2$Estimate[[3]]+modelout2$Std..Error[[3]]*1.96)
#eleCI <- c((modelout2$Estimate[[3]]+modelout2$Estimate[[4]])-(modelout2$Std..Error[[4]]*1.96),
#           (modelout2$Estimate[[3]]+modelout2$Estimate[[4]])+(modelout2$Std..Error[[4]]*1.96))

visreg(lme2.test2,xvar="meanAirT",overlay=T)
visreg(lme2,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme2,xvar="T_treatment")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- set up plot of Tleaf vs. Tair and assimilation-weighted Tleaf vs. Tair
windows(100,75)
par(mar=c(7,7,1,2),mfrow=c(1,2))
palette(c("blue","red"))
pchs=3
cexs=0.5

#------
#- plot Tleaf vs. Tair
mindate <- min(output_meanT$Date,na.rm=T)#as.Date("2016-05-01")
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),
       pch=pchs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,cex=cexs,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),
       pch=pchs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,cex=cexs,
       xlab="",ylab="",add=T)

# lmT <- lm(meanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lmT,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lmT)[[2]],2),sep=""),bty="n")
legend("bottomright",pch=c(pchs,pchs),col=c("blue","red"),legend=c("Ambient","Warmed"))
legend("topleft",legend=letters[1],cex=1.2,bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(T[leaf]~(degree*C)~(measured)),xpd=NA,cex.lab=2)

#------
#- plot assimilation-weighted Tleaf vs. Tair
plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=pchs,cex=cexs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)
plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=pchs,cex=cexs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,
       xlab="",ylab="",add=T)

# lm1 <- lm(weightedMeanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lm1,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lm1)[[2]],2),sep=""),bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(Assimilation~weighted~T[leaf]~(degree*C)),xpd=NA,cex.lab=2)
legend("topleft",legend=letters[2],cex=1.2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------













#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-- Repeat the calculation of assimilation-weighted leaf temperature, but on a monthly timescale
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#- calculate assimilation weighted leaf temperature

#- create monthly bins
combodat$bin4days <- as.factor(month(combodat$DateTime_hr))
combodat <- subset(combodat,bin4days != "2") #extract a month with little data
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
# SMATR statistical analysis of Tleaf vs. Tair
#sma1 <- sma(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment,
#            data=output_meanT) # treats each observation as independent, which inflates the statistical power

#-------------
#- random effects ANCOVA for Tleaf vs. Tair. Some evidence that the warmed treatment had a lower slope
#    but both slope 95% CI's included 1.0.
lme1 <- lmer(meanLeafT~meanAirT+T_treatment+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
anova(lme1) # some evidence of difference in slope, but not terribly strong (p = 0.02)
confint(lme1)

modelout <- data.frame(summary(lme1)$coefficients)
ambCI <- c(modelout$Estimate[[2]]-modelout$Std..Error[[2]]*1.96,modelout$Estimate[[2]]+modelout$Std..Error[[2]]*1.96)
eleCI <- c((modelout$Estimate[[2]]+modelout$Estimate[[4]])-(modelout$Std..Error[[4]]*1.96),
           (modelout$Estimate[[2]]+modelout$Estimate[[4]])+(modelout$Std..Error[[4]]*1.96))


lme1.test <- lmer(meanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme1.test2 <- lmer(meanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme1,lme1.test,lme1.test2) # simplest model is preferred
AIC(lme1,lme1.test,lme1.test2) # models have very similar AICs
confint(lme1.test2)

visreg(lme1,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme1.test,xvar="T_treatment")


#-------------
#- random effects ANCOVA for assimilation-weighted Tleaf vs. Tair
lme2 <- lmer(weightedMeanLeafT~T_treatment+meanAirT+meanAirT:T_treatment+(meanAirT|chamber),
             data=output_meanT)
lme2.test <- lmer(weightedMeanLeafT~meanAirT+T_treatment+(meanAirT|chamber),
                  data=output_meanT)
lme2.test2 <- lmer(weightedMeanLeafT~meanAirT+(meanAirT|chamber),
                   data=output_meanT)
anova(lme2,lme2.test,lme2.test2)
AIC(lme2,lme2.test,lme2.test2) # simpler model is preferred from AIC and logLik bases

anova(lme2.test2) # some evidence of difference in slope, but not terribly strong (p = 0.05)


modelout2 <- data.frame(summary(lme2.test2)$coefficients)
confint(lme2.test2)
#ambCI <- c(modelout2$Estimate[[3]]-modelout2$Std..Error[[3]]*1.96,modelout2$Estimate[[3]]+modelout2$Std..Error[[3]]*1.96)
#eleCI <- c((modelout2$Estimate[[3]]+modelout2$Estimate[[4]])-(modelout2$Std..Error[[4]]*1.96),
#           (modelout2$Estimate[[3]]+modelout2$Estimate[[4]])+(modelout2$Std..Error[[4]]*1.96))

visreg(lme2.test2,xvar="meanAirT",overlay=T)
visreg(lme2,xvar="meanAirT",by="T_treatment",overlay=T)
visreg(lme2,xvar="T_treatment")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- set up plot of Tleaf vs. Tair and assimilation-weighted Tleaf vs. Tair
windows(100,75)
par(mar=c(7,7,1,2),mfrow=c(1,2))
palette(c("blue","red"))
pchs=16
cexs=1

#------
#- plot Tleaf vs. Tair
mindate <- min(output_meanT$Date,na.rm=T)#as.Date("2016-05-01")
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),
       pch=pchs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,cex=cexs,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme1,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)
plotBy(meanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),
       pch=pchs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,cex=cexs,
       xlab="",ylab="",add=T)
# lmT <- lm(meanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lmT,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lmT)[[2]],2),sep=""),bty="n")
legend("bottomright",pch=c(pchs,pchs),col=c("blue","red"),legend=c("Ambient","Warmed"))
legend("topleft",legend=letters[1],cex=1.2,bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(T[leaf]~(degree*C)~(measured)),xpd=NA,cex.lab=2)

#------
#- plot assimilation-weighted Tleaf vs. Tair
plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=pchs,cex=cexs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,
       xlab="",ylab="")
abline(0,1,lty=2)

#- overlay mixed model predictions
xvar <- seq(from=min(output_meanT$meanAirT,na.rm=T),to=max(output_meanT$meanAirT,na.rm=T),length.out=101)
newdata <- expand.grid(T_treatment=c("ambient"),meanAirT=xvar,chamber="C01")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="blue",lwd=2)
lines(preds$upr~xvar,col="blue",lty=2,lwd=2)
lines(preds$lwr~xvar,col="blue",lty=2,lwd=2)
newdata <- expand.grid(T_treatment=c("elevated"),meanAirT=xvar,chamber="C02")
preds <- predictInterval(lme2,newdata=newdata)
lines(preds$fit~xvar,col="red",lwd=2)
lines(preds$upr~xvar,col="red",lty=2,lwd=2)
lines(preds$lwr~xvar,col="red",lty=2,lwd=2)

plotBy(weightedMeanLeafT~meanAirT|T_treatment,data=subset(output_meanT,Date>mindate),pch=pchs,cex=cexs,xlim=c(0,35),ylim=c(0,35),legend=F,axes=F,
       xlab="",ylab="",add=T)
# lm1 <- lm(weightedMeanLeafT~meanAirT,data=subset(output_meanT,Date>mindate))
# abline(lm1,lty=2)
#legend("bottomright",paste("Slope = ",round(coef(lm1)[[2]],2),sep=""),bty="n")
magaxis(side=c(1:4),labels=c(1,1,0,1),las=1,ratio=0.25)
title(xlab=expression(T[air]~(degree*C)),xpd=NA,cex.lab=2)
title(ylab=expression(Assimilation~weighted~T[leaf]~(degree*C)),xpd=NA,cex.lab=2)
legend("topleft",legend=letters[2],cex=1.2,bty="n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
