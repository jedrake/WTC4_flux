#- read in the oxygen isotope data from Richard Harwood

o18dat1 <- read.csv("Data/WTC4_wood_cellulose_18O.csv")
o18dat <- subset(o18dat1,Targeted.Temp != "None")
o18dat$Targeted.Temp <- as.numeric(as.character(o18dat$Targeted.Temp))
plot(d18O~Targeted.Temp,data=o18dat)

#- average and sem's across treatments and targeted temperatures
o18dat.m <- summaryBy(d18O~T_Treatment+Targeted.Temp,FUN=c(mean,se),data=o18dat)
windows()
plotBy(d18O.mean~Targeted.Temp|T_Treatment,data=o18dat.m,type="o",pch=16,cex=1.2,ylim=c(30,35),
       col=c("blue","red"),
       xlab="Air temperature (deg C)",ylab="Wood cellulose d18O (per mil)",
       panel.first=adderrorbars(x=o18dat.m$Targeted.Temp,y=o18dat.m$d18O.mean,
                                SE=o18dat.m$d18O.se,direction="updown"))