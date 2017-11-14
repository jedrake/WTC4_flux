
library(plotBy)
library(calibrate)

hwdat <- read.csv("data/Proposed_heatwave_pairing_WTC4_cointoss.csv")

windows();par(cex.lab=1.5)
plotBy(FluxCO2..kg.C.~d2h..cm3.|treatment,data=subset(hwdat,Heatwave=="control"),pch=16,legend=F,
       xlim=c(8000,40000),ylim=c(0,9),cex=1.5,
       xlab="Tree Size (d2h; cm3)",ylab="Sum of CO2 fluxes (kg C)")
plotBy(FluxCO2..kg.C.~d2h..cm3.|treatment,data=subset(hwdat,Heatwave=="heatwave"),pch=1,legend=F,add=T,
       xlim=c(50,400),ylim=c(0,9),cex=1.5,lwd=3)
legend("topleft",legend=c("A-C","A-HW","W-C","W-HW"),col=c("black","black","red","red"),pch=c(16,1,16,1),cex=2)
textxy(X=hwdat$d2h..cm3.,Y=hwdat$FluxCO2..kg.C.,labs=hwdat$chamber,cex=0.8)
