#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#- create a conceptual figure to separate the ideas of thermoregulation
#    and differential "sampling" of leaf temperatures by photosynthesis
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
library(calibrate)

Tair <- seq(10,40,length.out=101)
Tleaf1 <- Tair
Tleaf2 <- Tair*0.7+6.5
Tleaf3 <- rep(22,101)

#- plot leaf temperature model
windows(85,55);par(mar=c(7,7,1,2),mfrow=c(1,2),cex.lab=2,cex.axis=1.1)
plot(Tleaf1~Tair,type="l",
     xlab=expression(Air~temperature~(T[air]~","~degree*C)),
     ylab=expression(Leaf~temperature~(T[leaf]~","~degree*C)))
lines(Tleaf2~Tair,lty=2)
lines(Tleaf3~Tair,lty=3)

textxy(X=32,Y=32.7,labs="1:1",cex=1.5,srt=45)
textxy(X=25.5,Y=24.8,labs="Partial thermal regulation",cex=1.5,srt=37)
textxy(X=24,Y=22,labs="Complete thermal regulation",cex=1.5,srt=0)
legend("topleft",legend=letters[1],cex=1.2,bty="n")

#- plot "sampling" effect model
plot(Tleaf1~Tair,type="l",
     xlab=expression(Air~temperature~(T[air]~","~degree*C)),
     ylab=expression(Assimiation~weighted~T[leaf]~(degree*C)))
lines(Tleaf2~Tair,lty=2)
lines(Tleaf3~Tair,lty=3)
arrows(x0=c(12,35),y0=c(15.5,30.5),x1=c(12,35),y1=c(21.5,22.5))

textxy(X=32,Y=32.7,labs="1:1",cex=1.5,srt=45)
textxy(X=30,Y=27.8,labs="Leaf temperature",cex=1.5,srt=37)
textxy(X=17,Y=21,labs="Constant assimitation-weighted leaf temperature",cex=1.3,srt=0)
legend("topleft",legend=letters[2],cex=1.2,bty="n")