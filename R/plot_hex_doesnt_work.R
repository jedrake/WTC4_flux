#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#- assumes combine_data_plot_assimilationWeightedLeafT.R has been run
#    such that "combodat" is an available dataframe
library(hexbin)
source("R/hexagon_plotting_functions.R")
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------


#- calculate Tdiff
combodat$Tdiff <- with(combodat,TargTempC_Avg - Tair_al)


#creates a scale of colors
myColorRamp_raw <- function(colors, values) {
  #v <- (values - min(values))/diff(range(values))
  v <- (values + 7)/25
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}




#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#- plot heat-map style hexagons for Tdiff relative to PAR and Transpiration



#- minimum number of observations per bin to be displayed
countmin <- 50

#- number of bins in each direction
nbins <- 40 # 10



#-------------- 
#average values of points inside hexbins
combodat_light <- subset(combodat,PAR>20 & is.na(Tdiff)==F)

h_a <- hexbin(combodat_light$PAR, combodat_light$VPD,xbins=nbins,IDs=TRUE)
              #xbnds=c(7.9,31),ybnds=c(1.2,38)) # note the dimensions only work for some xbins values (21)

cells_a <- hcell2xy(h_a)
hexd_a <- (h_a@xbnds[2] - h_a@xbnds[1])/h_a@xbins
nhex_a <- h_a@ncells
d_a <- getdiams(cells_a)
#d[2] <- 0.005

rampcolors <- c("blue","forestgreen","yellow","orange")


meanHexBinamb<-data.frame(mean=hexTapply(h_a, combodat_light$Tdiff, mean)) 

#- find the bins with fewer than the critical number of observations
lengthHexBinamb<-data.frame(mean=hexTapply(h_a, combodat_light$Tdiff, length)) 
hexCols_a[which(lengthHexBinamb<countmin)] <- NA

hexCols_a <- myColorRamp_raw(rampcolors, meanHexBinamb$mean)




# Plot amb
windows(40,35);par(mfrow=c(1,1),mar=c(6,6,0.0,0.2),oma=c(1,1,4.5,4))

plot(cells_a, type='n',xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,6))
for(i in 1:nhex_a){
  Hexagon(cells_a$x[i], cells_a$y[i], xdiam=d_a$xdiam*2, ydiam=d_a$ydiam,
          border=NA,#border="grey",
          col=hexCols_a[i])
}
magaxis(side=c(1,2,3,4),las=1,labels=c(1,0,0,0),cex.axis=1.5)
axis(side=2,at=c(-0.02,0,0.02,0.04,0.06),labels=c(-0.02,0,0.02,0.04,0.06),las=1,cex.axis=1.5,tick=F)
title(xlab=expression(PPFD~(mu*mol~m^-2~s^-1)), ylab=expression(H[2]*O~flux~(mmol~s^-1)),cex.lab=3,line=4,xpd=NA)

# add legend to top of the first two panels
values <- c(-2,0,2,4,6)
legcolors <- myColorRamp_raw(rampcolors, values)[1:6]
xloc <- seq(10,1000,length.out=length(values))
yloc <- rep(.067,length(values))
points(yloc~xloc,pch=18,cex=4,col=legcolors,xaxt="n",yaxt="n",xlab="",ylab="",xpd=NA)
text(xloc,yloc+3,labels=values[1:6],cex=1.5,xpd=NA)

# 
# #-------------- 
# 
# #creates a scale of colors
# myColorRamp <- function(colors, values) {
#   v <- (values - min(values))/diff(range(values))
#   #v <- (values - -3)/10 
#   x <- colorRamp(colors)(v)
#   rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
# }
# 
# toplot <- subset(t4,as.Date(DateTime_hr) > as.Date("2016-7-1"))
# 
# all <- toplot
# 
# #- create hex bins
# library(hexbin)
# hall <- hexbin(all$PPFD, all$VPD,xbins=20,IDs=TRUE)
# 
# 
# 
# #average values of points inside hexbins 
# meanHexBinall<-data.frame(mean=hexTapply(hall, all$Tdiff_met, mean)) 
# 
# # #- find the NUMBER of points per hexbin. Remove hexagons with fewer than 5 points?
# # NumHexBinall<-data.frame(number=hexTapply(hall, all$Tdiff_met, length)) 
# # torm <- as.numeric(which(NumHexBinall$number < 10))
# # meanHexBinall[torm,] <- NA
# # meanHexBinall <- meanHexBinall[complete.cases(meanHexBinall),]
# 
# colsall <- myColorRamp(c("blue","green","yellow", "red"), meanHexBinall$mean)


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

