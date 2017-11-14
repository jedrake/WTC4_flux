
source("R/loadLibraries.R")

library(readxl)
library(visreg)
library(RColorBrewer)
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#- This script is meant to evaluate the efficacy of a branch allometry to predict leaf area in WTC4
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------


#- Read in the leaf size data (from April 7th 2016)
leafsize <- read.csv("data/branches/leafSize_2016-4-7.csv")
leafsize.m <- summaryBy(leaf_size~chamber,data=leafsize,FUN=mean,keep.names=T)

#- Read in the leaf counts (from April 7th 2016)
leafno <- read.csv("data/branches/leafCount_2016-4-7.csv")
leafno <- merge(leafno,leafsize.m,by="chamber")
leafno$leaf_area <- with(leafno,leaf_no*leaf_size) #- calculate the leaf area on each branch

#- read in the branch dimensions
branches <- read_excel("data/branches/WTC_4_Branch_Data_set1_20160419edit.xlsx",skip=7)
names(branches) <- c("tree","branch","insertion_point_floor","insertion_point_ground","diam1","diam2",
                     "diam_mean","tip_height","tip_height_ground","branch_length","direction","comments")
branches <- branches[,c("tree","branch","diam_mean","tip_height_ground","insertion_point_ground","branch_length")]
branches$chamber <- as.factor(paste0("C", sprintf("%02.0f", branches$tree))) 

#- calculate relative insertion point for each tree
branches.l <- split(branches,branches$chamber)
for (i in 1:length(branches.l)){
  maxIP <- max(branches.l[[i]]$insertion_point_ground)
  branches.l[[i]]$insertion_point_relative <- branches.l[[i]]$insertion_point_ground/maxIP
    
}
branches <- do.call(rbind,branches.l)

#- merge datasets
br <- merge(leafno,branches,by=c("chamber","branch"))
br$d2h <- with(br,diam_mean*branch_length)
br$log_LA <- log10(br$leaf_area)

# pairs(br[,c("leaf_no","leaf_area","d2h","diam_mean","tip_height_ground","branch_length","insertion_point_ground")])

plot(leaf_area~d2h,data=br)

#- subset to even branches, fit relationship
# br.tofit <- br[which(br$branch %%2 ==1),]
# br.toeval <- br[which(br$branch %%2 ==0),]

#- fit allom
lm1 <- lm(leaf_no~chamber*branch_length*insertion_point_relative*I(insertion_point_relative)^2,data=br)
visreg(lm1)
plot(log_LA~d2h,data=br)
plot(log_LA~insertion_point_relative,data=br)


#- fit poisson regression to better model the count data
m1 <- glm(leaf_no ~ chamber+branch_length+chamber*insertion_point_relative*I(insertion_point_relative^2),
                  family="poisson", data=br)

br$pred <-fitted(m1)
br$predLA <- br$pred*br$leaf_size



#--- predict the leaf area of branches not used to drive the allometry
#br.toeval$log_LA_pred <- predict(lm1,newdata=br.toeval)
#br.toeval$predLA <- 10^br.toeval$log_LA_pred
windows()
plot(leaf_area~predLA,data=br,ylab="Observed leaf area (cm2)",xlab="predicted leaf area (cm2)",
     ylim=c(0,12000))
abline(0,1)


windows(100,50)
par(mfrow=c(1,3),cex.lab=2,mar=c(5,6,1,1))
plot(leaf_no~diam_mean,data=br,xlab="Diam",ylab="Leaf Area (cm2)")
plot(leaf_no~branch_length,data=br,xlab="Length",ylab="Leaf Area (cm2)")
plot(leaf_no~insertion_point_relative,data=br,xlab="Insertion point",ylab="Leaf Area (cm2)")






#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#- Okay, now we take that predicted allometery (lm1) and test it against new data
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

#- read in the new branch data, 

#- Read in the leaf size data (from April 7th 2016)
# leafsize <- read.csv("data/branches/leafSize_2016-4-7.csv")
# leafsize.m <- summaryBy(leaf_size~chamber,data=leafsize,FUN=mean,keep.names=T)
# leafsize.m$leaf_size <- leafsize.m$leaf_size #*0.85


#- read in the leaf size data, from the isotope campaigns
size <- read_excel("data/branches/WTC4_d13C_leafsize.xlsx",skip=0)
size$chamber <- as.factor(paste0("C", sprintf("%02.0f", size$tree))) 
size$leaf_size <- with(size,area/nleaves)
size.m <- summaryBy(leaf_size~chamber,data=size,FUN=mean,keep.names=T)
size.m2 <- merge(size.m,leafsize.m,by="chamber",all=T)
mean(with(size.m2,leaf_size.x/leaf_size.y),na.rm=T) # leaves are ~35 % bigger now

#- Read in the leaf counts (from Aug25th 2016)
leafno2 <- read.csv("data/branches/WTC4_leafcount_set2_select_brs_20160825.csv")
leafno2$chamber <- as.factor(paste0("C", sprintf("%02.0f", leafno2$Chamber))) 
leafno2 <- merge(leafno2,leafsize.m,by="chamber")
leafno2[which(leafno2$chamber=="C01" & leafno2$branch_no==35),"leaf_no"] <- NA #- NA fill crazy outlier
#leafno2$leaf_area <- with(leafno2,leaf_no*leaf_size) #- calculate the leaf area on each branch
        


#- read in the branch dimensions
branches2 <- read.csv("data/branches/WTC_4_Branch_Data_set2_20160824_entered.csv")
names(branches2) <- c("tree","stem","branch","insertion_point_floor","insertion_point_ground","diam1","diam2",
                     "diam_mean","tip_height","tip_height_ground","branch_length","direction","comments")
branches2 <- branches2[,c("tree","stem","branch","diam_mean","tip_height_ground","insertion_point_ground","branch_length")]

#- work out the tree number stuff
branches2$chamber <- as.factor(paste0("C", sprintf("%02.0f", branches2$tree))) 

#- calculate relative insertion point for each tree
branches2.l <- split(branches2,branches2$chamber)
for (i in 1:length(branches2.l)){
  maxIP <- max(branches2.l[[i]]$insertion_point_ground)
  branches2.l[[i]]$insertion_point_relative <- branches2.l[[i]]$insertion_point_ground/maxIP
  
}
branches2 <- do.call(rbind,branches2.l)

#- merge datasets
br2 <- merge(leafno2,branches2,all.y=T,by.x=c("chamber","branch_no"),by.y=c("chamber","branch"))
br2$d2h <- with(br2,diam_mean*branch_length)
#br2$log_LA <- log10(br2$leaf_area)



#--- predict the branch-level leaf area
br2$pred_leaf_no <- predict(m1,type="response",newdata=data.frame(chamber=br2$chamber,branch_length=br2$branch_length,
                                                  insertion_point_relative=br2$insertion_point_relative))
br2$leaf_size <- NULL
br2 <- merge(br2,leafsize.m,by="chamber",all.x=T)

br2$pred_leaf_area <- br2$pred_leaf_no*br2$leaf_size*1.35
#br2$diff <- with(br2,pred_leaf_area-leaf_area)


windows(100,50)
palette(c("black",brewer.pal(11,"Spectral")))

par(mfrow=c(1,2),cex.lab=2,mar=c(5,6,1,1))

plotBy(leaf_no~pred_leaf_no|chamber,data=br2,pch=16,xlim=c(0,500),ylim=c(0,500),
       xlab="Predicted leaf number (#)",ylab="'Measured' leaf number (#)")
abline(0,1)

#- sum across the 10 branches for each chamber
br2.sum <- summaryBy(leaf_no+pred_leaf_no~chamber,
                     data=subset(br2,is.na(leaf_no)==F),FUN=sum,keep.names=T)
plotBy(leaf_no~pred_leaf_no|chamber,data=br2.sum,pch=16,
       xlab="Sum of predicted leaf number (#)",ylab="Sum of 'Measured' leaf number (#)")
abline(0,1)



# #--- root mean square error for the two pred vs. obs, for Mark.
# # Function that returns Root Mean Squared Error
# rmse <- function(error){
#   sqrt(mean(error^2))
# }
# rmse(subset(br2,is.na(leaf_area)==F)$diff) # RMSE for the 120 branches
# rmse(br2.sum$leaf_area-br2.sum$pred_leaf_area) # RMSE for the 12 branch sums




#- look at data
#subset(br2,is.na(leaf_area)==F)[,c("chamber","branch_no","diam_mean","insertion_point_relative","leaf_area","pred_leaf_area","diff")]


#- chamber sums
ch.sum <- summaryBy(pred_leaf_area~chamber,
                     data=subset(br2,is.na(diam_mean)==F),FUN=sum,keep.names=T)
ch.sum$predLA <- ch.sum$pred_leaf_area/10000

#- get teh most recent diameter measurements
d65_3 <- read_excel("W://WORKING_DATA/WTC/WTC4/Share/RAW_GROWTH_DATA/WTC4 tree measure data/65cm Dia data/WTC4_65cm_dia_20160810_measure_7.xlsx",
                    skip=2)
d65_3$Date <- as.Date("2016-08-10")


windows()
plot(ch.sum$predLA~d65_3[,3],col=ch.sum$chamber,xlab="diam (mm)",ylab="total predicted leaf area (m2)",pch=16,cex=1.5)
legend("topleft",legend=ch.sum$chamber,fill=palette()[1:12])       




#------------------------------------------------------------------------------------
#- For an additional comparison, read in the complete leaf counts that Adam did on 
#  Friday 26 Aug. He did a complete count on C08 and an incomplete count on C09.
#------------------------------------------------------------------------------------
adam <- read_excel("data/branches/Copy of WTC4_Leafcount_set2_20160826_total_templete.xlsx",skip=4)
adam$chamber <- as.factor(paste0("C", sprintf("%02.0f", adam$tree))) 
names(adam) <- c("tree","branch_no","leaf_count","comments","null","chamber")
adam <- subset(adam,chamber %in% c("C08","C09"))[,c("chamber","branch_no","leaf_count")]

br3 <- merge(br2,adam,by=c("chamber","branch_no"),all=T)
br3$leaf_size <- NULL
br3$chamber <- factor(br3$chamber)

#- read in the leaf size data
size <- read_excel("data/branches/WTC4_d13C_leafsize.xlsx",skip=0)
size$chamber <- as.factor(paste0("C", sprintf("%02.0f", size$tree))) 
size$leaf_size <- with(size,area/nleaves)
size.m <- summaryBy(leaf_size~chamber,data=size,FUN=mean,keep.names=T)

br4 <- merge(br3,leafsize.m,by="chamber",all.x=T)

br4$leaf_area_adam <- with(br4,leaf_count*leaf_size*1.35)
br4$log_leaf_area_adam <- log10(br4$leaf_area_adam)

br4.m <- summaryBy(leaf_area_adam+pred_leaf_area~chamber,data=br4,na.rm=T,FUN=sum)


palette(c("black",brewer.pal(11,"Spectral")))
windows()
plot(ch.sum$predLA~d65_3[,3],col=ch.sum$chamber,xlab="diam (mm)",ylab="total predicted leaf area (m2)",pch=16,cex=1.5)
legend("topleft",legend=ch.sum$chamber,fill=palette()[1:12])       
points(br4.m$leaf_area_adam.sum/10000~d65_3[8:9,3],pch=15,col=palette()[8:9],cex=1.5)




#----------- plot predicted vs. observed for all branches for Craig
palette(c("black",brewer.pal(11,"Spectral")))
windows()
plotBy(leaf_count~pred_leaf_no|chamber,data=br3,pch=16,xlim=c(0,400),ylim=c(0,400),cex.lab=1.7,
       xlab="Predicted leaf count (#)",ylab="Observed leaf count (#)")
plotBy(leaf_no~pred_leaf_no|chamber,data=br3,pch=16,add=T,legend=F,xlab="",ylab="")
abline(0,1)

#- note that there is real counting error
plot(leaf_count~leaf_no,data=br3,pch=16,xlim=c(0,300),ylim=c(0,300))
abline(0,1)




#----------- 
#- simulation for Craig.
craig <- subset(br3,is.na(leaf_no)==F | is.na(leaf_count)==F)
craig$leaves <- NA
craig$leaves[which(is.na(craig$leaf_no)==F)] <- craig$leaf_no[which(is.na(craig$leaf_no)==F)]
craig$leaves[which(is.na(craig$leaf_count)==F)] <- craig$leaf_count[which(is.na(craig$leaf_count)==F)]

craig <- subset(craig,pred_leaf_no>0)[,c("chamber","branch_no","pred_leaf_no","leaves")]

#- take a selection of 80 branches from "Craig".
perdiff <- c()
for(i in 1:10000){
  branches <- sample(1:241,size=80,replace=T)
  leafno <- sum(craig$leaves[branches])
  leafno_pred <- sum(craig$pred_leaf_no[branches])
  perdiff[i] <- (leafno_pred-leafno)/leafno
}
hist(perdiff)

#----------- 
