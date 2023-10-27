# script to clean and combine Iowa fish data into a small numer of files before uploading it to the drop box

rm(list = ls())
library(tidyverse)
setwd("~/fishClimateChange/receivedCoopFishData/Iowa/pulledFromIDNRdb")

bcpCPE=read.csv("BlackCrappieCPUE.csv",stringsAsFactors = F)
bcpLen=read.csv("BlackCrappieFishMeasurementDetail.csv",stringsAsFactors = F)
blgCPE=read.csv("BluegillCPUE.csv",stringsAsFactors = F)
blgLen=read.csv("BluegillFishMeasurementDetail.csv",stringsAsFactors = F)
chcCPE=read.csv("ChannelCatfishCPUE.csv",stringsAsFactors = F)
chcLen=read.csv("ChannelCatfishFishMeasurementDetail.csv",stringsAsFactors = F)
lmbCPE=read.csv("LargemouthBassCPUE.csv",stringsAsFactors = F)
lmbLen=read.csv("LargemouthBassFishMeasurementDetail.csv",stringsAsFactors = F)
npkCPE=read.csv("NorthernPikeCPUE.csv",stringsAsFactors = F)
npkLen=read.csv("NorthernPikeFishMeasurementDetail.csv",stringsAsFactors = F)
sampLoc=read.csv("samplingLocations.csv",stringsAsFactors = F)
smbCPE=read.csv("SmallmouthBassCPUE.csv",stringsAsFactors = F)
smbLen=read.csv("SmallmouthBassFishMeasurementDetail.csv",stringsAsFactors = F)
wlyCPE=read.csv("WalleyeCPUE.csv",stringsAsFactors = F)
wlyLen=read.csv("WalleyeFishMeasurementDetail.csv",stringsAsFactors = F)
wcpCPE=read.csv("WhiteCrappieCPUE.csv",stringsAsFactors = F)
wcpLen=read.csv("WhiteCrappieFishMeasurementDetail.csv",stringsAsFactors = F)
ywpCPE=read.csv("YellowPerchCPUE.csv",stringsAsFactors = F)
ywpLen=read.csv("YellowPerchFishMeasurementDetail.csv",stringsAsFactors = F)


# making one big CPE file

cpes=rbind(bcpCPE,blgCPE,chcCPE,lmbCPE,npkCPE,smbCPE,wlyCPE,wcpCPE,ywpCPE)
cpes$cpe=cpes$Total.Fish/cpes$Duration.hrs..NumNets
allcpes=cpes[,c(1,2,7,8,9,10,11)]
allcpes=allcpes%>%
  left_join(sampLoc, by=c("LakeName"="First.SiteName"))%>%
  filter(LakeId!="TEST")
write.csv(allcpes,"cleanedIACPUEs.csv",row.names = F)


# making one big length file
# note some lengths are outliers for each species and should be filtered out
# I've already filered out the super obvious ones but semi-realistic but incorrect measurements probably still remain
# put this in the README associated with this data

lens=rbind(bcpLen,blgLen,chcLen,lmbLen,npkLen,smbLen,wlyLen,wcpLen,ywpLen)
alllens=lens[,c(1,2,15,16,17,19,23,7,11,25,8,9,10,26:28)]
alllens=alllens%>%
  left_join(sampLoc,by=c("X.report_FishMeasurements..SiteName."="First.SiteName"))%>%
  filter(LakeID!="TEST")
write.csv(alllens,"cleanedIALengths.csv",row.names = F)
