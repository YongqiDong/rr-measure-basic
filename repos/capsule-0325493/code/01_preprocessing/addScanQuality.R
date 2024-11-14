addScanQuality <- function(filePath){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  load("~/Downloads/day2day/data/joinDataSources/cleanData/prepImpute/1.RData")
  prepImpute <- repres$res
  load(filePath)
  myData <- repres$res
  
  scanQuality <- read.csv(paste0(projDir,'/data/brain/scanQuality.txt'),sep='\t')
  scanQuality$FS_CortexVolFake <- scanQuality$CortexVol
  scanQuality <- scanQuality[,c("FS_CortexVolFake","SurfaceHoles")]
  
  ##remove duplicate in scanQuality
  duplicateIndex <- duplicated(scanQuality$FS_CortexVol)
  duplicates <- which(scanQuality$FS_CortexVol[duplicateIndex] == scanQuality$FS_CortexVol)
  scanQuality <- scanQuality[-duplicates[1],]
  
  ##remove duplicate in allData
  allData <- complete(myData, action='long', include=TRUE)
  allDataImp1 <- allData[allData$.imp==0,] 
  duplicateIndex <- duplicated(allDataImp1$FS_CortexVol)
  duplicatesA <- which(allDataImp1$FS_CortexVol[duplicateIndex] == allDataImp1$FS_CortexVol)
  duplicateID <- allDataImp1$.id[duplicatesA[1]]
  allData <- allData[allData$.id!=duplicateID,]
  
  
  #merge with scanQuality
  allData$FS_CortexVolFake <- allData$FS_Cortex*prepImpute$sds['FS_CortexVol']+prepImpute$means['FS_CortexVol']
  allDataNew <- merge(allData,scanQuality,by=c('FS_CortexVolFake'),all.x=TRUE,all.y=FALSE,sort=FALSE)
  allDataNew$FS_CortexVolFake <- NULL
  allDataNew <- allDataNew[with(allDataNew, order(.imp,.id)), ]
  
  
  #predictive mean matching multiple imputation for only missing value in Surfaceholes
  allDataImp1 <- allDataNew[allDataNew$.imp==0 & allDataNew$subject==8,] 
  mSurfHoles <- mean(allDataImp1$SurfaceHoles,na.rm=TRUE)
  sdSurfHoles <- sd(allDataImp1$SurfaceHoles,na.rm=TRUE)
  
  theNas <- which(is.na(allDataNew$SurfaceHoles))
  cands <- allDataImp1$SurfaceHoles[!is.na(allDataImp1$SurfaceHoles)]
  for (i in theNas){
    prediction <- rnorm(1,mean=mSurfHoles,sd=sdSurfHoles)
    distances <- (prediction-cands)^2
    sortRes <- sort(distances,index.return=TRUE)
    candClosest <- cands[sortRes$ix[1:5]]
    stopifnot(is.na(allDataNew$SurfaceHoles[i]))
    allDataNew$SurfaceHoles[i] <- sample(candClosest,1)  
  }
  allDataNew$SurfaceHoles <- allDataNew$SurfaceHoles/sdSurfHoles-mSurfHoles
  myData <- as.mids(allDataNew)
  attr(myData,'statsSurfaceHoles') <- list(mean=mSurfHoles,sd=sdSurfHoles)
  
  return(help_repro(match.call(),myData))#return and write dataset to disk
}
setwd("~/Downloads/day2day/data/joinDataSources/cleanData/prepImpute/impute/repairFactors")
tmp <- addScanQuality('1.RData')