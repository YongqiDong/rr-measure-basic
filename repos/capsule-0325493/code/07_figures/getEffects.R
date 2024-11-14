corPath <- paste0("~/mystuff/projects/daytoday/dataRestart/joinDataSources/cleanData/removeColsRowSplit/getCorrelation/paper1wModelswDaywTimewSteps",".RData")
rawDataPath <- "~/mystuff/projects/daytoday/dataRestart/joinDataSources/cleanData/removeColsRowSplit/2.RData"
load(corPath)
cors <- repres$res$result
models <-  repres$res$resultModels
load(rawDataPath)
myData <- repres$res$train

#scanTime
earliestScan <- min(myData$scanTime,na.rm = TRUE)
latestScan <- max(myData$scanTime,na.rm = TRUE)
diffTime <- latestScan - earliestScan


#Days
latestDay <- max(myData$daysFromFirstScan)


#number steps
minSteps <- min(myData$number_steps,na.rm=TRUE)
maxSteps <- max(myData$number_steps,na.rm=TRUE)
diffSteps <- maxSteps-minSteps
baseLines <- c(earliestScan,0,minSteps)
diffs <- c(diffTime,365,diffSteps)
vars <- c('scanTime','daysFromFirstScan')
mult <- matrix(data=0,nrow=6,ncol=7)
mult[,1] <- 1 #everybody has intercept

mult[2:6,2:6] <- diag(5)
for (i in 1:2){
  cVar <-vars[i]
  print(cVar)
  mult[,7] <- baseLines[i] #everybody starts here  
  for (out in c("FS_TotalGrayVol","FS_CortexVol")){
    tmp <- summary(models[[out,cVar]]$full)
    coefs <- as.matrix(tmp$coefficients[,1])
    baseline <- mult %*% coefs  #brain volume in the morning
    change <- diffs[i]*coefs[cVar,1]#brain volume change
    print(out)
    print(change/baseline)
  }
}


#make plots checking for the effects
for (subj in levels(myData$subject)){
  tData <- myData[myData$subject==subj,]  
  plot(tData$scanTime,tData$FS_TotalGrayVol)
}


