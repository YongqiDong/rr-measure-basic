library(car)
source(file.path(projDir,'code/global.R'),local=TRUE)
#load data set
load("/data/joinDataSources/cleanData/3.RData")
theData <- repres$res
#run lm model
theData$FS_CortexVol <- theData$FS_CortexVol / 1000
theData$FS_TotalGrayVol <- theData$FS_TotalGrayVol / 1000
theModelCortex <- lm(FS_CortexVol   ~daysFromFirstScan + scanTime+subject,data=theData)
theModelGray <- lm(FS_TotalGrayVol   ~daysFromFirstScan + scanTime+subject,data=theData)


opar <- par()      # make a copy of current settings
par(pch=16)
pdf(file.path(projDir,'/results/9_ParResid.pdf'),width=paperWidth, height=paperHeight,onefile=TRUE)
par(mfrow=c(2,2))
crPlot(theModelCortex,'daysFromFirstScan',ylab='Partial Residual FS-Cortex',xlab=c('Days Since First Scan'),main='',pch=16,col='#BEBEBE',col.lines = c('black'),grid=FALSE,bty='l')
crPlot(theModelCortex,'scanTime',ylab='Partial Residual FS-Cortex',xlab=c('Time of Day'),main='',pch=16,col='#BEBEBE',col.lines = c('black'),grid=FALSE,bty='l')
crPlot(theModelGray,'daysFromFirstScan',ylab='Partial Residual FS-TotalGray',xlab=c('Days Since First Scan'),main='',pch=16,col='#BEBEBE',col.lines = c('black'),grid=FALSE,bty='l')
crPlot(theModelGray,'scanTime',ylab='Partial Residual FS-TotalGray',xlab=c('Time of Day'),main='',pch=16,col='#BEBEBE',col.lines = c('black'),grid=FALSE,bty='l')
par(opar) 
dev.off()
