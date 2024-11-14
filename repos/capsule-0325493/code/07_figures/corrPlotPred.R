
#packages
library(corrplot)
file.path(projDir,'code/global.R')
source(file.path(projDir,'code/global.R'))
source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'))
paperHeight = paperWidth*3/4 #to get 4:3 ratio
corPathList <-  list(file.path(projDir,'data/joinDataSources/cleanData/getCorrelation/paper1UpdatePreds.RData'))
savePathList <- list(file.path(figHome,'/3_CorrPreds'))

#sanity check plot
usedFiles <- checkPlot(usedFiles,getVersions(corPathList[[1]]))
j <- 1
for (corPath in corPathList){
  #meat
  load(corPath)
  #current data format
  if ("result" %in% names(repres$res)){
    cors <- repres$res$result
    #legacy
  }else{
    cors <- repres$res
  }
  rownames(cors) <- sapply(rownames(cors), variableTranslate)
  colnames(cors) <- sapply(colnames(cors), variableTranslate)
  pdf(paste0(savePathList[[j]],".pdf"),width=paperWidth, height=paperHeight*1.6,onefile=FALSE)
  noiseFull <- cors[,,"error"]-cors[,,"errorRed"]
  varExplained <- 1-noiseFull/cors[,,"error"]
  diag(varExplained) <- 0
  varExplained[varExplained<0] <- 0
  corrplot(varExplained,method='number',is.corr = FALSE,type='lower',tl.col="black",addDot = TRUE,addCoefasPercent = TRUE,diag=FALSE,order='FPC',col=colorRampPalette(c("red",'#bfbfbf',"black"))(200))
  dev.off()
  j <- j + 10
}
