#packages
library(ggpubr)
library(ggplot2)
library(scales)
library(corrplot)
source(file.path(projDir,'/code/05a_linearModel/LinearModelf.R'))

paperHeight = paperWidth*3/4 #to get 4:3 ratio
corPathList <-  list(file.path(projDir,'data/joinDataSources/cleanData/getCorrelation/paper1Update.RData'),
                     file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/getCorrelation/paper1WDays.RData'),
                     file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/getCorrelation/paper1WTime.RData'),
                     file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/getCorrelation/paper1WSteps.RData'))



#intialize
preds <- getSettings(paper=1)$preds
corMat <- array(data=NA,dim=c(length(selOutcomes),length(corPathList),length(preds),3)) #outcomes x step x predictors x type
types <- c("error","errorRed","pVal")
dimnames(corMat) <- list(selOutcomes,c('Baseline','Days','Time of Scan','Steps'),preds,types)


#load corpath and extract important values
#generate plots
i <- 1
for (corPath in corPathList){
  #meat
  load(corPath)
  #current data format
  cors <- repres$res$result
  corMat[selOutcomes,i,dimnames(cors)[[2]],] <- cors[selOutcomes,,]
  i <- i+1
}
#for each outcome generate the appropriate figure
for(i in 1:dim(corMat)[1]){
  errorRed <-corMat[i,,,"errorRed"]
  pVals <-corMat[i,,,"pVal"]
  colnames(errorRed) <- sapply(colnames(errorRed), variableTranslate)
  #sort by absolut error Reduction in first step
  if (i==i){
    decOrder <- order(errorRed['Baseline',],decreasing = TRUE)
  }
  errorRed <- errorRed[,decOrder]
  pVals <- pVals[,decOrder]
  pdf(file.path(figHome,paste0('4_',rownames(corMat)[i],"_stepwise.pdf")),width=paperWidth, height=paperHeight,onefile=FALSE)
  fakeP <- pVals
  fakeP[pVals<0.05]<-1
  fakeP[pVals>0.05]<-0
  errorRed[is.na(errorRed)] <- 0
  corrplot(errorRed*100,p.mat = fakeP,is.corr = FALSE,method="number",addCoefasPercent = TRUE,pch=1,col =colorRampPalette(c('#bfbfbf','#bfbfbf',"black"))(200),tl.col="black",cl.pos = 'n',cl.ratio=0.4)
  title(outcomeTranslate(rownames(corMat)[i]))
  dev.off()
}
