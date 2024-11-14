#packages
library(corrplot)
source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'))
paperHeight = paperWidth*3/4 #to get 4:3 ratio
corPathList <-  list(file.path(projDir,'/data/joinDataSources/cleanData/getCorrelation/paper1Update.RData'))
savePathList <- list(file.path(figHome,'/2_Corr'))
downScale <- 0.5

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
  rownames(cors) <- sapply(rownames(cors), outcomeTranslate)
  rownames(cors) <- gsub("\n", "", rownames(cors))
  colnames(cors) <- sapply(colnames(cors), variableTranslate)
  pdf(paste0(savePathList[[j]],".pdf"),width=paperWidth, height=paperHeight,onefile=FALSE)
  fakeP <- cors[,,'pVal']
  fakeP[cors[,,'pVal']<0.01]<-1
  fakeP[cors[,,'pVal']>0.01]<-0
  corrplot(cors[,,'pVal'],is.corr = FALSE,method="number",col=colorRampPalette(c("red","black", '#bfbfbf'))(200),addCoefasPercent=TRUE,p.mat = fakeP,sig.level = 0.01,pch=1,pch.cex = 3.75,cl.pos='n',tl.col="black",addDot = TRUE)
  dev.off()
  j <- j + 1
}
