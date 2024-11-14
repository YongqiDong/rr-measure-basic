#packages
library(corrplot)
source(file.path(projDir,'/code/05a_linearModel/LinearModelf.R'))

paperHeight = paperWidth*3/4 #to get 4:3 ratio
corPathList <-  list(file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/linearModelImp/1.RData'))
savePathList <- list(file.path(figHome,'5_ANOVA'))
load(corPathList[[1]])

tmp <- summary(repres$res[[1]],poolCI=TRUE)
corrMat <- matrix(NA,nrow = length(repres$res),nrow(tmp$CIs)-6)
rownames(corrMat) <- lapply(repres$res, function(x) summary(x)$outcome)
colnames(corrMat) <- setdiff(rownames(tmp$CIs),c("(Intercept)","subject3","subject5","subject6","subject7","subject8"))
#create matrix for plotting
for (res in repres$res){
  tmp <- summary(res,poolCI=TRUE)
  corrMat[tmp$outcome,] <- tmp$CIs[colnames(corrMat),"p.value"]
}
corrMat <- corrMat[selOutcomes,]
rownames(corrMat) <- sapply(rownames(corrMat), outcomeTranslate)
colnames(corrMat) <- sapply(colnames(corrMat), variableTranslate)
pdf(paste0(savePathList[[1]],".pdf"),width=paperWidth*1.1, height=paperHeight,onefile=FALSE)
fakeP <- corrMat
fakeP[corrMat<0.05]<-1
fakeP[corrMat>0.05]<-0
corrplot(corrMat,is.corr = FALSE,method="number",col=colorRampPalette(c("red","black","#bfbfbf"))(200),addCoefasPercent=TRUE,p.mat = fakeP,pch = 1,pch.cex = 4.5,cl.pos = 'n',cl.ratio = 0.6,tl.col="black",addDot = TRUE)
dev.off()
