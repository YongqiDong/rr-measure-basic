
source(file.path(projDir,'/code/05a_linearModel/LinearModelf.R'))
source(file.path(projDir,'/code/global.R'))
dataFile <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/linearModelImp/regularizedImp/1.RData')
load(dataFile)

tmp <- summary(repres$res[[1]])
impMat <- matrix(NA,nrow = length(repres$res),ncol=length(tmp$importance))
rownames(impMat) <- lapply(repres$res, function(x) summary(x)$outcome)
colnames(impMat) <- names(tmp$importance)

#create matrix for plotting
for (res in repres$res){
  tmp <- summary(res)
  impMat[tmp$outcome,] <- tmp$importance
}


impMat <- impMat[selOutcomes,]
impMat <- impMat[,!'(Intercept)'==colnames(impMat)]
impMat <- impMat[,seq(ncol(impMat),1,-1)]
rownames(impMat) <- sapply(rownames(impMat), outcomeTranslate)
colnames(impMat) <- sapply(colnames(impMat), variableTranslate)
pdf(file.path(figHome,"7_LASSOImportance.pdf"),width=paperWidth, height=paperHeight,onefile=FALSE)
corrplot(impMat,is.corr = FALSE,col=colorRampPalette(c("black",'#bfbfbf',"red"))(200),cl.length=5,pch=1,tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"),cl.pos = 'n',cl.ratio = 0.8, addDot = TRUE,addCoefasPercent=TRUE,method='number',tl.col="black")
dev.off()

