
source(file.path(projDir,'/code/05a_linearModel/LinearModelf.R'))
source(file.path(projDir,'/code/global.R'))
dataFile <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/randomForestImp/3.RData')
load(dataFile)
fake <- FALSE

tmp <- summary(repres$res[[1]])
impMat <- matrix(NA,nrow = length(repres$res),ncol=length(tmp$importance))
rownames(impMat) <- lapply(repres$res, function(x) summary(x)$outcome)
colnames(impMat) <- names(tmp$importance)

#create matrix for plotting
for (res in repres$res){
  tmp <- summary(res)
  impMat[tmp$outcome,] <- tmp$importance
}
if(!fake){
  impMat <- impMat[selOutcomes,]
}
rownames(impMat) <- sapply(rownames(impMat), outcomeTranslate)
colnames(impMat) <- sapply(colnames(impMat), variableTranslate)
fakeP <-impMat
fakeP[impMat>1.960]<- 1
fakeP[impMat<=1.960]<- 0
stopifnot(all(order(impMat['FS-GM',])==order(impMat['FS-Cortex',])))
pdf(file.path(figHome,"8_rfImportance.pdf"),width=paperWidth, height=paperHeight,onefile=FALSE)
corrplot(impMat*100,is.corr = FALSE,p.mat=fakeP,col=colorRampPalette(c("red",'#bfbfbf',"black"))(200),cl.length=5,pch=1,tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"),cl.pos = 'n',cl.ratio = 0.8, method='number',addCoefasPercent = TRUE,tl.col="black")
dev.off()

