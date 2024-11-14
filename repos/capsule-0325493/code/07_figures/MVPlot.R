#libraries
library(ggpubr)
library(ggplot2)
library(scales)
library(latex2exp)
source(file.path(projDir,'/code/05a_linearModel/LinearModelf.R'))
source(file.path(projDir,'/code/05c_rForest/randomForestModel.R'))

fake <- FALSE
paperHeight = paperWidth*3/4 #to get 4:3 ratio



if (fake){
  regPath <- file.path(projDir,'data/joinDataSources/cleanData/removeColsRowSplit/prepImpute/makeDataFake/impute/repairFactors/linearModelImp/regularizedImp/3.RData')
  rfPath <- file.path(projDir,'data/joinDataSources/cleanData/removeColsRowSplit/prepImpute/makeDataFake/impute/repairFactors/randomForestImp/8.RData')
}else{
  regPath <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/linearModelImp/regularizedImp/1.RData')
  rfPath <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/randomForestImp/3.RData')
}
#sanity check plot
# usedFiles <- checkPlot(usedFiles,getVersions(regPath))
# usedFiles <- checkPlot(usedFiles,getVersions(rfPath))

inDF <- data.frame(path=c(regPath,rfPath),
                     names=c('LASSO','Random Forest'),stringsAsFactors = FALSE)

##load all files
resultList <- list()
for (i in 1:nrow(inDF)){
  load(inDF$path[i])
  if (i==1){
    resultList[[i]] <- repres$res[4:5]  
  }else{
    resultList[[i]] <- repres$res
  }
  if (i>1){
    stopifnot(length(resultList[[i]])==length(resultList[[i-1]]))
  }
}


plotList <- list()
#for every outcome
for (i in 1:length(resultList[[i]])){
  cRes <- summary(resultList[[1]][[i]])
  #convert to key value format
  tdata <- data.frame(Model=inDF$names[1],Value=cRes$res[1,'Difference'],stringsAsFactors = FALSE)
  outcome <- cRes$outcome
  for (j in 2:length(resultList)){
    cRes <- summary(resultList[[j]][[i]])
    stopifnot(outcome==cRes$outcome)
    tdata <- rbind(tdata,data.frame(Model=inDF$names[j],Value=cRes$res[1,'Difference'],stringsAsFactors = FALSE))
  }
  tdata$Model <- as.factor(tdata$Model)
  tdata$Value <- tdata$Value*100
  #plot
  plotList[[i]] <- ggbarplot(tdata, x = "Model", y = "Value",label = FALSE,order=tdata$Model)+
    ggtitle(outcomeTranslate(cRes$outcome))+
    ylab( TeX('Out-of-Sample $R^2$ in %'))+
    xlab("")+
    theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"),axis.text.x = element_text(angle = 0, hjust = 0.5))+
    scale_y_continuous(labels = comma,limits = c(0,4))
}

if(!fake){
  pdf(file.path(figHome,'6_MV1.pdf'),width=paperWidth, height=paperHeight*0.4,onefile=FALSE)
  multiplot(plotlist =plotList,cols=2)
  dev.off()
}else{
  pdf(file.path(fakeFigHome,'6_MV1.pdf'),width=paperWidth, height=paperHeight,onefile=FALSE)
  multiplot(plotlist = plotList[1:2],cols=2)
  dev.off()
}
