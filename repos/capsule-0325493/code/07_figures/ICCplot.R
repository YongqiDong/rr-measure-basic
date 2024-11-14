library(ggpubr)
library(foreign)
library(ggplot2)
library(latex2exp)

source(file.path(projDir,'code/global.R'))

fake <- FALSE

if (!fake){
  rawDataPath <- file.path(projDir,'data/joinDataSources/cleanData/2.RData')
  ICCPath <- file.path(projDir,'data/joinDataSources/cleanData/calcICC/paper1Update.RData')
  saveTo <- figHome
}else {
  ICCPath <- file.path(projDir,'data/joinDataSources/cleanData/removeColsRowSplit/prepImpute/makeDataFake/calcICC/1.RData')
  rawDataPath <- file.path(projDir,'data/joinDataSources/cleanData/removeColsRowSplit/prepImpute/makeDataFake/1.RData')
  saveTo <- fakeFigHome
}

#sanity check plot
# usedFiles <- checkPlot(usedFiles,getVersions(rawDataPath))
# usedFiles <- checkPlot(usedFiles,getVersions(ICCPath))

myOrder <- getSettings()$outcomes
load(ICCPath)
ICCs <- repres$res
load(rawDataPath)
raw <- repres$res
keep <- c(ICCs$outcome,'subject')
raw <- raw[,keep]


plotList <- list()
stopifnot(sort(myOrder)==sort(ICCs[,'outcome'])) #all results are here, and only those I want
rownames(ICCs) <- ICCs[,'outcome']
ICCs <- ICCs[myOrder,]
for (i in 1:nrow(ICCs)){
  ##labels and so on
  myTitle <- outcomeTranslate(ICCs$outcome[i])
  tmp <- yAxisTranslate(ICCs$outcome[i])
  ylabel <- tmp$label
  thousands <- tmp$thousands
  
  
  
  
  #actual plotting
  traw <- raw
  traw$toplot <- traw[,ICCs$outcome[i]]
  if (thousands){
    traw$toplot <- traw$toplot/1000
  }
  
  #scaling
  traw$toplot
  plotList[[i]] <- ggstripchart(traw, x = "subject", y = "toplot", size=2,position = position_jitter(0.1),color='grey',
                                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                add = "mean_sd", add.params = list(color = "black"))+ggtitle(sprintf('%s \n ICC: %.4f',outcomeTranslate(myTitle),ICCs$ICC[i]))+ylab(TeX(ylabel))+xlab('Subject')+theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))
  
}
pdf(file.path(saveTo,'1_ICC.pdf'),width=paperWidth, height=paperHeight,onefile=TRUE)
multiplot(plotlist = plotList[c(1,4,2,5,3,6)],cols=3)
dev.off()

