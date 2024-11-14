CIPlot <- function(plot=TRUE,standardized=FALSE,addStuff=TRUE){
  library(latex2exp)
  library(plyr)
  library(ggplot2)
  library(ggpubr)
  
  stopifnot(!standardized & addStuff)
    myResults <- c("FS_TotalGrayVol","FS_CortexVol")
  if (addStuff){
    stopifnot(!standardized)
  }

  covariates <- c('daysFromFirstScan','scanTime')
  dataFile <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/linearModelImp/reduced.RData')
  load(dataFile)
  
  
  results <- repres$res
  dataFile <- file.path(projDir,'data/joinDataSources/cleanData/prepImpute/2.RData')
  load(dataFile)

  
  means <- repres$res$means
  sds <- repres$res$sds
  resultList <- list()
  for (percenTage in c(FALSE,TRUE)){
    plotList <- list()
    for (i in 1:length(results)){ #for every outcome
      tmp <- summary(results[[i]],poolCI=TRUE)
      if (tmp$outcome %in% myResults){
        CIsall <- tmp$CIs
        CIs <- CIsall[covariates,]
        CIs <- as.data.frame(CIs)
        CIs$covariate <- row.names(CIs)
        CIs$lo95 <- -1.96*CIs$std.error+CIs$estimate
        CIs$hi95 <- 1.96*CIs$std.error+CIs$estimate
        CIs$est <- CIs$estimate 
        CIs$covariate <- sapply(CIs$covariate,variableTranslate)
        if (!standardized){
          rangeVars <- c(365,12)
          names(rangeVars) <- c('daysFromFirstScan','scanTime')
          #destandardized for all outcomes
          for (covari in covariates){
            CIs[covari,c('lo95','hi95','est')] <-  CIs[covari,c('lo95','hi95','est')]*sds[tmp$outcome]/sds[covari]
            if (addStuff){
              CIs[covari,c('lo95','hi95','est')] <-  CIs[covari,c('lo95','hi95','est')]*rangeVars[covari]
            }
            if (percenTage){
              CIs[covari,c('lo95','hi95','est')] <- 100*CIs[covari,c('lo95','hi95','est')] / means[tmp$outcome]
              myYLab <- "Predicted Change in %"
            }else{
              CIs[covari,c('lo95','hi95','est')] <- CIs[covari,c('lo95','hi95','est')] / 1000
              myYLab <- "Predicted Change in cm^3"
            }
          }
        }
        if (addStuff){
          stopifnot(CIs[,'covariate']==c('Days Since First Scan','Time of Day'))
          CIs[,'covariate'] <- c('In a Year','From 8am to 8pm')
        }
        if (plot){
          tplot <- ggplot(CIs, aes(x=reorder(covariate,est), y=est)) +
            geom_errorbar(aes(ymin=lo95, ymax=hi95), width=.1)+
            geom_point()+
            geom_hline(aes(yintercept=0))+
            theme_pubr()+
            ggtitle(outcomeTranslate(tmp$outcome))+
            ylab(TeX(myYLab))+
            xlab('')+
            theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))
          plotList[[i]] <- tplot
        }else{
          plotList[[i]] <- CIs
          names(plotList)[i] <- tmp$outcome
        }

      }

    }
    plotList <- plotList[!vapply(plotList,is.null,TRUE)]
    if(plot){
      pdf(file.path(figHome,paste0('10_CIPercent', percenTage,".pdf")),width=paperWidth, height=paperWidth*9/16,onefile=FALSE)
      multiplot(plotlist = plotList,cols=2)
      dev.off()
    }
      resultList[[percenTage+1]] <- plotList
  }
  names(resultList) <- c('raw','percent')
  return(resultList)
}