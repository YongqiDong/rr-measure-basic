getCorrelation <-function(dataPath,mode='default',paper=1,extraControl=NULL){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  source(file.path(projDir,'code/03_Correlations/personSpecificCorr.R'),local=TRUE)
  source(file.path(projDir,'code/global.R'),local=TRUE)
  source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'),local=TRUE)
  #load data
  load(dataPath)
  if (!is.null(repres$res$train)){
    df <- repres$res$train
  }
  else{
    df <- repres$res
  }

 #get settings
 sets <- getSettings(paper=paper)
 if(mode=='default'){
   keep <- sets$preds
   outcomes <- sets$outcomes
   lmode = 'fix'
 }else if(mode=='imp'){
   hasMis <- getMiss(df,sets$preds)>0
   outcomes <- sets$preds[hasMis] #everything I in the end want to use as predictor and has missing values
   keep <- setdiff(names(df),c(getBrain(df),'scanDate','marihuanaCigarettes_last24hs')) #all variables but the brain variables and date are potential predictor
   lmode = 'fix'
 }else if(mode=='pred'){
   keep <- sets$preds
   outcomes <- sets$preds
   lmode = 'fix'
 }else if(mode=='out'){
   keep <- sets$outcomes
   outcomes <- sets$outcomes
   lmode = 'fix'
 }else if(mode=='corWTime'){
   keep <- sets$preds
   outcomes <-'daysFromFirstScan'
   lmode = 'fix'
 }else{
   error('mode not supported')
 }
control <- c('subject',extraControl)
keep <- setdiff(keep,control)
 onlyFemale <- sets$onlyFemale
  #init result matrix
  nOut <- length(outcomes)
  nPred <- length(keep)
  result <- array(NA,dim=c(nOut,nPred,3),dimnames = list(outcomes,keep,c('error','errorRed','pVal')))
  resultModels <-matrix(list(), nrow=nOut, ncol=nPred,dimnames = list(outcomes,keep))
  #compute correlation for all outcome keep combinations
  for (i in 1:length(outcomes)){ #
    for (j in 1:length(keep)){
      if (!keep[j]==outcomes[i]  #no self correlation
          && !(outcomes[i] %in% onlyFemale && keep[j]=='gender')) #no correlation of gender with stuff that only occurs in females
        {
        #if data source is one simple data set represented by a data frame
        if(is.data.frame(df)){
          df <- transferFactors(df)
          tmp <- personSpecificCorr(df,control,keep[j],outcomes[i],mode=lmode)
          result[outcomes[i],keep[j],] <- tmp$results
          resultModels[[outcomes[i],keep[j]]] <- tmp$models
        #if data source is a multiple imputation object
        }else if (class(df)=='mids'){
          res <- LinearModelImpf(data=df,outcome=outcomes[i],special=control,mixed=FALSE,keep=keep[j])
          resSum <- summary(res)
          resultModels[[outcomes[i],keep[j]]] <- resSum
          result[outcomes[i],keep[j],] <- c(resSum$res['adj. R2','Reduced Model'],resSum$res['adj. R2','difference/pvalue'],resSum$res['F-Test','difference/pvalue'])
        }else{
          error('Should never happen')
        }
      }
    }
  }
  returnRes <- list(result=result,resultModels=resultModels)
  return(help_repro(match.call(),returnRes,environment()))
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/'))
setwd("~/Downloads/day2day/data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality")
result <- getCorrelation("1.RData")




