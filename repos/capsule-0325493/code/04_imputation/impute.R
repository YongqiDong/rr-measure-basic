impute <- function(path){
  library(mice)
  library(miceadds)
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  source(file.path(projDir,'code/global.R'),local=TRUE)
  precheck <- FALSE

  load(path)
  df <- repres$res$data
  
  ##get variables for which we need to impute
  sets <- getSettings(paper=1)
  naSums <- getMiss(df,sets$preds)
  namesNotZero <- rownames(naSums$allmiss)[naSums$allmiss$miss>0]
  subVarsNotZero <- setdiff(namesNotZero,c('MR_Room_Temperature','MR_Room_Humidity'))
  #values here are set using getCorrelation and investigareCorrs
  ini <-mice(df, max=0, print=FALSE)
  predictorMatrix <- ini$predictorMatrix
  predictorMatrix[,] <- 0
  predictorMatrix[subVarsNotZero,'subject'] <- -2
  # predictorMatrix['scanTime','ruminationDuringScan'] <- 2
  predictorMatrix['MR_Room_Temperature',c('maxTemp_C','daysFromFirstScan', 'minTemp_C', 'MR_HeliumLevel','sunshine_hrs','relHumidity','precip_form','MR_Room_Humidity')] <- 1 #fixed effects because not nested
  predictorMatrix['MR_Room_Humidity',c('minTemp_C', 'maxTemp_C', 'daysFromFirstScan','MR_Room_Temperature','MR_HeliumLevel')] <- 1 #fixed effects because not nested
  predictorMatrix['caffein_last2hs_cups', c('caffein_last24hs_cups', 'liquid_last24hs_liters')] <- 2
  predictorMatrix['bloodPressure_systolic_mmHg', 'bloodPressure_diastolic_mmHg'] <- 2 #not the other way around because sys is always missing when dias is
  predictorMatrix[predictorMatrix==2] <- 1
  stopifnot(identical(rownames(predictorMatrix),names(df)))

  rnames <- rownames(predictorMatrix)
  if (precheck){
    #double check the predictorMatrix
    for (i in 1:nrow(predictorMatrix)){
      if (!rnames[i] %in% sets$preds){
        stopifnot(all(predictorMatrix[i,]==0))
        cat(paste0(rnames[i],' done\n'))
      }else{
        print(rnames[i])
        print(naSums$allmiss[,'missRel',drop=FALSE][rnames[i],])
        if (all(predictorMatrix[i,]==0)){
          print('All zero')
        }else{
          print(predictorMatrix[i,predictorMatrix[i,]!=0])
        }
        cat ("Press [enter] to continue")
        line <- readline()
      }
    }

  }

  #set appropriate imputation method for each row
  methodStrings <- rep(NA,nrow(predictorMatrix))
  names(methodStrings) <- rownames(predictorMatrix)
  for (i in 1:nrow(predictorMatrix)){
    predVector <- predictorMatrix[i,]
    if(all(predVector==0)){
      #no imputation requested
      methodStrings[i] <- ""
    }else if(all(predVector==1 | predVector==0)){
      #non nested data, both continuous
      methodStrings[i] <- 'pmm'
    }else if(all(predVector==1 | predVector==-2 |predVector==0)){ #nested
      #continous data
      if (is.numeric(df[,i])){
        methodStrings[i] <- '2l.pmm'
      }else if(is.factor(df[,i])){
        if (length(levels(df[,i]))==2){
          methodStrings[i] <- '2l.binary'
        }else{
          methodStrings[i] <- '2l.pmm'
        }
      }
    }else{
      print(predVector)
      error('Invalid value in predictor Matrix')
    }
  }

  #only user linear contrasts
  imputees <- names(methodStrings[methodStrings!=''])
  for (imputee in imputees){
    if (is.factor(df[,imputee])){
      df[,imputee] <- as.numeric(levels(df[,imputee]))[df[,imputee]]
      print(imputee)
    }
  }

  #finally impute
  df$subject <- as.numeric(levels(df$subject))[df$subject]
  imp<-mice(df, meth=methodStrings, pred=predictorMatrix, maxit=50, seed = 1111, print=TRUE,m=50)
  return(help_repro(match.call(),imp))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/prepImpute'))
impute('2.RData')
