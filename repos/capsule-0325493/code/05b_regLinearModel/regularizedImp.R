source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'),local=TRUE)
library(plyr); library(dplyr)
print.glmSum <- function(sum){
  print.linearModelImpSum(sum)
}

revertContrast <- function(contrastMat){
  patterns <- plyr::count(contrastMat)
  nFac <- nrow(patterns)
  res <- factor(x=rep(NA,nrow(contrastMat)),level=1:nFac)
  patterns <-patterns[,1:(ncol(patterns)-1)]
  for (i in 1:nrow(patterns)){
    res[apply(contrastMat, 1, function(x){all(x == patterns[i,])})] <- i
  }
  return(res)
}

nestedCV <-  function(x,y,pen,means=NULL,perfM,importance){
  #initialize
  stopifnot(pen==TRUE || is.null(means)) #stop if regular model is used with demeaned data, does not make sense
  nFolds <- 10
  N <- length(y)
  preds <- rep(NA,N)
  preds2 <- rep(NA,N)
  
  #make reproducible partition of dataset for cross-validation
  fold_association <- rep(1:nFolds, N/nFolds + 1)[1:N]
  set.seed(1946019467)
  fold_association <- sample(fold_association, N, replace=F)
  
  #for each fold test on the fold, train on the rest
  for (cFold in 1:nFolds){
    #get samples
    testSamples <- fold_association==cFold
    trainSamples <- !testSamples
    
    #predictions full model
    if (pen){ #lasso regression
      #train with all folds but 1
      trainedModel <- cv.glmnet(x[trainSamples,],y[trainSamples])
      #test with rest
      preds[testSamples] <- predict(trainedModel,newx=x[testSamples,], s="lambda.1se")
    }else{ #standard linear regression
      df <- as.data.frame(cbind(y[trainSamples],x[trainSamples,]))
      colnames(df)[1] <- 'out'
      trainedModel <- lm(out ~ . -1,data=df) # no intercept because we have explicit intercept in data
      preds[testSamples] <- predict(trainedModel,as.data.frame(x[testSamples,]))
    }
    
    #predictions reduced model, if data are demeaned, i.e., !is.null(means). this is simply the mean for every subject
    if (is.null(means)){
      if (mixed){
        newx <- x[,c('1','3','5','6','7','8')]
      }else{
        newx <- x[,c("(Intercept)","subject3","subject5","subject6","subject7","subject8")]
      }
      
      if (pen){
        trainedModel2 <- cv.glmnet(newx[trainSamples,],y[trainSamples])
        preds2[testSamples] <- predict(trainedModel2,newx=newx[testSamples,], s="lambda.min")
      }else{
        df <- as.data.frame(cbind(y[trainSamples],newx[trainSamples,]))
        colnames(df)[1] <- 'out'
        trainedModel <- lm(out ~ . -1,data=df)
        preds2[testSamples] <- predict(trainedModel,as.data.frame(newx[testSamples,]))
      }
    }  
  } #end of for each fold
  
  if (perfM=='old'){
    if (!is.null(means)){
        origY <- means$origY
        preds2 <- means$means[means$subjects]
        preds <- preds2 + preds  
    }else{
      origY <- y
    }
  
    #calculate out of sample r2 differences for both models
    totalVar <- var(origY)
    errorVarFull <- var(preds-origY)
    errorVarRed <- var(preds2-origY)
    relFull <- 1-errorVarFull/totalVar
    relRed <- 1-errorVarRed/totalVar
  }else{
    relFull <- cor(y,preds)^2
    relRed <- NA
  }
  if (importance){
    #standardize both x and y
    xstd <- scale(x)
    ystd <- scale(y)
    glmnetModel <- cv.glmnet(xstd,ystd)
    importance <- coef(glmnetModel, s = "lambda.min")
  }
  resFull <- list(r2=relFull,importance=importance)
  res <- list(full=resFull,reduced=relRed)
  return(res)
}



regularizedImp <- function(dataPath,pen=TRUE,predVars=NULL,predError=TRUE,perfM='new',importance=TRUE){
  stopifnot(perfM=='old' | predError)
  #load stuff
  source('~/mystuff/others/general_R_code/my_helper/help_repro.R')
  library(glmnet)
  library(lme4)
  call <- match.call()
  set.seed(1011)

  #load data
  load(dataPath)
  impData <- repres$res

  #compute results
  result <- list()
  for (i in 1:length(impData)){ #for every outcome
    analyses <- impData[[i]]$pooledFull$analyses
    redAnalyses <- analyses
    for (j in 1:length(analyses)){ #for every imputed dataset
      cAnalysis <- analyses[[j]]
      #get data
      stopifnot(class(cAnalysis)=='lm')
      outcome <- names(cAnalysis$model)[1]
      x <- model.matrix(cAnalysis)
      y <- cAnalysis$model[,outcome]
      #reduce x
      if (!is.null(predVars)){
        x <- x[,c("(Intercept)","subject3","subject5","subject6","subject7","subject8",predVars)]
      }
      if (predError){
        #remove subject specific means
        origY <- y
        #recode dummy coding to factor
        subjects <- revertContrast(x[,c("(Intercept)","subject3","subject5","subject6","subject7","subject8")])
        means<-by(y,subjects,function(x){mean(x,na.rm=TRUE)})
        y <- as.vector(y - means[subjects])
        meanStruc <- list(means=means,origY=origY,subjects=subjects)
        #remove the subject stuff from x
        x <- x[,!(colnames(x) %in% c("(Intercept)","subject3","subject5","subject6","subject7","subject8"))]
      }else{
        meanStruc <- NULL
      }
      #perform analysis
      tmp <- nestedCV(x,y,pen=pen,means=meanStruc,perfM,importance)
      
      #propagate results
      resFull <- list(r2=tmp$full$r2,outcome=outcome,importance=tmp$full$importance)
      class(resFull) <- 'oobR2'
      analyses[[j]] <- resFull
      resRed <- list(r2=tmp$reduced,outcome=outcome)
      class(resRed) <- 'oobR2'
      redAnalyses[[j]] <- resRed
      class(redAnalyses[[j]]) <- 'oobR2'
    }
    #gather results of all imputations
    pooledFull <- impData[[i]]$pooledFull
    pooledFull$call <- call
    pooledFull$analyses <- analyses
    pooledRed <- pooledFull
    pooledRed$analyses <- redAnalyses
    result[[i]] <- list(pooledFull=pooledFull,pooledRed=pooledRed)
    class(result[[i]]) <- 'regImp'
  }
  return(help_repro(match.call(),result,environment()))
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/linearModelImp'))
regularizedImp('2.RData',pen=TRUE,predError=TRUE,perfM='new',importance=TRUE)