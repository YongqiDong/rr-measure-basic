source(file.path(projDir,'code/global.R'),local=TRUE)
source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'),local=TRUE)

randomForestModelImp <- function(impData,...){ #see randomForestmodel for additional arguments
  require(mice)
  call <- match.call()
  if (!is.mids(impData))
    stop("The data must have class mids")
  analyses <- as.list(1:impData$m)
  redAnalyses <- as.list(1:impData$m)
  for (i in 1:impData$m) {
    print(sprintf("Imputation %d of %d",i,impData$m))
    cData <- complete(impData, i)
    tmp <- randomForestModel(cData,...)
    analyses[[i]] <- tmp$resFull
    redAnalyses[[i]] <- tmp$resRed
  }
  pooledFull <- list(call = call, call1 = impData$call, nmis = impData$nmis,
                     analyses = analyses)
  class(pooledFull) <- 'mira'
  pooledRed <- list(call = call, call1 = impData$call, nmis = impData$nmis,
                    analyses = redAnalyses)
  class(pooledRed) <- 'mira'
  result <- list(pooledFull=pooledFull,pooledRed=pooledRed)
  class(result) <- 'rfImp'
  return(result)
}


randomForestModel<- function(data,outcome,keep,special="subject",quick=FALSE,software='breiman',perfM='new',importance=TRUE){
  library(R.utils)
  assertNoMis(data,keep)
  #remove means of subject column
  saveAtt <- attributes(data[,outcome])
  #remove nas in reponse variable
  data <- data[!is.na(data[,outcome]),]
  origOutcome <- data[,outcome]
  if (!is.null(special)){
    means<-by(data[,outcome],data[,special],function(x){mean(x,na.rm=TRUE)})
    data[,outcome] <- data[,outcome] - means[data[,special]]
    attributes(data[,outcome])<- saveAtt
  }


  if (quick){
    ntree <- 50
  }else{
    ntree <- 20000
  }

  #set values for estradiol and testosterone really high for male subject
  onlyFemale <- c('estradiol','testosterone')
  maxVal <- max(data[,onlyFemale])
  data[data$gender=='male',onlyFemale] <- maxVal + 100
  assertNoMis(data,keep)
  #setup full model
  pred <- setdiff(keep,special)
  stringformula<- paste0(outcome,' ~ ',paste(pred,collapse="+"))
  res <- c()

  #run model in all implementations of random forests
  if (software=='party'){
    #make sure party kit is not loaded
    if (isPackageLoaded('partykit')){
      detach('package:partykit',unload=TRUE)
    }
    library(party)
    mycontrols <- cforest_unbiased(trace=TRUE,ntree=ntree,minsplit = 0,minbucket = 0)
    res$model <- cforest(as.formula(stringformula), data=data,controls=mycontrols)
    res$y_hat <- predict(res$model, OOB=TRUE)
  }
  else if (software=='partykit'){
    if (isPackageLoaded('party')){
      detach('package:party',unload=TRUE)
    }
    library(partykit)
    res$model <- cforest(as.formula(stringformula), data=data,trace=TRUE,ntree=ntree,cores=7,control = ctree_control(teststat = "quad",
                                                                                                                     testtype = "Univ", mincriterion = 0, minsplit = 0,minbucket = 0))
    res$y_hat <- predict(res$model, OOB=TRUE)
  }else if (software=='breiman'){
    library(randomForest)

    res$model <- randomForest(as.formula(stringformula), data=data,ntree=ntree,nodesize=1,importance=importance)
    res$y_hat <- predict(res$model)
  }

  if (perfM=='old'){
    #compute reliability
    totalVar <- var(origOutcome)
    basePred <- means[data$subject]
    rfPreds <- res$y_hat+basePred
    errorVarFull <- var(rfPreds-origOutcome)
    errorVarRed <- var(basePred-origOutcome)
    relFull <- 1-errorVarFull/totalVar
    relRed <- 1-errorVarRed/totalVar
  }else if (perfM=='new'){
    relFull <- cor(res$y_hat,data[,outcome])^2
    relRed <- NA
  }else{
    stop('Should never happen')
  }



  if (importance){
    importanceRes <- importance(res$model,type=1,scale=FALSE)
  }else{
    importanceRes <- NULL
  }
  resRed <- list(r2=relRed,outcome=outcome)
  resFull <- list(r2=relFull,outcome=outcome,importance=importanceRes)
  class(resFull) <- 'oobR2'
  class(resRed) <- 'oobR2'
  return(list(resFull=resFull,resRed=resRed))
}



plot.RandomForest <- function(cf,number=1){
  pt <- party:::prettytree(cf@ensemble[[number]], names(cf@data@get("input")))
  nt <- new("BinaryTree")
  nt@tree <- pt
  nt@data <- cf@data
  nt@responses <- cf@responses
  plot(nt)
}

summary.rfImp <- function(result){
  out <- summary.regImp(result)
  class(out) <- 'rfSum'
  return(out)
}




