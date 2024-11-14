 myprint <-function(x){
  print(format(x,digits=4,scientific=FALSE))
}
imputation <- FALSE

if (imputation){
  threshold <- 0.01
}else{
  threshold <- 0.0
}
 
load(file.path(projDir,'data/joinDataSources/cleanData/getCorrelation/paper1UpdatePreds.RData'))
corrs <- repres$res$result
load(file.path(projDir,'data/joinDataSources/cleanData/3.RData'))
myData <- repres$res
varNames <- rownames(corrs)
d <- colSums(is.na(myData))
d <- d[varNames]
for (i in 1:length(varNames)){
  print(varNames[i])
  print(d[i])
  if (all(is.na(corrs[varNames[i],,1]))){
    #we have binary variable, print p val
    cat('PValues\n')
    cat('PValues\n')
    myprint(sort(corrs[varNames[i],,'pVal']))
  }else{
    #we have continous variable, print errorRed
    cat('ErrorRed\n')
    myprint(sort(corrs[varNames[i],corrs[varNames[i],,'errorRed']>threshold,'errorRed'],decreasing = TRUE))
  }
  cat ("Press [enter] to continue")
  line <- readline()
}
