linearModelImp <- function(dataPath){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  source(file.path(projDir,'code/global.R'),local=TRUE)
  source(file.path(projDir,'code/05a_linearModel/LinearModelf.R'),local=TRUE)

  #load data
  load(dataPath)
  imp <- repres$res

  #settings
  sets <- getSettings(paper=1,reduced=TRUE)
  outcomes <- sets$outcomes
  preds <- sets$preds
  onlyFemale <- sets$onlyFemale
  stopifnot(all(preds %in% names(imp$data))) #check that keeps are valid names
  stopifnot(all(onlyFemale %in% preds)) #check that onlyFemale are valid names

  #for every outcome apply linear model
  result <- list()
  for (i in 1:length(outcomes)){
    result[[i]] <- LinearModelImpf(data=imp,outcome=outcomes[i],keep=preds,mixed=FALSE,onlyFemale=onlyFemale)
  }
  # return(result)
  return(help_repro(match.call(),result,environment()))
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality'))
linearModelImp('1.RData')

