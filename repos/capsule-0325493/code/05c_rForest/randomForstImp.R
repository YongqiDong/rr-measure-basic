randomForestImp <- function(dataPath){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  source(file.path(projDir,'code/05c_rForest/randomForestModel.R'),local=TRUE)
  source(file.path(projDir,'code/global.R'),local=TRUE)
  

  set.seed(3313175)
  #load data
  load(dataPath)
  imp <- repres$res

  #settings
  sets <- getSettings(paper=1,fake = FALSE)
  outcomes <- sets$outcomes
  preds <- sets$preds
  onlyFemale <- sets$onlyFemale
  stopifnot(all(preds %in% names(imp$data))) #check that keeps are valid names
  stopifnot(all(onlyFemale %in% preds)) #check that onlyFemale are valid names
  outcomes <- c("FS_TotalGrayVol", "FS_CortexVol")
  #for every outcome apply randomForestmodel
  result <- list()
  for (i in 1:length(outcomes)){
    result[[i]] <- randomForestModelImp(impData=imp,outcome=outcomes[i],keep=preds,special="subject",quick=FALSE,software='breiman',perfM='new',importance=TRUE)
  }
  # return(result)
  return(help_repro(match.call(),result,environment()))
}
setwd(file.path(projDir,'/data/joinDataSources/cleanData/prepImpute/impute/repairFactors/addScanQuality/'))
randomForestImp("1.RData")

