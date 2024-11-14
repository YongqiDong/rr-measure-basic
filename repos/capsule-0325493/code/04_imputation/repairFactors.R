### repairs factors which are destroyed by imputation routine
repairFactors <- function(path){
  #was much bigger in the previos version
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  load(path)
  imp <- repres$res
  asFactor <- c('subject')
  for (fac in asFactor){
    imp$data[,fac] <- as.factor(imp$data[,fac])
  }
  # return(imp)
  return(help_repro(match.call(),imp))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/prepImpute/impute'))
impRep <- repairFactors('3.RData')
# for (j in 1:ncol(impRep$data)){ #checked using this code
#   print(names(impRep$data)[j])
#   print(impRep$data[,j])
#   cat ("Press [enter] to continue")
#   line <- readline()
# }
