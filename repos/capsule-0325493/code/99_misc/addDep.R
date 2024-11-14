addDep <- function (dataP){
  source('~/mystuff/general_R_code/my_helper/help_repro.R')
  load(dataP)
  repres$res$train$Total <- repres$res$train$Total + 5*repres$res$train$bodyWater
  return(help_repro(match.call(),repres$res))
}
addDep('1.RData')