file1 <- "~/Downloads/day2day/data/joinDataSources/cleanData/prepImpute/impute/repairFactors/randomForestImp/5.RData"
file2 <- "~/Downloads/day2day/data/joinDataSources/cleanData/prepImpute/impute/repairFactors/randomForestImp/7.RData"

load(file1)
res1 <- repres
load(file2)
res2 <- repres

func1 <- res1$func 
func2 <- res2$func