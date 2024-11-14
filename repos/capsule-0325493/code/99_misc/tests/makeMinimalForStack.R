setwd("~/mystuff/projects/daytoday/data/cleanData/joinAc/joinNewBrainScores/removeColsRowSplit/prepare_for_imputation/impute")
load('fixedSlopes.RData')
imp <- repres$res
dataSet <- imp$data
dataSet <- dataSet[1,c(1,4)]
dataSet[1,vapply(dataSet, is.numeric, FUN.VALUE = TRUE)] <- 0.1
dataSet[1,vapply(dataSet, is.factor, FUN.VALUE = TRUE)] <- 1
dataSet[1,1] <- 'foo'
brokenDS <- dataSet


setwd("~/mystuff/projects/daytoday/data/cleanData/joinAc/joinNewBrainScores/removeColsRowSplit/prepare_for_imputation/impute")
load('randomSlopes.RData')
imp <- repres$res
dataSet <- imp$data
dataSet <- dataSet[1,c(1,4)]
dataSet[1,vapply(dataSet, is.numeric, FUN.VALUE = TRUE)] <- 0.1
dataSet[1,vapply(dataSet, is.factor, FUN.VALUE = TRUE)] <- 1
dataSet[1,1] <- 'foo'
workingDS <- dataSet

