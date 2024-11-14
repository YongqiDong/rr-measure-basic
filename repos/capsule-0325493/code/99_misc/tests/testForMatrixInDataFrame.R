setwd("~/mystuff/projects/daytoday/data/cleanData/joinAc/joinNewBrainScores/removeColsRowSplit/prepare_for_imputation")
load('2.RData')
imp <- repres$res
dataSet <- imp
print(class(dataSet$FS_bilat_rostralmiddlefrontal_thickness))