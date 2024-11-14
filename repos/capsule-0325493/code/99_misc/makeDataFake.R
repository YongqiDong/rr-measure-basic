makeDataFake <- function(path){
  source('~/mystuff/others/general_R_code/my_helper/help_repro.R')
  source('~/mystuff/projects/daytoday/code/global.R',local=TRUE)
  library(plyr)

  load(path)
  df <- repres$res
  sets <- getSettings(fake=TRUE)
  preds <- sets$preds
  outcomes <- sets$outcomes
  df <- rename(df, c('FS_bilat_rostralmiddlefrontal_thickness' = outcomes[1],'FS_bilat_Hippocampus' = outcomes[2]))


  df[,preds] <- rnorm(prod(dim(df[,preds])))
  #no signal
  df[,outcomes[2]] <- as.numeric(levels(df$subject))[df$subject]  + sqrt(2)*rnorm(nrow(df))
  #signal
  df[,outcomes[1]] <- as.numeric(levels(df$subject))[df$subject] + df$bloodPressure_diastolic_mmHg+rnorm(nrow(df))
  
  #check my assumptions
  demeaned <- df[,outcomes[1]] - as.numeric(levels(df$subject))[df$subject]
  predictions <- df$bloodPressure_diastolic_mmHg
  print(cor(predictions,demeaned)^2)
  ##insert 10% missingness
  for (i in 1:nrow(df)){
    for (col in preds){
      if (rbinom(1,1,0.1)){
        df[i,col] <- NA
      }
    }
  }
  # return(help_repro(match.call(),df))#return and write dataset to disk
}


setwd("~/mystuff/projects/daytoday/dataRestart/joinDataSources/cleanData/removeColsRowSplit/prepImpute")
tmp <- makeDataFake('1.RData')



