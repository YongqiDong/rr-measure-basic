removeColsRowSplit <- function(dataP){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  #load
  load(dataP)
  data <- repres$res
  #remove 10% data points per person
  freq <- table(data$subject)
  sel <- replicate(sum(freq),FALSE)
  count<-1;
  for (fr in freq){
     nsel <- round(fr/10)
     tsel <- c(replicate(nsel,TRUE),replicate(fr-nsel,FALSE))
     set.seed(3313175)
     sel[count:(count+fr-1)]<- sample(tsel,length(tsel))
     count <- count+fr
  }
  train <- data[!sel,]
  test<- data[sel,]
  res <- list(train=train,test=test)
  return(help_repro(match.call(),res))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources/cleanData'))
repres <- removeColsRowSplit('2.RData')
