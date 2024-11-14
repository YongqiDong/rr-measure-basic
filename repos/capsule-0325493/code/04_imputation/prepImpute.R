betterScale <- function(x){
  y <- scale(x)
  return(as.numeric(y))
}

prepImpute <- function(filePath){
  library(dplyr)
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  load(filePath)
  data <- repres$res
  brainVars <- c("FS_bilat_rostralmiddlefrontal_thickness","FS_bilat_Hippocampus","FS_bilat_medialorbitofrontal_thickness","VBM_BA9_46_AAL","VBM_HC_AAL","VBM_mOFC_AAL",
               "FS_long_bilat_rostralmiddlefrontal_thickness","FS_long_bilat_Hippocampus",
               "FS_long_bilat_medialorbitofrontal_thickness","longVBM_BA9_46_AAL","longVBM_HC_AAL","longVBM_mOFC_AAL","ICV_ASHS",
               "FS_CortexVol","FS_TotalGrayVol","FS_long_CortexVol","FS_long_TotalGrayVol","GM","WM","CSF","Total")
#standardize all numeric variables
stopifnot(all(!vapply(data, is.matrix, FUN.VALUE = TRUE)))
numVars <- setdiff(names(data)[vapply(data,is.numeric,FUN.VALUE = TRUE)],'marihuanaCigarettes_last24hs')
dataOrig <- data
data<- data %>% mutate_each_(funs(betterScale),vars=numVars)
stopifnot(all(!vapply(data, is.matrix, FUN.VALUE = TRUE)))

#check if scaling does exactly what I assume
means <- colMeans(dataOrig[,numVars],na.rm=TRUE)
mySd <- function (x) sd(x, na.rm = TRUE)
sds <- apply(dataOrig[,numVars], 2, mySd)  
reScaled <- data
reScaled[,numVars] <- reScaled[,numVars]*matrix(rep(sds,each=nrow(reScaled)),nrow=nrow(reScaled))
reScaled[,numVars] <- reScaled[,numVars]+matrix(rep(means,each=nrow(reScaled)),nrow=nrow(reScaled))
stopifnot(all.equal(reScaled,dataOrig))
#save which variables are brain vars and which not
for (i in 1:ncol(data)){
  if (names(data)[i] %in% brainVars){
    attr(data[,i],'brain')  <- TRUE
  }else{
    attr(data[,i],'brain')  <- FALSE
  }
}
 return(help_repro(match.call(),list(data=data,means=means,sds=sds)))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/'))
res <- prepImpute('3.RData')
