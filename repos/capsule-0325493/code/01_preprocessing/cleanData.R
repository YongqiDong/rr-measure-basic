cleanData <- function(filePath){
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  load(filePath)
  myData <- repres$res
  
  ##convert stuff to factors
  myData$subject <- as.factor(myData$subject)
  myData$sleptDuringScan <- as.factor(myData$sleptDuringScan)
  myData$precip_form <- as.factor(myData$precip_form)
  myData$rememberDreams_fromLastNight <-as.factor(myData$rememberDreams_fromLastNight)
  toOrder <- c("ruminationDuringScan","anxietyDuringScan","sweets_last24hs_Likert","physicalPainDuringScan","generalHealth_last24hs",
               "generalStress_last24hs","easeOfConcentration_last24hs",names(myData)[grep("PANAS*",names(myData))],
               "sleepQuality_lastNight","dayDreams_last24hs")
  for (i in toOrder){
    myData[,i] <- as.ordered(myData[,i])
  }
  tData <- myData[,setdiff(names(myData),"scanDate")]
  
  ##assert that all the other variants of missing values that have been there
  ##are not there anymore
  stopifnot(!any(tData==-99 | tData==-999,na.rm=TRUE))
  stopifnot(!any(is.nan(as.matrix(myData)),na.rm=TRUE))
  stopifnot(!any(tData=='NaN',na.rm=TRUE))
  stopifnot(!any(tData=="   ",na.rm=TRUE))

  myData$chocolate_cacao_24hr <- myData$cacao_last24hs_percentage*myData$chocolate_last24hs_gramms/100
  myData$chocolate_cacao_2hr <- myData$cacao_last2hs_percentage*myData$chocolate_last2hs_gramms/100
  myData <- droplevels(myData) #get rid of NaN factor levelss
  
  #real corrections
  myData$FS_bilat_Hippocampus[myData$subject==7 & myData$scanDate=='2013-09-24'] <- NA   
  return(help_repro(match.call(),myData))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources'))
cleanData('3.RData')