joinDataSources <- function(dataP){
source(file.path(projDir,'code/99_misc/help_repro.R'))
library(jsonlite)
library(foreign)
subjs <- setdiff(1:8,c(2,4)) #drop subjects 2 and 4, because they have only a limited number of timepoints
#load questionaire, scanner and hormone data
quest <- data.frame()
for (subj in subjs) {
  cquest <- as.data.frame(fromJSON(paste0("questionaire/QuestionnaireResponses_Subj0", subj, "_clean.txt")))
  cquest$subject <- rep(subj,nrow(cquest))
  quest <- rbind(quest, cquest)
}

#load brain
options(warn=-1)
brain <- read.spss(file=paste0('brain/brain.sav'),to.data.frame=TRUE)
options(warn=0)

#load fitbit
activities <- data.frame()
for (subj in subjs) {
  activity <- as.data.frame(fromJSON(paste0("fitbit/fitbitData_Subj0", subj, ".txt")))
  activity$subject 	<- rep(subj,nrow(activity))
  activities <- rbind(activities, activity)
}

#join brain with questionaire
names(brain)[names(brain)=="date"] <- "scanDate"
brain$scanDate<-as.Date(brain$scanDate,"%Y-%m-%d",tz="CET")
quest$scanDate<-as.Date(quest$scanDate,"%Y-%m-%d",tz="CET")
brainQuest <- merge(brain,quest,by=c("subject",  "scanDate"))
#for two occassions the questionaire data is missing. because subject 7 did not complete the questionaire on two occasions


#join the rest with fitbit, always use 1 day before the scanning as discusses with elisa
#rationale is that
#1. we cant get hourly data, otherwise we would use some constant number of hourse before the scan
#2. The day of the scanning mostly contains data from the future besides the way to work which should be relatively constant within subjects

activities$Date <- as.Date(activities$Date,"%d.%m.%Y")
activities$Date <- activities$Date -1
#remove this variables because they are not used currently
activities$time_slept_min <- NULL
activities$time_bed <- NULL
names(activities)[names(activities)=="Date"] <- "scanDate"
allData <- merge(brainQuest,activities,by=c("subject",  "scanDate"))

##remove duplicated in allData
duplicateIndex <- duplicated(allData$FS_CortexVol)
duplicatesA <- which(allData$FS_CortexVol[duplicateIndex] == allData$FS_CortexVol)
allData <- allData[-duplicatesA[1],]



##merge with new scanner quality data
scanQuality <- read.csv('brain/scanQuality.txt',sep='\t')
scanQuality$FS_CortexVol <- scanQuality$CortexVol
scanQuality$FS_TotalGrayVol <- scanQuality$TotalGrayVol
scanQuality <- scanQuality[,c("FS_CortexVol","SurfaceHoles")]

##remove duplicate in scanQuality
duplicateIndex <- duplicated(scanQuality$FS_CortexVol)
duplicates <- which(scanQuality$FS_CortexVol[duplicateIndex] == scanQuality$FS_CortexVol)
scanQuality <- scanQuality[-duplicates[1],]
allDataNew <- merge(allData,scanQuality,by=c('FS_CortexVol'),all.x=TRUE,all.y=FALSE,sort=FALSE)
return(help_repro(match.call(),allDataNew))
}
setwd(file.path(projDir,'data/'))
joinDataSources()
