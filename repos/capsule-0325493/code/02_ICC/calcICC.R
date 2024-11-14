calcICC <- function(dataP){
  #according to MLM 12 HU Berlin.ppt by Oliver Luedtke
  source(file.path(projDir,'code/99_misc/help_repro.R'))
  source(file.path(projDir,'code/global.R'),local=TRUE)
  require(lme4)

  #load
  load(dataP)
  data <- repres$res
  sets <- getSettings(paper=1,exotic = FALSE)
  outcomes <- sets$outcomes
  df <- data.frame(outcome=outcomes,
                   ICC= rep(NA,length(outcomes)),withinvar=rep(NA,length(outcomes)),stringsAsFactors=FALSE)
  for (i in 1:length(df$outcome)){
    stringformula<- paste0(df$outcome[i],' ~ ',"1+(1|subject)")
    lmFull<-lmer(as.formula(stringformula),data=data)
    df[i,c('ICC','withinvar')] <- getReliability(lmFull)[c(1,3)]
  }
  return(help_repro(match.call(),df,environment))#return and write dataset to disk
}
setwd(file.path(projDir,'data/joinDataSources/cleanData/'))
tmp <- calcICC('3.RData')
