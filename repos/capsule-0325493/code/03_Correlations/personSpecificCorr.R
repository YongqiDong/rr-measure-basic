personSpecificCorr <- function(dfOrig,subjectVar,controlVar,outComeVar,mode='random'){
  stopifnot(is.factor(dfOrig[,subjectVar[1]]))
  #some data set for both models, for comparsion and to be able to do lr test
  dfOrig <- dfOrig[,c(outComeVar,subjectVar,controlVar)]
  Norig <- nrow(dfOrig)
  df <- na.omit(dfOrig)
  #print(sprintf('For %s, and %s, reduced to %.2f %% of cases ',outComeVar,controlVar,nrow(df)/Norig))
  if (identical(mode,'fix')){ #only used for imputation because easier/faster
    #reduced model
    formulaRed <- paste0(outComeVar,' ~ ',paste(subjectVar,collapse = '+'))
    if (!is.factor(df[,outComeVar])){
      redLm <- lm(as.formula(formulaRed),data=df)
    }else{
      redLm  <-  glm(as.formula(formulaRed),data=df,family=binomial())
    }
    #full model
    formula <- paste0(formulaRed,'+',controlVar)
    if (!is.factor(df[,outComeVar])){
      fullLm <- lm(as.formula(formula),data=df)
      #compare models

      #adjusted noise
      sumRed <- summary(redLm)
      noiseRed <- 1-sumRed$adj.r.squared

      sumFull <- summary(fullLm)
      noiseFull <- 1-sumFull$adj.r.squared
      noiseDiff <- noiseRed - noiseFull

      #ftest
      fstat <- anova(fullLm,redLm)
      pVal <- fstat[['Pr(>F)']][2]
    }else{
      fullLm <- glm(as.formula(formula),data=df,family = binomial())
      tmp <- summary(fullLm)
      pVal = tryCatch({
        tmp$coefficients[controlVar,'Pr(>|z|)']
      }, error = function(e) {
        tmp$coefficients[paste0(controlVar,'1'),'Pr(>|z|)']
      })
      noiseRed <- NA
      noiseDiff <- NA
    }
    return(list(results=c(noiseRed,noiseDiff,pVal),models=list(full=fullLm,red=redLm)))
  }else if(identical(mode,'random')){
    warning('deprecated')
    require(lme4)
    #first simple model with only subject as predictor
    formulaRed <- paste0(outComeVar,' ~ ','1+(1|',subjectVar,')')
    if (!is.factor(df[,outComeVar])){
      lmRed<-lmer(as.formula(formulaRed),data=df)
    }else{
      lmRed <- glmer(as.formula(formulaRed),data=df,family=binomial())
    }
    relRed <- getReliability(lmRed)

    #then model with predictor added
    formulaFull <- paste0(formulaRed,'+',controlVar)

    if (!is.factor(df[,outComeVar])){
      lmFull<-lmer(as.formula(formulaFull),data=df)
    }else{
      lmFull <- glmer(as.formula(formulaFull),data=df,family=binomial())
    }
    relFull <- getReliability(lmFull)
    relImprov <- relFull-relRed
    #model comparison
    tmp <- anova(lmFull,lmRed)
    pValue <- tmp$`Pr(>Chisq)`[2]
    return(c(1-relRed,relImprov,pValue))
  }else{
    stop(sprintf('Wrong mode %s',mode))
  }
}
