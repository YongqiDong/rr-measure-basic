library(mice)
library(glmnet)
# --------------------------pool.r.squared--------------------------

#'Pooling: R squared
#'
#'Pools R^2 of m repeated complete data models.
#'
#'The function pools the coefficients of determination R^2 or the adjusted
#'coefficients of determination (R^2_a) obtained with the \code{lm} modelling
#'function. For pooling it uses the Fisher \emph{z}-transformation.
#'
#'@param object An object of class 'mira', produced by \code{lm.mids} or
#'\code{with.mids} with \code{lm} as modelling function.
#'@param adjusted A logical value. If adjusted=TRUE then the adjusted R^2 is
#'calculated.  The default value is FALSE.
#'@return Returns a 1x4 table with components. Component \code{est} is the
#'pooled R^2 estimate. Component \code{lo95} is the 95 \% lower bound of the pooled R^2.
#'Component \code{hi95} is the 95 \% upper bound of the pooled R^2.
#'Component \code{fmi} is the fraction of missing information due to nonresponse.
#'@author Karin Groothuis-Oudshoorn and Stef van Buuren, 2009
#'@seealso \code{\link{pool}},\code{\link{pool.scalar}}
#'@references Harel, O (2009). The estimation of R^2 and adjusted R^2 in
#'incomplete data sets using multiple imputation, Journal of Applied Statistics,
#'36:1109-1118.
#'
#'Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys.  New
#'York: John Wiley and Sons.
#'
#'van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}: Multivariate
#'Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#'Software}, \bold{45}(3), 1-67. \url{http://www.jstatsoft.org/v45/i03/}
#'

#'@keywords htest
#'@examples
#'
#'
#'imp<-mice(nhanes)
#'
#'fit<-lm.mids(chl~age+hyp+bmi,imp)
#'pool.r.squared(fit)
#'pool.r.squared(fit,adjusted=TRUE)
#'
#'#fit<-lm.mids(chl~age+hyp+bmi,imp)
#'#
#'#> pool.r.squared(fit)
#'#          est     lo 95     hi 95       fmi
#'#R^2 0.5108041 0.1479687 0.7791927 0.3024413
#'#
#'#> pool.r.squared(fit,adjusted=TRUE)
#'#          est      lo 95    hi 95       fmi
#'#adj R^2 0.4398066 0.08251427 0.743172 0.3404165
#'#
#'
#'
#'@export
#' Adaptation by JK to also support lme analyses
pool.r.squared2 <- function(object, adjusted = FALSE) {
  # pooled rsquared for multiple imputed datasets.
  #
  # object: object of class mira based on article of O. Harel (Journal of Applied Statistics, 2009).

  call <- match.call()
  if (!is.mira(object))
    stop("The object must have class 'mira'")
  if ((m <- length(object$analyses)) < 2)
    stop("At least two imputations are needed for pooling.\n")
  if (class((object$analyses[[1]]))[1] != "lm" & class((object$analyses[[1]]))[1] != "lmerMod" & class((object$analyses[[1]]))[1] != "oobR2")
    stop("r^2 can only be calculated for results of the 'lm' or 'lmer' modelling functions")
  # Set up array r2 to store R2 values, Fisher z-transformations of R2 values and its variance.
  analyses <- object$analyses
  isLmer <- class(analyses[[1]])=='lmerMod'
  isGLMNET <-  class(analyses[[1]])=='oobR2'
  stopifnot(!(isLmer & isGLMNET)) #can only by one
  m <- length(analyses)
  r2 <- matrix(NA, nrow = m, ncol = 3, dimnames = list(1:m, c("R^2", "Fisher trans F^2", "se()")))
  # Fill arrays
  for (i in 1:m) {
    fit <- analyses[[i]]
    if (isLmer){
      stopifnot(class(fit)[1]=='lmerMod')
      r2[i, 1] <- sqrt(getReliability(fit))
    }else if (isGLMNET){
      stopifnot(class(fit)[1]=='oobR2')
      r2[i, 1] <- sqrt(fit$r2)
    }
    else{
      stopifnot(class(fit)[1]=='lm')
      if (adjusted == FALSE)
        r2[i, 1] <- sqrt(summary(fit)$r.squared) else r2[i, 1] <- sqrt(summary(fit)$adj.r.squared)
    }
      r2[i, 2] <- 0.5 * log((r2[i, 1] + 1)/(1 - r2[i, 1]))
      r2[i, 3] <- NA
  }
  # Compute within, between and total variances following Rubin's rules.  with function pool.scalar().
  fit <- pool.scalar(r2[, 2], r2[, 3])

  # Make table with results.
  table <- array(((exp(2 * fit$qbar) - 1)/(1 + exp(2 * fit$qbar)))^2, dim = c(1, 4))

  if (adjusted == FALSE)
    dimnames(table) <- list("R^2", c("est", "lo 95", "hi 95", "fmi")) else dimnames(table) <- list("adj R^2", c("est", "lo 95", "hi 95", "fmi"))

  table[, 2] <- ((exp(2 * (fit$qbar - 1.96 * sqrt(fit$t))) - 1)/(1 + exp(2 * (fit$qbar - 1.96 * sqrt(fit$t)))))^2
  table[, 3] <- ((exp(2 * (fit$qbar + 1.96 * sqrt(fit$t))) - 1)/(1 + exp(2 * (fit$qbar + 1.96 * sqrt(fit$t)))))^2
  table[, 4] <- fit$f
  return(table)
}



LinearModelImpf<-function(data,outcome,special="subject",keep=NULL,mixed=TRUE,onlyFemale=NULL){
  require(mice)
  if (mixed){
    require(lme4)
  }
  call <- match.call()
  if (!is.mids(data))
    stop("The data must have class mids")
  analyses <- as.list(1:data$m)
  redAnalyses <- as.list(1:data$m)
  for (i in 1:data$m) {
    cData <- complete(data, i)
    cData <- transferFactors(cData)
    #set stuff that only happens in female to 0 for males, i.e. only the interaction is modelled
    cData[cData$gender=='male',onlyFemale] <- 0
    N<-nrow(cData)
    if (is.null(keep)){
      keep <- names(cData)
    }
    pred<- setdiff(keep,outcome) #make sure outcome is not in predictors

    #create appropriate formuals
    if (mixed){
      specialChar <- paste0('(1|',special,')')
      lmf <- lmer
      warning('Deprecated, might not work')
    }else{
      specialChar <- special
      lmf <- lm
    }
    assertNoMis(cData,c(special,keep))
    redFormula <- paste0(outcome,'~',paste(specialChar,collapse="+"))
    fullFormula <- paste0(redFormula,' + ',paste(pred,collapse="+"))
    ##fit  full model
    fullLM  = tryCatch({
      lmf(as.formula(fullFormula),data=cData)
    }, warning = function(w) {
      lmf(as.formula(fullFormula),data=cData,control=lmerControl((optimizer="Nelder_Mead")))
    }, error = function(e) {
      lmf(as.formula(fullFormula),data=cData,control=lmerControl((optimizer="Nelder_Mead")))
    })

    ##fit reduced model
    redModel <- lmf(as.formula(redFormula),data=cData)
    analyses[[i]] <- fullLM
    redAnalyses[[i]] <- redModel
  }
  pooledFull <- list(call = call, call1 = data$call, nmis = data$nmis,
                 analyses = analyses)
  pooledRed <- list(call = call, call1 = data$call, nmis = data$nmis,
                     analyses = redAnalyses)
  oldClass(pooledFull) <- c("mira", "matrix")
  oldClass(pooledRed) <- c("mira", "matrix")
  result <- list(pooledFull=pooledFull,pooledRed=pooledRed)
  class(result) <- 'LinearModelImp'
  return(result)
}

summary.LinearModelImp <- function(result,poolCI=FALSE){
  out <- matrix(nrow=2,ncol=3)
  rownames(out) <- c('adj. R2','F-Test')
  colnames(out) <- c('Full Model','Reduced Model','difference/pvalue')
  r2Full <- pool.r.squared2(result$pooledFull,adjusted =TRUE)
  coefs <- summary(pool(result$pooledFull))
  r2Red <- pool.r.squared2(result$pooledRed,adjusted =TRUE)
  fTest <- pool.compare(result$pooledFull,result$pooledRed)
  out['adj. R2','Full Model'] <- r2Full[,'est']
  out['adj. R2','Reduced Model'] <- r2Red[,'est']
  out['adj. R2','difference/pvalue'] <- out['adj. R2','Full Model']-out['adj. R2','Reduced Model']
  out[2,3] <- fTest$pvalue
  firstRed <-result$pooledRed$analyses[[1]]
  if (!class(firstRed)=='lm'){
    outcome <- colnames(result$pooledRed$analyses[[1]]@frame)[1]
    predictors <- list(full=setdiff(all.vars(result$pooledFull$analyses[[1]]@call$formula),outcome),red=setdiff(all.vars(result$pooledRed$analyses[[1]]@call$formula),outcome))
  }else{
    outcome <- names(firstRed$model)[1]
    predictors <- list(full=setdiff(names(result$pooledFull$analyses[[1]]$model),outcome),red=setdiff(names(result$pooledRed$analyses[[1]]$model),outcome))
  }
  realOut <- list(res=out,outcome=outcome,predictors=predictors)
  if (poolCI){
    realOut$CIs <- summary(pool(result$pooledFull))
  }
  class(realOut) <- 'linearModelImpSum'
 return(realOut)
}

print.linearModelImpSum <- function(sum){
  print(paste0('Outcome: ', sum$outcome))
  print(sum$res)
}

summary.regImp <- function(result){
  out <- matrix(nrow=1,ncol=3)
  rownames(out) <- 'OOS R-squared'
  colnames(out) <- c('Full Model','Reduced Model','Difference')
  r2Full <- pool.r.squared2(result$pooledFull)
  r2Red <- pool.r.squared2(result$pooledRed)
  out[1,'Full Model'] <- r2Full[,'est']
  out[1,'Reduced Model'] <- r2Red[,'est']
  if (is.na(r2Red[,'est'])){
    out[1,'Difference'] <- r2Full[,'est']
  }
  outcome<-result$pooledFull$analyses[[1]]$outcome
  if (!(is.null(result$pooledFull$analyses[[1]]$importance))){
    importanceMatrix <- matrix(NA,nrow=nrow(result$pooledFull$analyses[[1]]$importance),ncol=length(result$pooledFull$analyses))
    rownames(importanceMatrix) <- rownames(result$pooledFull$analyses[[1]]$importance)
    for (i in 1:length(result$pooledFull$analyses)){
      stopifnot(identical(rownames(importanceMatrix),rownames(result$pooledFull$analyses[[i]]$importance)))
      if (class(result$pooledFull$analyses[[i]]$importance)=="dgCMatrix"){#LASSO coeffcients
        importanceMatrix[,i] <- as.vector(result$pooledFull$analyses[[i]]$importance)
      } else{ #random forest variable importance
        importanceMatrix[,i] <- result$pooledFull$analyses[[i]]$importance[,'%IncMSE']
      }
    }
    pooledImportance <- rowMeans(importanceMatrix)
    pooledImportance <- sort(pooledImportance,decreasing = TRUE)
  }else{
    pooledImportance <- NULL
  }
  realOut <- list(outcome=outcome,res=out,importance=pooledImportance)
  class(realOut) <- 'glmSum'
  return(realOut)
}



