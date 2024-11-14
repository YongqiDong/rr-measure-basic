require(glmnet)
require(ggplot2)
summary.glmnet.cmp <- function(model,...){
  coefs <- coef(model$lm) 
  outcome <- model$outcome
  result<- list(coefs=coefs,outcome=outcome,)
  class(result)<- "summary.glmnet.cmp"
  return(result)
}

getBestCV <- function(model){
  stopifnot(identical(class(model),'cv.glmnet'))
                      selector <- model$lambda.min==model$lambda
                      result <- matrix(nrow=1,ncol=2,dimnames=list(c('row1'),c('up','low')))
                      result[1,'up'] <- model$cvup[selector]
                      result[1,'low'] <- model$cvlo[selector]
  return(result)
}

plot.glmnet.cmp <- function(model,...){
  #plot(coefs(model$lm),main=model$outcome)
  limits <- rbind(getBestCV(model$lm),getBestCV(model$lmbase))
  df <- data.frame(model=c('full','reduced'),up=limits[,'up'],low=limits[,'low'])
  cvCmp <- ggplot(df,aes(x=model,ymax=up,ymin=low)) + geom_errorbar() + ggtitle(model$outcome)
  print(cvCmp)
}


