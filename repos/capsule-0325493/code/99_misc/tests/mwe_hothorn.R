library(MASS)
library(R.utils)

#settings
N <- 253 
p <- 38
ntree<-50 #is higher in the original analysis but effect stays the same

#generate noise data
x <- mvrnorm(n=N,mu=rep(0,p),diag(rep(1,p)))
y <- rnorm(n=N)
myData <- data.frame(y,x)

#party analysis
if (isPackageLoaded('partykit')){
  detach('package:partykit',unload=TRUE)        
}
library(party)
mycontrols <- cforest_unbiased(trace=TRUE,ntree=ntree,minsplit = 0,minbucket = 0)
party <- c()
party$model <- cforest(y ~ .,controls=mycontrols,data=myData)
party$y_hat <- predict(party$model, OOB=TRUE)
par(mfrow=c(1,3))
plot(y,party$y_hat) #looks reasonable to me. high bias towards 0, no correlation 
c1 <- cor(y,party$y_hat)
title(paste0('party, r=',round(c1, digits = 2)))

#partykit analysis
if (isPackageLoaded('party')){
  detach('package:party',unload=TRUE)        
}
library(partykit)
partykit <- c()
partykit$model <- cforest(y ~ ., data=myData,trace=TRUE,ntree=ntree,cores=7,control = ctree_control(teststat = "quad",
                                                                                                                 testtype = "Univ", mincriterion = 0, minsplit = 0,minbucket = 0))
partykit$y_hat <- predict(partykit$model, OOB=TRUE)
plot(y,partykit$y_hat) #does not look reansonable
c2 <- cor(y,partykit$y_hat)
title(paste0('partykit, r=',round(c2, digits = 2)))

#maybe OOB argument is just ignored?
partykitNoOOB <- predict(partykit$model, OOB=FALSE)
plot(y,partykitNoOOB) 
c3 <- cor(y,partykitNoOOB)
title(paste0('partykit, OOB=FALSE, r=',round(c3, digits = 2))) 