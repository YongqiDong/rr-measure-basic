source(file.path(projDir,'code/global.R'))
require(lme4)

between <- 0.95
within <- (1-between)
trueICC <-between/(within+between) #0.95
N <- 6
p <- 50
reps <- 100
rel <- rep(NA,reps)
betweenEst <- rel
withinEst <- rel
for (i in 1:reps){
  #between structure
  subjectMeans <- rnorm(n=N,sd=sqrt(between))
  subjectMeans <- rep(subjectMeans,each=p)
  
  #within structure
  noise <- rnorm(N*p,sd=sqrt(within))
  
  #join 
  measure <- subjectMeans+noise
  dataSet <- data.frame(Y=measure,subject=as.factor(rep(1:N,each=p)))
  
  #our model
  myModel1 <- lmer(Y ~ (1|subject),data=dataSet)
  relRes <- getReliability(myModel1)
  rel[i] <- relRes[1]
  withinEst[i] <- relRes[2]
  betweenEst[i] <- relRes[3]-relRes[2]
}
#biased
cat(sprintf('Reliability: True Value %.2f Mean of Estimate %.2f\n',between/(between+within),mean(rel)))
#biased
cat(sprintf('Between Variance: True Value %.2f Mean of Estimate %.2f\n',between,mean(betweenEst)))
#unbiased
cat(sprintf('Between Variance: True Value %.2f Mean of Estimate %.2f\n',within,mean(withinEst[i])))
#sanity check
stopifnot(identical(mean((betweenEst)/(betweenEst+withinEst)),mean(rel)))