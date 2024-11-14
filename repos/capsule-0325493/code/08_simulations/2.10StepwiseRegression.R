source('~/mystuff/projects/daytoday/code/global.R',local=TRUE)
require(lme4)
#BASE ICC
baseICC <- 0.95 #change this to 0.99 for even higher power
#Explain
explain <- 0.005
reps <- 1000
N <- 6
p <- 50
pVal <- rep(NA,reps)
rSquareDiff <- pVal
ICC <- matrix(NA,nrow = reps,ncol=3)
colnames(ICC) <- c('ICC','true','err')
for (altTrue in c(FALSE,TRUE)){
  if (altTrue){
    cexplain <- explain
  }else{
    cexplain <- 0
  }
  for (i in 1:reps){
    ##create data accoring to the model
    
    #subjectMeans
    subjectMeans <- rnorm(n=N,sd=sqrt(0.95))
    subjectMeans <- rep(subjectMeans,each=p)
    
    #add predictor
    predictor <- rnorm(N*p)
    predicted <- sqrt(cexplain)*predictor
    
    #add noise
    noise <- rnorm(N*p,sd=sqrt(1-(baseICC+explain)))
    
    #gather everything
    measure <- subjectMeans+predicted+noise
    dataSet <- data.frame(Y=measure,pred=predictor,subject=as.factor(rep(1:N,each=p)))
    
    #within subject correlation
    baseModel <- lm(Y ~ subject,data=dataSet)
    fullModel <- lm(Y ~ subject+predictor,data=dataSet)
    tmp <- anova(baseModel,fullModel)
    pVal[i] <- tmp$`Pr(>F)`[2]
    rSquareDiff[i] <- summary(fullModel)$adj.r.squared-summary(baseModel)$adj.r.squared
  }
  if(altTrue){
    cat(sprintf('Power: %.4f\n',sum(pVal<0.05)/reps))
    cat(sprintf('Variance Explained: True: %.4f vs Estimated: %.4f\n',explain,mean(rSquareDiff)))  
  }else{
    cat(sprintf('Type I Error: %.4f\n',sum(pVal<0.05)/reps))
  }
} 


