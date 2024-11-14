#frau 1, mann 0
dataSet <- data.frame(matrix(nrow=200,ncol=4))
dataSet$gender <- c(rep(0,100),rep(1,100))
dataSet$hormon <- c(rep(0,100),rnorm(100))
dataSet$outcome <- rep(NA,200)
dataSet$outcome[1:100] <- 5+10*rnorm(100)
dataSet$outcome[101:200] <- 100+3*dataSet$hormon[101:200]+10*rnorm(100)
blub <- lm(outcome ~ 1+gender+gender*hormon-hormon,data=dataSet)
print(summary(blub))