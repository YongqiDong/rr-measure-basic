imp <- mice(nhanes)
fit <- with(imp,lm(age~bmi + hyp + chl))
res <- pool(fit)
coefs <- matrix(nrow =imp$m,ncol=4)
covMatrix <- array(dim=c(imp$m,4,4))
for (i in 1:imp$m){
  lmRes <- lm(age~bmi + hyp + chl,data=complete(imp,i))
  coefs[i,] <- coefficients(lmRes)
  covMatrix[i,,] <- vcov(lmRes)
}
miceSum <- summary(res)
print(miceSum)

#recreate using simple R
myRes <- matrix(nrow=4,ncol=2)
rownames(myRes) <- rownames(miceSum)
colnames(myRes) <- colnames(miceSum)[1:2]

myRes[,'est'] <- colMeans(coefs) 
UStrich <- apply(covMatrix,c(2, 3), mean)
B <- cov(coefs)
T <- UStrich + (1+1/imp$m)*B
myRes[,'se'] <- sqrt(diag(T))
class(fit[[1]])
res <- pool(fit)
stopifnot(all.equal(myRes,miceSum[,1:2]))