
source('~/mystuff/projects/daytoday/code/global.R')
library(lme4)

#generata data
N <- 200
nTime <- 50
subject <- rep(1:N,nTime)
height <- rep(rnorm(N),nTime)
cortisol <- rnorm(N*nTime)
cognition <- rep(rnorm(N),nTime)+1*cortisol+rnorm(N*nTime)+height*3
df <- data.frame(subject,height,cortisol,cognition)

#only random subject
lmBase<-lmer(cognition~(1|subject),data=df)
tmp <- getReliability(lmBase)
tmp2 <- getReliability2(lmBase)
print(paste0(tmp,' vs. ', tmp2))

#added height trait
lmHeight<-lmer(cognition~(1|subject)+height,data=df)
tmp <- getReliability(lmHeight)
tmp2 <- getReliability2(lmHeight)
print(paste0(tmp,' vs. ', tmp2))
# print(anova(lmHeight,lmBase))

#added cortisol state
lmCortisol<-lmer(cognition~(1|subject)+cortisol,data=df)
tmp <- getReliability(lmCortisol)
tmp2 <- getReliability2(lmCortisol)
print(paste0(tmp,' vs. ', tmp2))

#only reliability1 works as expected for all models
#difference between 1 and 2 is in the 4th decimal place
tmp <- getReliability(lmBase)
tmp2 <- getReliability2(lmBase)
print(tmp-tmp2)

print(getReliability(lmCortisol)-getReliability(lmBase))