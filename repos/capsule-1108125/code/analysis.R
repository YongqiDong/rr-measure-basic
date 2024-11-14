#All analysis can be reproduced in order of appearance in the main text, closing with analysis from the supplementary material. 
#All figures are at the end of the script.

#Load all four data tables

exp1 <- read.table('../data/exp1.txt',"\t", header=T)
exp2 <- read.table('../data/exp2.txt',"\t", header=T)
payoffs <- read.table('../data/payoffs.txt',"\t", header=T)
villages <- read.table('../data/villages.txt',"\t", header=T)

###Analysis of payoff data###

#Descriptive stats of payoffs in USD (22,800 Dong = 1 USD)

fileConn<-file("../results/other_results00.txt")

writeLines(c("Payoffs exp1 (mean, sd) and exp2 (mean, sd) in USD w/o show-up fee",

mean(payoffs$Earning[payoffs$Experiment==1]-50)/22.8,
sd(payoffs$Earning[payoffs$Experiment==1]-50)/22.8,

mean(payoffs$Earning[payoffs$Experiment==2]-50)/22.8,
sd(payoffs$Earning[payoffs$Experiment==2]-50)/22.8),fileConn)

close(fileConn)

###Analysis of Experiment 1###

#Exclusion of subjects who cross-talked or -listened: 39,91,109

exp1_red<-exp1[!(
  exp1$ID=="2_91"|
  exp1$ID=="2_109"|
  exp1$ID=="2_39"
),]

#Stats on survey question on reasons for motivation to exert effort

fileConn<-file("../results/other_results01.txt")

writeLines(c("Survey question on reason for effort motivation",

mean(exp1_red$reason1,na.rm=T),
mean(exp1_red$reason2,na.rm=T),
mean(exp1_red$reason3,na.rm=T),
mean(exp1_red$reason4,na.rm=T)), fileConn)

close(fileConn)

#Define variable that contains number of produced bags

exp1_red$bagsmade<-exp1_red$bags-5

#Descriptive stats overall

fileConn<-file("../results/other_results02.txt")

writeLines(c("Performance statistics overall: mean, median, sd, min, max",

mean(exp1_red$bagsmade),
median(exp1_red$bagsmade),
sd(exp1_red$bagsmade),
min(exp1_red$bagsmade),
max(exp1_red$bagsmade)), fileConn)

close(fileConn)

#Descriptive stats by village

sink("../results/other_results03.txt")

print("Performance statistics by village")

print("mean")
print(round(tapply(exp1_red$bagsmade,exp1_red$village,mean),2))

print("sd")
print(round(tapply(exp1_red$bagsmade,exp1_red$village,sd),2))

print("median")
print(tapply(exp1_red$bagsmade,exp1_red$village,median))

sink()

#ANOVA on treatment with village

sink("../results/other_results04.txt")

print("ANOVA on effort by village and treatment")

print(summary(aov(exp1_red$bagsmade~as.factor(exp1_red$village)+as.factor(exp1_red$treat))))

sink()

#Mixed-effect models

#Prepare independent variables for models

exp1_red$meritin <- ifelse(exp1_red$treat==1,1,0)
exp1_red$meritout <- ifelse(exp1_red$treat==2,1,0)
exp1_red$fixedequal <- ifelse(exp1_red$treat==3,1,0)
exp1_red$fixedrandom <- ifelse(exp1_red$treat==4,1,0)

exp1_red$forestgroup <- as.numeric(as.character(exp1_red$forestgroup))
exp1_red$gender <- as.numeric(as.character(exp1_red$gender))
exp1_red$income <- as.numeric(as.character(exp1_red$income))/22.8
exp1_red$age <- as.numeric(as.character(exp1_red$age))
exp1_red$education <- as.numeric(as.character(exp1_red$education))
exp1_red$children <- as.numeric(as.character(exp1_red$children))
exp1_red$experience <- as.numeric(as.character(exp1_red$experience))
exp1_red$snacks <- as.numeric(as.character(exp1_red$snacks))
exp1_red$estimation <- as.numeric(as.character(exp1_red$estimation))
exp1_red$expectation <-ifelse((exp1_red$treat==1|exp1_red$treat==2)&exp1_red$estimation>1250,1,
                ifelse((exp1_red$treat==3|exp1_red$treat==4)&exp1_red$estimation>50000,1,0))

#Principal component analysis for independent variable of wealth

exp1_red$material<-as.data.frame(cbind(exp1_red$motobike,exp1_red$bicycle,exp1_red$gasstove,
                              exp1_red$cellphone,exp1_red$TV,exp1_red$fridge))

head<-c("motobike","bicycle","gasstove","cellphone","TV","fridge")
names(exp1_red$material)<-head

library(psych)

mPCA<-principal(exp1_red$material,rotate="varimax",nfactors=2,scores=T)

freq <- round(sapply(exp1_red$material,sum)/nrow(exp1_red),2)
PCAoutput <- as.data.frame(round(cbind(freq,mPCA$loadings[,1],mPCA$loadings[,2]),3))
names(PCAoutput) <- c("frequency","PC1","PC2")
PCAtable <- rbind(PCAoutput,c("",round(as.vector(mPCA$Vaccounted[2,]),3)))
rownames(PCAtable)[7] <- "prop. variance"

library(stargazer)

sink("../results/TableS2.txt")
stargazer(PCAtable,type="text",summary=F,rownames=T)
sink()

fileConn<-file("../results/TableS2.txt")

exp1_red$wealtha<-mPCA$scores[,1]

#Correlations of independent variables

ind_vars <- data.frame(cbind(exp1_red$forestgroup,exp1_red$gender,exp1_red$income,
                  exp1_red$age,exp1_red$education,exp1_red$children,
                  exp1_red$experience,exp1_red$snacks,exp1_red$expectation,exp1_red$wealtha))

names(ind_vars) <- c("forestgroup","gender","income","age","education",
                     "children","experience","snacks","expectation","wealtha")
                     
sink("../results/other_results05.txt")

print("Correlations of independent variables")

print(round(cor(ind_vars,use="pairwise.complete.obs"),2))

sink()

#Summary table

su_table <- stargazer(ind_vars,type="text",digits=2)

fileConn<-file("../results/Table2.txt")

writeLines(su_table, fileConn)

close(fileConn)

#Models 1-2

library(nlme)

Mod1<-lme(bagsmade
             ~meritin
             +meritout
             +fixedequal
          ,random=~1|village,
          data=exp1_red,
          na.action=na.omit)

summary(Mod1)

Mod2<-lme(bagsmade
            ~meritin
             +meritout
             +fixedequal
             +forestgroup
             +gender
             +income
             +age
             +education
             +children
             +wealtha
             +experience
             +snacks
             +expectation
          ,random=~1|village,
          data=exp1_red,
          na.action=na.omit)

summary(Mod2)

#Model table

mo_table <- stargazer(Mod1,Mod2,type="text",single.row=T,omit.stat="BIC")

fileConn<-file("../results/Table3.txt")

writeLines(mo_table, fileConn)

close(fileConn)

#Model 2 with switched reference category

Mod2_switch<-lme(bagsmade
          ~meritin
          +meritout
          +fixedrandom
          +forestgroup
          +gender
          +income
          +age
          +education
          +children
          +wealtha
          +experience
          +snacks
          +expectation
          ,random=~1|village,
          data=exp1_red,
          na.action=na.omit)
          

sink("../results/other_results06.txt")

print("Model 2 with fixedequal as reference treatment")

print(summary(Mod2_switch))

sink()

#Model 2 robustness check 1: include a priori removed subjects

exp1$bagsmade<-exp1$bags-5

exp1$meritin <- ifelse(exp1$treat==1,1,0)
exp1$meritout <- ifelse(exp1$treat==2,1,0)
exp1$fixedequal <- ifelse(exp1$treat==3,1,0)
exp1$fixedrandom <- ifelse(exp1$treat==4,1,0)

exp1$forestgroup <- as.numeric(as.character(exp1$forestgroup))
exp1$gender <- as.numeric(as.character(exp1$gender))
exp1$income <- as.numeric(as.character(exp1$income))/22.8
exp1$age <- as.numeric(as.character(exp1$age))
exp1$education <- as.numeric(as.character(exp1$education))
exp1$children <- as.numeric(as.character(exp1$children))
exp1$experience <- as.numeric(as.character(exp1$experience))
exp1$snacks <- as.numeric(as.character(exp1$snacks))
exp1$estimation <- as.numeric(as.character(exp1$estimation))
exp1$expectation <-ifelse((exp1$treat==1|exp1$treat==2)&exp1$estimation>1250,1,
                     ifelse((exp1$treat==3|exp1$treat==4)&exp1$estimation>50000,1,0))

exp1$material<-as.data.frame(cbind(exp1$motobike,exp1$bicycle,exp1$gasstove,
                              exp1$cellphone,exp1$TV,exp1$fridge))

head<-c("motobike","bicycle","gasstove","cellphone","TV","fridge")
names(exp1$material)<-head

mPCA<-principal(exp1$material,rotate="varimax",nfactors=2,scores=T)
exp1$wealtha<-mPCA$scores[,1]

Mod2_robust1<-lme(bagsmade
          ~meritin
          +meritout
          +fixedequal
          +forestgroup
          +gender
          +income
          +age
          +education
          +children
          +wealtha
          +experience
          +snacks
          +expectation
          ,random=~1|village,
          data=exp1,
          na.action=na.omit)

sink("../results/other_results07.txt")

print("Model 2 robustness check 1: include a priori removed subjects")

print(summary(Mod2_robust1))

sink()

#Model 2 robustness check 2: exclude outliers > 1.5 interquantile range

exp_red1_outlier <- exp1_red[!exp1_red$ID %in% exp1_red$ID[boxplot.stats(exp1_red$bagsmade)$out],]
nrow(exp_red1_outlier)

Mod2_robust2<-lme(bagsmade
                  ~meritin
                  +meritout
                  +fixedequal
                  +forestgroup
                  +gender
                  +income
                  +age
                  +education
                  +children
                  +wealtha
                  +experience
                  +snacks
                  +expectation
                  ,random=~1|village,
                  data=exp_red1_outlier,
                  na.action=na.omit)

sink("../results/other_results08.txt")

print("Model 2 robustness check 2: exclude outliers > 1.5 interquantile range")

print(summary(Mod2_robust2))

sink()

###Analysis of Experiment 2###

#Rate of reponse matching

match1<-vector(length=nrow(exp2))
match2<-vector(length=nrow(exp2))
match3<-vector(length=nrow(exp2))
match4<-vector(length=nrow(exp2))

getmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

for(i in 1:nrow(exp2)) {
  match1[i]<-ifelse(exp2$decT1[i] %in% getmode(exp2$decT1[exp2$village==exp2$village[i]]),1,0)
  match2[i]<-ifelse(exp2$decT2[i] %in% getmode(exp2$decT2[exp2$village==exp2$village[i]]),1,0)
  match3[i]<-ifelse(exp2$decT3[i] %in% getmode(exp2$decT3[exp2$village==exp2$village[i]]),1,0)
  match4[i]<-ifelse(exp2$decT4[i] %in% getmode(exp2$decT4[exp2$village==exp2$village[i]]),1,0)
}

fileConn<-file("../results/other_results09.txt")

writeLines(c("Rate of matching modal responses",

sum(c(match1,match2,match3,match4))/(4*nrow(exp2))
 ), fileConn)
 
close(fileConn)

#Simulation: which rate response matching to expect for groups of 10 and random dedcision process (code by Carl Salk)

n<-10
nrep<-100000
outvec<-rep(NA,nrep)
for(i in 1:nrep){
  resvec<-summary(as.factor(sample(c('A','B','C','D'),n,replace=T)))
  outvec[i]<-sum(resvec[resvec==max(resvec)])
  }
  
fileConn<-file("../results/other_results10.txt")

writeLines(c("Expected rate of matching modal responses for random process",

mean(outvec)/n), fileConn)

 close(fileConn)

#Rate of unique modes

a <- 0
for(i in 1:8){
  if(length(getmode(exp2$decT1[exp2$village==i]))==1)
  {a <- a+1}
  if(length(getmode(exp2$decT2[exp2$village==i]))==1)
  {a <- a+1}
  if(length(getmode(exp2$decT3[exp2$village==i]))==1)
  {a <- a+1}
  if(length(getmode(exp2$decT4[exp2$village==i]))==1)
  {a <- a+1}
}

fileConn<-file("../results/other_results11.txt")

writeLines(c("Rate of unique modal responses",
a/32
 ), fileConn)
 
close(fileConn)

#Comparing responses by village

kruskal.test(decT1 ~ as.factor(village), data = exp2)
kruskal.test(decT2 ~ as.factor(village), data = exp2) 
kruskal.test(decT3 ~ as.factor(village), data = exp2) 
kruskal.test(decT4 ~ as.factor(village), data = exp2) 

#Equity scores by village and treatment and summary statistics

eqT1<-tapply((ifelse(exp2$decT1=="1",1,
                        ifelse(exp2$decT1=="2",1/3,
                               ifelse(exp2$decT1=="3",-1/3,-1)))),exp2$village,mean)
eqT2<-tapply((ifelse(exp2$decT2=="1",1,
                        ifelse(exp2$decT2=="2",1/3,
                               ifelse(exp2$decT2=="3",-1/3,-1)))),exp2$village,mean)
eqT3<-tapply((ifelse(exp2$decT3=="1",1,
                        ifelse(exp2$decT3=="2",1/3,
                               ifelse(exp2$decT3=="3",-1/3,-1)))),exp2$village,mean)
eqT4<-tapply((ifelse(exp2$decT4=="1",1,
                        ifelse(exp2$decT4=="2",1/3,
                               ifelse(exp2$decT4=="3",-1/3,-1)))),exp2$village,mean)

eqT<-rbind(cbind(eqT1,eqT2,eqT3,eqT4),mean=c(mean(eqT1),mean(eqT2),mean(eqT3),mean(eqT4)),median=c(median(eqT1),
 median(eqT2),median(eqT3),median(eqT4)))

sink("../results/other_results12.txt")

print("Equity scores for all treatments and villages")

print(eqT)

sink()

#Comparison of equity scores between villages

sink("../results/other_results13.txt")

print("Statistical comparison of equity ratings between villages for each treatment")

kruskal.test(exp2$decT1 ~ exp2$village)
kruskal.test(exp2$decT2 ~ exp2$village)
kruskal.test(exp2$decT3 ~ exp2$village)
kruskal.test(exp2$decT4 ~ exp2$village)
             
sink()

#Comparison of equity scores between treatments, overall and paired

sink("../results/other_results14.txt")

print("Statistical comparison of equity scores between treatments overall (kruskal) and pairwise (wilcox)")

kruskal.test(as.numeric(as.character(c(eqT1,eqT2,eqT3,eqT4)))
             ~ as.factor(c(rep("eqT1",8),rep("eqT2",8),rep("eqT3",8),rep("eqT4",8))))

wilcox.test(as.numeric(as.character(c(eqT1))), as.numeric(as.character(c(eqT2))))
wilcox.test(as.numeric(as.character(c(eqT1))), as.numeric(as.character(c(eqT3))))
wilcox.test(as.numeric(as.character(c(eqT1))), as.numeric(as.character(c(eqT4))))
wilcox.test(as.numeric(as.character(c(eqT2))), as.numeric(as.character(c(eqT3))))
wilcox.test(as.numeric(as.character(c(eqT2))), as.numeric(as.character(c(eqT4))))
wilcox.test(as.numeric(as.character(c(eqT3))), as.numeric(as.character(c(eqT4))))

sink()

###Joint analysis of Experiment 1 and 2###

#Define equity scores for individuals in Experiment 1 (equity score of their treatment, rated by their village)

equity_indiv<-vector(length=173)

for(i in 1:173) {
  equity_indiv[i] <-eqT[exp1_red$village[i],exp1_red$treat[i]]
}

#Define adjusted performance for individuals in Experiment 1 (distance to village mean)

villmeans<-as.data.frame(tapply(exp1_red$bagsmade,exp1_red$village,mean))
villmeans_tab<-cbind(seq(1,8),villmeans)
names(villmeans_tab)<-c("vill_number","vill_mean")

exp1_red$bagsmadedeviate<-rep(NA, 173)

for (i in seq (1:173)){
  exp1_red$bagsmadedeviate[i]<-exp1_red$bagsmade[i]-villmeans_tab$vill_mean[villmeans_tab$vill_number==exp1_red$village[i]]
}

#Correlations w and w/o adjustment

sink("../results/other_results15.txt")

print("Correlations of performance and local equity score with and without village adjustment")

cor.test(exp1_red$bagsmade,equity_indiv,method="pearson") #r = 0.14, p =0.076

cor.test(exp1_red$bagsmadedeviate,equity_indiv,method="pearson") #r = 0.19, p =0.010

sink()

#Mixed-effect model as alternative analysis

Mod_cor <- lme(bagsmade~equity_indiv,random=~1|village,data=exp1_red,na.action=na.omit)

sink("../results/other_results16.txt")

print("Alternative analysis of performance by local equity score with mixed-effect model")

print(summary(Mod_cor))

sink()

###Supp. Material: Testing village-level correlates of performance###

vi_table <- stargazer(villages,summary=F, rownames=F, type="text")
fileConn<-file("../results/TableS1.txt")
writeLines(vi_table, fileConn)
close(fileConn)

sink("../results/other_results17.txt")

print("Correlations of village-level performance with village charactersitics")

cor.test(villages$shadow,villages$meanperformance, method="spearman")
cor.test(villages$mobilechairs ,villages$meanperformance, method="spearman")
cor.test(villages$afterlunch,villages$meanperformance, method="spearman")
cor.test(villages$sharewomen1,villages$meanperformance, method="spearman")
cor.test(villages$snackminutes,villages$meanperformance, method="spearman")
cor.test(villages$povertyrate,villages$meanperformance, method="spearman")
cor.test(villages$households,villages$meanperformance, method="spearman")
cor.test(villages$avgincome,villages$meanperformance, method="spearman")
cor.test(villages$distancecommune,villages$meanperformance, method="spearman")

sink()

###Plotting Figures###

#Figure 4 (Performance in Experiment 1)

sem<-function(x){sd(x)/sqrt(length(x))}

png(file="../results/Figure4.png",width=1500,height=900,res=150)

par(cex.main = 1.5, mar = c(6, 7, 2, 3) + 0.1, mgp = c(4.5, 1.2, 0), 
    cex.lab = 2.25, cex.axis = 1.75, las = 1)

x<-c(1,4)
y<-c(0,90)

plot(x,y,xaxt="n",type="n",xlim=c(0.5,4.5),ylim=c(0,82),ylab= "Conservation effort",xlab="Treatment",xaxs="i")
axis(1,at=c(1,2,3,4),label=c(expression(italic("Merit input")),
                             expression(italic("Merit output")),
                             expression(italic("Fixed equal")),
                             expression(italic("Fixed individual"))
))

x1<-rep(1,length(exp1_red$bagsmade[exp1_red$treat==1]))
x2<-rep(2,length(exp1_red$bagsmade[exp1_red$treat==2]))
x3<-rep(3,length(exp1_red$bagsmade[exp1_red$treat==3]))
x4<-rep(4,length(exp1_red$bagsmade[exp1_red$treat==4]))

points(jitter(x1,factor=0,amount=0.04),exp1_red$bagsmade[exp1_red$treat==1],cex=2.2)
points(jitter(x2,factor=0,amount=0.04),exp1_red$bagsmade[exp1_red$treat==2],cex=2.2)
points(jitter(x3,factor=0,amount=0.04),exp1_red$bagsmade[exp1_red$treat==3],cex=2.2)
points(jitter(x4,factor=0,amount=0.04),exp1_red$bagsmade[exp1_red$treat==4],cex=2.2)

segments(x1-0.27,mean(exp1_red$bagsmade[exp1_red$treat==1]),x1+0.27,mean(exp1_red$bagsmade[exp1_red$treat==1]),lwd=3)
segments(x2-0.27,mean(exp1_red$bagsmade[exp1_red$treat==2]),x2+0.27,mean(exp1_red$bagsmade[exp1_red$treat==2]),lwd=3)
segments(x3-0.27,mean(exp1_red$bagsmade[exp1_red$treat==3]),x3+0.27,mean(exp1_red$bagsmade[exp1_red$treat==3]),lwd=3)
segments(x4-0.27,mean(exp1_red$bagsmade[exp1_red$treat==4]),x4+0.27,mean(exp1_red$bagsmade[exp1_red$treat==4]),lwd=3)

segments(x1,mean(exp1_red$bagsmade[exp1_red$treat==1])-sem(exp1_red$bagsmade[exp1_red$treat==1]),x1,mean(exp1_red$bagsmade[exp1_red$treat==1])+sem(exp1_red$bagsmade[exp1_red$treat==1]),lwd=3)
segments(x2,mean(exp1_red$bagsmade[exp1_red$treat==2])-sem(exp1_red$bagsmade[exp1_red$treat==2]),x2,mean(exp1_red$bagsmade[exp1_red$treat==2])+sem(exp1_red$bagsmade[exp1_red$treat==2]),lwd=3)
segments(x3,mean(exp1_red$bagsmade[exp1_red$treat==3])-sem(exp1_red$bagsmade[exp1_red$treat==3]),x3,mean(exp1_red$bagsmade[exp1_red$treat==3])+sem(exp1_red$bagsmade[exp1_red$treat==3]),lwd=3)
segments(x4,mean(exp1_red$bagsmade[exp1_red$treat==4])-sem(exp1_red$bagsmade[exp1_red$treat==4]),x4,mean(exp1_red$bagsmade[exp1_red$treat==4])+sem(exp1_red$bagsmade[exp1_red$treat==4]),lwd=3)

segments(x1-0.15,mean(exp1_red$bagsmade[exp1_red$treat==1])+sem(exp1_red$bagsmade[exp1_red$treat==1]),x1+0.15,mean(exp1_red$bagsmade[exp1_red$treat==1])+sem(exp1_red$bagsmade[exp1_red$treat==1]),lwd=3)
segments(x2-0.15,mean(exp1_red$bagsmade[exp1_red$treat==2])+sem(exp1_red$bagsmade[exp1_red$treat==2]),x2+0.15,mean(exp1_red$bagsmade[exp1_red$treat==2])+sem(exp1_red$bagsmade[exp1_red$treat==2]),lwd=3)
segments(x3-0.15,mean(exp1_red$bagsmade[exp1_red$treat==3])+sem(exp1_red$bagsmade[exp1_red$treat==3]),x3+0.15,mean(exp1_red$bagsmade[exp1_red$treat==3])+sem(exp1_red$bagsmade[exp1_red$treat==3]),lwd=3)
segments(x4-0.15,mean(exp1_red$bagsmade[exp1_red$treat==4])+sem(exp1_red$bagsmade[exp1_red$treat==4]),x4+0.15,mean(exp1_red$bagsmade[exp1_red$treat==4])+sem(exp1_red$bagsmade[exp1_red$treat==4]),lwd=3)

segments(x1-0.15,mean(exp1_red$bagsmade[exp1_red$treat==1])-sem(exp1_red$bagsmade[exp1_red$treat==1]),x1+0.15,mean(exp1_red$bagsmade[exp1_red$treat==1])-sem(exp1_red$bagsmade[exp1_red$treat==1]),lwd=3)
segments(x2-0.15,mean(exp1_red$bagsmade[exp1_red$treat==2])-sem(exp1_red$bagsmade[exp1_red$treat==2]),x2+0.15,mean(exp1_red$bagsmade[exp1_red$treat==2])-sem(exp1_red$bagsmade[exp1_red$treat==2]),lwd=3)
segments(x3-0.15,mean(exp1_red$bagsmade[exp1_red$treat==3])-sem(exp1_red$bagsmade[exp1_red$treat==3]),x3+0.15,mean(exp1_red$bagsmade[exp1_red$treat==3])-sem(exp1_red$bagsmade[exp1_red$treat==3]),lwd=3)
segments(x4-0.15,mean(exp1_red$bagsmade[exp1_red$treat==4])-sem(exp1_red$bagsmade[exp1_red$treat==4]),x4+0.15,mean(exp1_red$bagsmade[exp1_red$treat==4])-sem(exp1_red$bagsmade[exp1_red$treat==4]),lwd=3)

dev.off()

#Figure 5 (Equity scores in Experiment 2)

png(file="../results/Figure5.png",width=1500,height=900,res=150)

par(cex.main = 1.5, mar = c(6, 7, 2, 3) + 0.1, mgp = c(4.5, 1.2, 0), 
    cex.lab = 2.25, cex.axis = 1.75, las = 1)

x<-c(1,4)
y<-c(-0.5,1)

x1<-rep(1,8)
x2<-rep(2,8)
x3<-rep(3,8)
x4<-rep(4,8)

plot(x,y,type="n",xaxt="n",xlab="Treatmemt",ylab="Equity score",ylim=c(-0.5,1),xlim=c(0.5,4.5),xaxs="i")
axis(1,at=c(1,2,3,4),label=c(expression(italic("Merit input")),
                             expression(italic("Merit output")),
                             expression(italic("Fixed equal")),
                             expression(italic("Fixed individual"))))

points(jitter(x1,factor=0,amount=0.025),eqT1,cex=2.2)
points(jitter(x2,factor=0,amount=0.025),eqT2,cex=2.2)
points(jitter(x3,factor=0,amount=0.025),eqT3,cex=2.2)
points(jitter(x4,factor=0,amount=0.025),eqT4,cex=2.2)

segments(x1-0.22,median(eqT1),x1+0.22,median(eqT1),lwd=3)
segments(x2-0.22,median(eqT2),x2+0.22,median(eqT2),lwd=3)
segments(x3-0.22,median(eqT3),x3+0.22,median(eqT3),lwd=3)
segments(x4-0.22,median(eqT4),x4+0.22,median(eqT4),lwd=3)

dev.off()

#Figure 6 (Correlation across Experiment 1 and 2)

library(plotrix)

png(file="../results/Figure6.png",width=1100,height=900,res=150)

par(cex.main = 1.5, mar = c(6, 6.5, 3.5, 3) + 0.1, mgp = c(4, 1, 0), 
    cex.lab = 2.2, cex.axis = 1.8, las = 1)

plot(equity_indiv, exp1_red$bagsmadedeviate,
     xlim = c(-0.4,1), ylim = c(-35,40), xaxt="no",
     xlab="Equity score",ylab="Conservation effort (adj.)",
     type="n")

points(equity_indiv[exp1_red$treat==1],exp1_red$bagsmadedeviate[exp1_red$treat==1],pch=21,
       cex=2.2,bg="tan1")
points(equity_indiv[exp1_red$treat==2],exp1_red$bagsmadedeviate[exp1_red$treat==2],pch=21,
       cex=2.2,bg="salmon2")
points(equity_indiv[exp1_red$treat==3],exp1_red$bagsmadedeviate[exp1_red$treat==3],pch=21,
       cex=2.2,bg="darkseagreen2")
points(equity_indiv[exp1_red$treat==4],exp1_red$bagsmadedeviate[exp1_red$treat==4],pch=21,
       cex=2.2,bg="steelblue1")

legend("topleft",c("Merit input","Merit output","Fixed equal","Fixed individual"),
       cex=1.4,pch=21,bty="n",pt.bg=c("tan1","salmon2","darkseagreen2","steelblue1"),ncol=2)

axis(1, at=c(-0.5,-0.25,0,0.25,0.5,0.75,1))

reg1 <- lm(exp1_red$bagsmadedeviate ~ equity_indiv)
ablineclip(reg1, lwd = 3,x1 = -0.4, x2 = 1) 

dev.off()

#Figure S1 (Performance distribution)

png(file="../results/FigureS1.png",width=850,height=700,res=130)

par(cex.main = 1.5, mar = c(5, 6, 2.5, 2.5) + 0.1, mgp = c(2.5, 1, 0), cex.lab = 2, 
    cex.axis = 1.25, bty = "n", las = 1)
hist(exp1_red$bagsmade, main = "", xlab = "", ylab = " ", 
     ylim = c(0, 40), xlim = c(0, 85), axes = FALSE, col = "white",breaks=20)
axis(1, seq(0, 85, by = 10))
axis(2, seq(0,60, by = 10))
rug(jitter(exp1_red$bagsmade))
mtext("Conservation effort", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Frequency", side = 2, line = 3, cex = 1.5, font = 2, las = 0)
dev.off()

#Figure S2 (Performance overall by village)

png(file="../results/FigureS2.png",width=2300,height=900,res=130)

par(cex.main = 1.5, mar = c(5, 6, 2.5, 2.5) + 0.1, mgp = c(2.5, 1, 0), cex.lab = 2, 
    cex.axis = 1.25, bty = "n", las = 1)

boxplot(exp1_red$bagsmade~exp1_red$village,xlab="Village",ylab="Conservation effort",las=1.5,
        names=c("Na Dang","Ke Cai","Doi 2","Doi 6","Ho Luong 1","Ho Huoi Luong", "Huoi Un","Muong Pon 2"),cex=1.5)

abline(h=median(exp1_red$bagsmade),lwd=2.5,lty=3)
dev.off()

#Figure S3 (Performance by treatment by village)

library(reshape2)
library(lattice)

efforts<-matrix(ncol=4,nrow=8)
for (j in 1:8){
  for (i in 1:4){
    efforts[j,i]<-mean(exp1_red$bagsmade[exp1_red$village==j&exp1_red$treat==i])}}
rownames(efforts)<-c("Na Dang","Ke Cai","Doi 2","Doi 6","Ho Luong 1","Ho Huoi Luong", "Huoi Un","Muong Pon 2")
colnames(efforts)<-c("Merit input","Merit output","Fixed equal","Fixed individual")
efforts <- as.data.frame(efforts)

efforts <- cbind(ID=rownames(efforts),  efforts)
efforts_re <- melt(efforts)

stderrors <- matrix(ncol=4,nrow=8)

for (j in 1:8){
  for (i in 1:4){
    stderrors[j,i]<-sem(exp1_red$bagsmade[exp1_red$village==j&exp1_red$treat==i])}}
stderr <- as.vector(stderrors)

efforts_re$ulim<-efforts_re$value+stderr
efforts_re$llim<-efforts_re$value-stderr
efforts_re$diff<-ifelse(efforts_re$variable == "Merit input", -0.25,
                      ifelse(efforts_re$variable == "Merit output", -0.0875,
                             ifelse(efforts_re$variable == "Fixed equal", 0.0875,0.25)))

png(file="../results/FigureS3.png",width=850,height=800,res=130)

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

barchart(efforts_re$value ~ efforts_re$ID, 
         groups = efforts_re$variable,
         ylim = c(0, 65),
         auto.key = list(rectangles = TRUE, points = FALSE),
         xlab = list(label = "Village", fontsize = 20),
         ylab = list(label = "Conservation effort", fontsize = 20),
         scales = list(alternating = FALSE, tck = c(1,0), cex=1.25,x=list(rot=45)),
         panel = function(x, y, ..., subscripts) 
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = efforts_re$llim[subscripts]
           ul = efforts_re$ulim[subscripts]
           
           panel.segments(as.numeric(x) + efforts_re$diff[subscripts], ll, 
                          as.numeric(x) + efforts_re$diff[subscripts], ul, 
                          col = 'black', lwd = 1)                    
           
           panel.segments(as.numeric(x) + efforts_re$diff[subscripts] - 0.05, ll, 
                          as.numeric(x) + efforts_re$diff[subscripts] + 0.05, ll,
                          col = 'black', lwd = 1)
           
           panel.segments(as.numeric(x) + efforts_re$diff[subscripts] - 0.05, ul, 
                          as.numeric(x) + efforts_re$diff[subscripts] + 0.05, ul,
                          col = 'black', lwd = 1)
         })
dev.off()

#Figure S4 (Equity scores by village)

eqT_<-rbind(cbind(eqT1,eqT2,eqT3,eqT4))

colnames(eqT_) <- c("Merit input","Merit output","Fixed equal","Fixed individual")
rownames(eqT_) <- c("Na Dang","Ke Cai","Doi 2","Doi 6","Ho Luong 1","Ho Huoi Luong", "Huoi Un","Muong Pon 2")

perceps <- as.data.frame(eqT_)

perceps <- cbind(ID=rownames(perceps),  perceps)
perceps_re <- melt(perceps)

png(file="../results/FigureS4.png",width=850,height=800,res=130)

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

barchart(value~ID,data=perceps_re,groups=variable, 
         scales=list(tck = c(1,0), cex=1.25,x=list(rot=45,cex=1.25)),
         ylab=list(label="Equity score",fontsize=20),
         xlab=list(label="Village",fontsize=20),
         auto.key=T,ylim=c(-1,1))
         
dev.off()

#Figure S5 (Scree plot PCA)

png(file="../results/FigureS5.png")

scree(exp1_red$material)

dev.off()