#Produces two plots for the Activities of Daily Living data. Firstly comparing the proportion that reported improvement between time periods to those who showed no change or a worsening. Then comparing the proportion that reported worsening between time periods to those who showed no change or an improvement.

#####for improved vs worse & no change
dat<-mydata.ADL
dat$sort<-c(1:dim(dat)[1])
dat$ID<-factor(dat$ID)
dat$xi<-dat$Improved 
dat<-escalc(measure="PR", xi=xi, ni=Pre..N., data=dat)
res <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=dat, slab= paste(ID))

tmp <- t(sapply(split(dat, dat$sort), function(x) binom.test(x$xi, x$Pre..N.)$conf.int))
tmp[ order(row.names(tmp)), ]
dat$ci.lb <- tmp[,1]
dat$ci.ub <- tmp[,2]

png(filename = "../results/Figure.a10.png", height=8, width=12, units="in", res=300)                
# use refline =FALSE, if we want to remove it from the plot
with(dat, forest(yi, ci.lb=ci.lb, slab=paste(ID),ci.ub=ci.ub, ylim=c(1,50),  cex=0.7,
                 xlim=c(-1,1.5),refline=predict(res, transf=transf.ilogit)$pred,  
                 rows=c(2:7,11:17,21:27, 31:36, 40:45)))

### fit random-effects model in the five subgroups
adl.WD<-subset(dat, sub=="WD")
adl.MS<-subset(dat, sub=="MS")
adl.END<-subset(dat, sub=="END")
adl.DPA<-subset(dat, sub=="DPA")
adl.PF<-subset(dat, sub=="PF")
res.WD <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.WD, slab= paste(ID))
res.MS <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.MS, slab= paste(ID))
res.END <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.END, slab= paste(ID))
res.DPA <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.DPA, slab= paste(ID))
res.PF <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.PF, slab= paste(ID))
text(-1,8.5,"Walking Distance", adj = 0,cex = 0.8)
text(-1,18.5,"Muscle Strength", adj = 0,cex = 0.8)
text(-1,28.5,"Endurance", adj = 0,cex = 0.8)
text(-1,37.5,"Daily Physical Activity", adj = 0,cex = 0.8)
text(-1,46.5,"Physical Fitness", adj = 0,cex = 0.8)

### add summary polygons for the five subgroups
addpoly(res.PF, row=38.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.DPA, row=29.5, cex=.75,transf=transf.ilogit, mlab="")
addpoly(res.END, row=19.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.MS, row=9.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.WD, row=0.5, cex=.75, transf=transf.ilogit, mlab="")

text( 1.5,50, "Proportion [95% CI]", pos=2, cex=0.75, font=2)
text(-0.5,50,"Pre (N)", pos=4,cex=.75, font=2)
text(-0.5, c(2:7,11:17,21:27, 31:36, 40:45), dat$Pre..N., pos=4, cex=.7)
text(-0.2,50,"Post (N)", pos=4, cex=.75, font=2)
text(-0.2,  c(2:7,11:17,21:27, 31:36, 40:45), dat$Post..N., pos=4, cex=.7)
text(-0.5,-1.5,sum(dat$Pre..N.) ,pos=4,cex=.75, font=2)
text(-0.2,-1.5,sum(dat$Post..N.) ,pos=4, cex=.75, font=2)

dev.off()

#####for worse vs improved & no change
dat<-mydata.ADL
dat$sort<-c(1:dim(dat)[1])
dat$ID<-factor(dat$ID)
dat$xi<-dat$Improved + dat$`No Change`
dat<-escalc(measure="PR", xi=xi, ni=Pre..N., data=dat)
res <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=dat, slab= paste(ID))
tmp <- t(sapply(split(dat, dat$sort), function(x) binom.test(x$xi, x$Pre..N.)$conf.int))
tmp[ order(row.names(tmp)), ]
dat$ci.lb <- tmp[,1]
dat$ci.ub <- tmp[,2]
png(filename = "../results/Figure.a11.png", height=8, width=12, units="in", res=300)   
with(dat, forest(yi, ci.lb=ci.lb, slab=paste(ID),ci.ub=ci.ub, ylim=c(0,50),  cex=0.7,
                 xlim=c(-1,1.5),refline=predict(res, transf=transf.ilogit)$pred,  
                 rows=c(2:7,11:17,21:27, 31:36, 40:45)))
adl.WD<-subset(dat, sub=="WD")
adl.MS<-subset(dat, sub=="MS")
adl.END<-subset(dat, sub=="END")
adl.DPA<-subset(dat, sub=="DPA")
adl.PF<-subset(dat, sub=="PF")
res.WD <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.WD, slab= paste(ID))
res.MS <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.MS, slab= paste(ID))
res.END <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.END, slab= paste(ID))
res.DPA <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.DPA, slab= paste(ID))
res.PF <- rma.glmm(measure="PLO", xi=xi, ni=Pre..N., data=adl.PF, slab= paste(ID))
text(-1,8.5,"Walking Distance", adj = 0,cex = 0.8)
text(-1,18.5,"Muscle Strength", adj = 0,cex = 0.8)
text(-1,28.5,"Endurance", adj = 0,cex = 0.8)
text(-1,37.5,"Daily Physical Activity", adj = 0,cex = 0.8)
text(-1,46.5,"Physical Fitness", adj = 0,cex = 0.8)
addpoly(res.PF, row=38.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.DPA, row=29.5, cex=.75,transf=transf.ilogit, mlab="")
addpoly(res.END, row=19.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.MS, row=9.5, cex=.75, transf=transf.ilogit, mlab="")
addpoly(res.WD, row=0.5, cex=.75, transf=transf.ilogit, mlab="")
text( 1.5,50, "Proportion [95% CI]", pos=2, cex=0.75, font=2)
text(-0.5,50,"Pre (N)", pos=4,cex=.75, font=2)
text(-0.5, c(2:7,11:17,21:27, 31:36, 40:45), dat$Pre..N., pos=4, cex=.7)
text(-0.2,50,"Post (N)", pos=4, cex=.75, font=2)
text(-0.2,  c(2:7,11:17,21:27, 31:36, 40:45), dat$Post..N., pos=4, cex=.7)
text(-0.5,-1.5,sum(dat$Pre..N.) ,pos=4,cex=.75, font=2)
text(-0.2,-1.5,sum(dat$Post..N.) ,pos=4, cex=.75, font=2)
                
dev.off()       
################