#Produces forest plots for each outcome, then adds informative text labels and data

#Before vs After Exercise
for (i in 1:length(mydataS)){
datS<-mydataS[[i]]
res <- rma(measure="SMD",m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., 
           data=datS, slab= paste(ID))   
png(filename = paste("../results/", namesS[i] ,".png",sep=""), height=8, width=8, units="in", res=300)
forest(res, cex=.8, xlim=c(min(res$yi)-7,max(res$yi)+4),mlab="", col = c(rep('black', 5), 'red'))
text(max(res$yi[1: length(res$yi)] + 1.96*sqrt(res$vi[1: length(res$yi)]))+2.5, dim(datS)[1]+2, "Observed SMD [95% CI]", pos=2, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2, dim(datS)[1]+2,"Before (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2, y= c( dim(datS)[1]:1), datS$Pre..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1, dim(datS)[1]+2,"After (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1, y= c(dim(datS)[1]:1), datS$Post..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2,-1,sum(datS$Pre..N.) ,pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1,-1,sum(datS$Post..N.) ,pos=4, cex=.8)
text(-0.8,-2,"Worsening", cex=0.8)
text(0.8,-2,"Improvement", cex=0.8)
Arrows(-1.4,-2,-2.3,-2, code = 2, arr.type = "triangle")
Arrows(1.4,-2, 2.3,-2, code = 2, arr.type = "triangle")
dev.off()
}

#Exercise vs Usual-Care Patients
for (i in 1:length(mydataR)){
datR<- mydataR[[i]]
png(filename = paste("../results/", namesR[i] ,".png",sep=""), height=12, width=12, units="in", res=300)
data.T<-subset(datR, Sample=="Exercise Trained Patients")
data.C<-subset(datR, Sample=="Non-Exercise Control Patients")
datT <- escalc(measure="SMD", m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., data=data.T)
datC <- escalc(measure="SMD", m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., data=data.C)
dat.both <- data.frame(data.T[,1:11],yi = datT$yi - datC$yi, vi = datT$vi + datC$vi)
res <- rma(yi, vi, data=dat.both, method="REML", digits=2, slab= paste(data.T$ID))
forest(res, cex=.8, xlim=c(min(res$yi)-9,max(res$yi)+4),mlab="")
text(max(res$yi[1: length(res$yi)] + 1.96*sqrt(res$vi[1: length(res$yi)]))+2.5, dim(datT)[1]+2, "Observed SMD [95% CI]", pos=2, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-4.5, dim(datT)[1]+2,"Ex \nBefore (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-4.5, y= c( dim(datT)[1]:1), datT$Pre..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-3.5, dim(datT)[1]+2,"Ex \nAfter (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-3.5, y= c(dim(datT)[1]:1), datT$Post..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-4.5,-1,sum(datT$Pre..N.) ,pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-3.5,-1,sum(datT$Post..N.) ,pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2.5, dim(datC)[1]+2,"UC \nBefore (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2.5, y= c( dim(datC)[1]:1), datC$Pre..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1.5, dim(datC)[1]+2,"UC \nAfter (N)",
     pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1.5, y= c(dim(datC)[1]:1), datC$Post..N., pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-2.5,-1,sum(datC$Pre..N.) ,pos=4, cex=.8)
text(min(res$yi[1: length(res$yi)] - 1.96*sqrt(res$vi[1: length(res$yi)]))-1.5,-1,sum(datC$Post..N.) ,pos=4, cex=.8)
text(-0.8,-1.5,"Worsening", cex=0.8)
text(0.8,-1.5,"Improvement", cex=0.8)
Arrows(-1.4,-1.5,-2.3,-1.5, code = 2, arr.type = "triangle")
Arrows(1.6,-1.5, 2.5,-1.5, code = 2, arr.type = "triangle")
dev.off()
}