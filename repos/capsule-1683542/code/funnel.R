#Produces funnel plots for each outcome, with trim and fill used where appropriate according to Egger's regression test

#Before vs After Exercise
for (i in 1:length(funneldata.S)){
datS<-funneldata.S[[i]]
datS <- escalc(measure="MD", m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., data=datS)
res <- rma(measure="SMD",m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., 
           data=datS, slab= paste(ID))
png(filename = paste("../results/", funnelnames.S[i] ,".png",sep=""), height=8, width=8, units="in", res=300)
funnel(res, xlab = "Correlation coefficient")
dev.off()
}

#Exercise vs Usual-Care Patients
for (i in 1:length(funneldata.R)){
datR<-funneldata.R[[i]]
datR <- escalc(measure="MD", m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., data=datS)
res <- rma(measure="SMD",m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., 
           data=datR, slab= paste(ID))
png(filename = paste("../results/", funnelnames.R[i] ,".png",sep=""), height=8, width=8, units="in", res=300)
funnel(res, xlab = "Correlation coefficient")
dev.off()
    }

#Trim and Fill Plots
for (i in 1:length(TAFdata)){
datS<-TAFdata[[i]]
datS <- escalc(measure="MD", m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., data=datS)
res <- rma(measure="SMD",m1i=Post.Mean, sd1i=Post.SD, n1i=Post..N., m2i=Pre.Mean, sd2i=Pre.SD, n2i=Pre..N., 
           data=datS, slab= paste(ID))
res.taf<-trimfill(res)
png(filename = paste("../results/", TAFnames[i] ,".png",sep=""), height=8, width=8, units="in", res=300)
funnel(res.taf, xlab = "Correlation coefficient")
dev.off()
    }