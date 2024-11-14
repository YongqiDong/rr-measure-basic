library(metafor)
library(shape)

#####################################################################################################
R.data<-read.csv("../data/Ex.vs.NonEx.csv", na.strings=c("N/A",""), stringsAsFactors = FALSE)
S.data<-read.csv("../data/Pre.Post.csv", na.strings=c("N/A",""), stringsAsFactors = FALSE)

#Forest Plots: Exercise vs Non-Exercise Patients
Figure.2<-subset(R.data,Outcome=="VO2") 
Figure.3<-subset(R.data,Outcome=="PeakW") 
Figure.a26<-subset(R.data,Outcome=="6MWT") 
Figure.a27<-subset(R.data,Outcome=="TUG") 
Figure.a28<-subset(R.data,Outcome=="SF36.VIT") 
Figure.a29<-subset(R.data,Outcome=="CK") 

#Forest Plots: Pre vs Post Exercise
Figure.4<-subset(S.data,Outcome=="VO2") 
Figure.5<-subset(S.data,Outcome=="PeakW") 
Figure.6<-subset(S.data,Outcome=="6MWT") 
Figure.7<-subset(S.data,Outcome=="STS.rep") 
Figure.8<-subset(S.data,Outcome=="STS") 
Figure.9<-subset(S.data,Outcome=="RiseFromSupine") 
Figure.10<-subset(S.data,Outcome=="SF36.AGG") 
Figure.11<-subset(S.data,Outcome=="FSS") 
Figure.12<-subset(S.data,Outcome=="CentralNuc") 
Figure.13<-subset(S.data,Outcome=="TypeI") 
Figure.14<-subset(S.data,Outcome=="TypeII") 
Figure.15<-subset(S.data,Outcome=="CitSyn") 
Figure.a12<-subset(S.data,Outcome=="TUG") 
Figure.a13<-subset(S.data,Outcome=="SF36.PF") 
Figure.a14<-subset(S.data,Outcome=="SF36.VIT") 
Figure.a15<-subset(S.data,Outcome=="SF36.GH") 
Figure.a16<-subset(S.data,Outcome=="SF36.MH") 
Figure.a17<-subset(S.data,Outcome=="CK") 
Figure.a18<-subset(S.data,Outcome=="TypeI.percent") 
Figure.a19<-subset(S.data,Outcome=="TypeII.percent") 
Figure.a20<-subset(S.data,Outcome=="TypeIIa.percent") 
Figure.a21<-subset(S.data,Outcome=="CapDens") 
Figure.a22<-subset(S.data,Outcome=="IsokineticPT") 
Figure.a23<-subset(S.data,Outcome=="Knee") 
Figure.a24<-subset(S.data,Outcome=="Elbow") 
Figure.a25<-subset(S.data,Outcome=="HandGrip") 

#Right-hand sided data only where appropriate
Figure.a23<-Figure.a23[Figure.a23$Side!="L",]
Figure.a24<-Figure.a24[Figure.a24$Side!="L",]
Figure.a25<-Figure.a25[Figure.a25$Side!="L",]
#####################################################################################################

#Funnel Plots: Exercise vs Non-Exercise Patients
Figure.a30<-subset(R.data,Outcome=="VO2") 
Figure.a31<-subset(R.data,Outcome=="PeakW")  #TAF
Figure.a32<-subset(R.data,Outcome=="6MWT") 
Figure.a33<-subset(R.data,Outcome=="TUG")  #TAF
Figure.a34<-subset(R.data,Outcome=="SF36.VIT")  #TAF
Figure.a35<-subset(R.data,Outcome=="CK") 

#Funnel Plots: Pre vs Post Exercise
Figure.a36<-subset(S.data,Outcome=="VO2")
Figure.a37<-subset(S.data,Outcome=="PeakW") 
Figure.a38<-subset(S.data,Outcome=="6MWT")  #TAF
Figure.a39<-subset(S.data,Outcome=="CK") 
Figure.a40<-subset(S.data,Outcome=="FSS")  #TAF
Figure.a41<-subset(S.data,Outcome=="TypeI")  #TAF 
Figure.a42<-subset(S.data,Outcome=="Knee") 
Figure.a43<-subset(S.data,Outcome=="IsokineticPT")  #TAF

#####################################################################################################
#Collate Outcomes: Forest Data
namesS<-c("Figure.4","Figure.5","Figure.6","Figure.7","Figure.8","Figure.9","Figure.10","Figure.11","Figure.12","Figure.13",
          "Figure.14","Figure.15",
          "Figure.a12","Figure.a13","Figure.a14","Figure.a15","Figure.a16","Figure.a17","Figure.a18","Figure.a19","Figure.a20","Figure.a21",
          "Figure.a22","Figure.a23","Figure.a24","Figure.a25")
namesR<-c("Figure.2","Figure.3","Figure.a26","Figure.a27","Figure.a28","Figure.a29")
mydataS<-list(Figure.4,Figure.5,Figure.6,Figure.7,Figure.8,Figure.9,Figure.10,Figure.11,Figure.12,Figure.13,Figure.14,Figure.15,
              Figure.a12,Figure.a13,Figure.a14,Figure.a15,Figure.a16,Figure.a17,Figure.a18,Figure.a19,Figure.a20,Figure.a21,Figure.a22,
              Figure.a23,Figure.a24,Figure.a25)
mydataR<-list(Figure.2,Figure.3,Figure.a26,Figure.a27,Figure.a28,Figure.a29)

#Funnel Data
funneldata.S<-list(Figure.a36,Figure.a37,Figure.a29,Figure.a42) 
funneldata.R<-list(Figure.a30,Figure.a32,Figure.a35)
funnelnames.S<-c("Figure.a36","Figure.a37","Figure.a39","Figure.a42")
funnelnames.R<-c("Figure.a30","Figure.a32","Figure.a35")

#Trim and Fill Data
TAFdata<-list(Figure.a31,Figure.a33,Figure.a36,Figure.a38,Figure.a40,Figure.a41,Figure.a43)
TAFnames<-c("Figure.a31","Figure.a33","Figure.a34","Figure.a38","Figure.a40","Figure.a41","Figure.a43")
#####################################################################################################

#####Activities of Daily Living Data
adl<-read.csv("../data/ADL.csv")

#subset groups, complete cases only
adl.WD<-adl[c(7,1:6),c(1:6,19:21)]
adl.WD<-adl.WD[complete.cases(adl.WD),]
adl.MS<-adl[c(7,1:6),c(1:3,7:9,19:21)]
adl.MS<-adl.MS[complete.cases(adl.MS),]
adl.END<-adl[c(7,1:6),c(1:3,10:12,19:21)]
adl.END<-adl.END[complete.cases(adl.END),]
adl.DPA<-adl[c(7,1:6),c(1:3,13:15,19:21)]
adl.DPA<-adl.DPA[complete.cases(adl.DPA),]
adl.PF<-adl[c(7,1:6),c(1:3,16:18,19:21)]
adl.PF<-adl.PF[complete.cases(adl.PF),]
names(adl.WD)[4:6]<-c("Improved","No Change","Worse")
names(adl.MS)[4:6]<-c("Improved","No Change","Worse")
names(adl.END)[4:6]<-c("Improved","No Change","Worse")
names(adl.DPA)[4:6]<-c("Improved","No Change","Worse")
names(adl.PF)[4:6]<-c("Improved","No Change","Worse")

#create dataset for ADL.R
namesADL<-c("adl.WD","adl.MS","adl.END","adl.DPA","adl.PF")
mydata.ADL<-rbind(adl.WD,adl.MS,adl.END,adl.DPA,adl.PF)
mydata.ADL$sub<-c(rep("WD",6),rep("MS",7),rep("END",7),rep("DPA",6),rep("PF",6))
#############################################################

