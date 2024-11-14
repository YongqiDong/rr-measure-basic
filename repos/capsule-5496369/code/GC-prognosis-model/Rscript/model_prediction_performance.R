# List of packages 
packages<-c("openxlsx","tidyverse")
# Function to check and install package.
check_and_install_packages<-function(packages){
  for(package in packages){
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
    }
  }
}
# Run the function
check_and_install_packages(packages)
### GC prognostic model OS risk score
library(openxlsx)
library(tidyverse)
setwd("~/capsule/")
rm(list=ls())

# Create result folder.
result_folder_path<-"results/GC-prognosis-model/Figures/"
dir.create(paste0(result_folder_path,"Model-performance/"),recursive = TRUE)


risk_score<-read.xlsx('data/GC-prognosis-model/GC-prognosis-model-risk-score.xlsx',sheet = 1,rowNames = F,check.names=F)
risk_score$State=as.factor(as.integer(risk_score$State))
ylab="overall survival"
xlab="predicted risk score for GC"

p1<-ggplot(data=risk_score,aes(x=risk_score,y=OS,color=State))+
  geom_point(size=8)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x=xlab,y=ylab,color="")+
  geom_vline(xintercept=c(2.1026), linetype="dotted")+
  theme(axis.title.x = element_text(size = 16, hjust = 0.5, face = "plain"),
        axis.title.y = element_text(size = 16, face = "plain"), 
        axis.text.y = element_text(size = 16, face = "plain", colour = "black"),
        axis.text.x = element_text(size = 16, face = "plain", colour = "black"))+
  theme(legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16), 
        legend.spacing.y = unit(0.2, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(0.3, 'cm'))

pdf("results/GC-prognosis-model/Figures/Model-performance/GC-prognosis-model-prediction-performance.pdf",width=11.69,height = 8.27)
p1
dev.off()

