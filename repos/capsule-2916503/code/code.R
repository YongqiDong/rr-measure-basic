# read the csv
dat <- read.csv("../data/semlavaan.csv", header=TRUE, fileEncoding="CP932")

library(lavaan)
# https://lavaan.ugent.be/
model <- '
Achievement =~ Midterm + Final + Quiz
Proficiency =~ Exam1 + Exam2 + Exam3
Proficiency ~ Achievement
'

fit <- sem(model, data=dat)
summary(fit, standardized=TRUE, fit.measures=TRUE)

# Path diagram
library(semPlot)

# Standardized estimates
png("../results/plot1.png", width = 600, height = 600)
semPaths(fit, "std", style="lisrel", # LISREL style
         mar=c(6,1,3,1), # margin (bottom, left, up, right)
         edge.label.cex=.8, # font size for coefficients
         fade=F, # keep the arrow size the same
         theme="gray") # gray color
dev.off()