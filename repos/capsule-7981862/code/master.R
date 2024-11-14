##############################################################################
##############################################################################
##############################################################################
##############################################################################
## Master script to run replication code for:
## Zhukov, Byers, Davidson, and Kollman,
## "Integrating Data Across Misaligned Spatial Units," 
## Political Analysis (conditionally accepted 10/2022)
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# Clear workspace
rm(list=ls())

# Set working directory (path should be to folder that contains master.R file)
# setwd(grep("Replication_ZBDK$",list.dirs(recursive=TRUE),value=TRUE)) 
if(grepl("code$",getwd())==TRUE){setwd("../")}

# Work from intermediate results?
intermz <- TRUE
write.table(intermz,file="data/r_output/intermz_param.txt")

# Source code 
source("code/run1_setup.R")
source("code/run2_main.R")
source("code/run3_appendix.R")