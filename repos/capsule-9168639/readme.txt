In this study, we assessed whether application forms to Research Ethics Committees contained requests for sample size justifications and sample size calculations.

A study protocol was pre-registered on the Open Science Framework at https://doi.org/10.17605/OSF.IO/RHW3M

This deposit includes the following folders and files:

#####################
#### environment ####
#####################

Dockerfile contain the environment necessary to execute the analysis.Rmd file

################
##### code #####
################

flowchart.PNG is the flowchat figure

analysis.Rmd contained the analysis script and outputs the data for Figure 1 of the manuscript in the file analysis.pdf

################
##### data #####
################

rec_data_raw.csv contains the raw data, including archived links to the ethics forms

A codebook is provided in the file rec_codebook.csv and outline the meaning of each variable name in rec_data_raw.csv

############################
#### additional details ####
############################

All data and codebooks are in CSV format and can be opened using a number of software packages.

The analysis code was written in RMarkdown. The dependencies necessary to run the code are provided in the Dockerfile. The easiest way to re-run the code is by clicking "Reproducible Run" in this manuscript's reproducible container on Code Ocean [insert link when ready]. Running the code will output the numbers used for Figure 1 of the manuscript in pdf format. Alternatively, R can be downloaded from www.r-project.org/