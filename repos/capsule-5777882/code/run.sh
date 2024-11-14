#!/bin/bash 

mkdir ../results/figures
Rscript -e "rmarkdown::render(input = 'paper.Rmd', output_dir = '../results', clean = TRUE)"

