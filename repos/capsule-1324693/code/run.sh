#!/bin/bash
Rscript -e "rmarkdown::render('main.Rmd', output_dir = '../results')"
