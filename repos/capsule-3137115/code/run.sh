#!/usr/bin/env bash
set -ex

Rscript -e "rmarkdown::render(input = 'manuscript.Rmd', output_dir = '../results', clean = TRUE)"