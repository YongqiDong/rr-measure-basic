#!/bin/bash
set -ex 

mkdir -p ../results/{Dose_Response_Script_Output,RNA_Seq_Script_Output,Resistance_and_Sensitivity_Genes_Script_Output,Fig6c_Script_Output}

Rscript -e "rmarkdown::render(input = 'Dose_Response_Script.Rmd', output_dir = '../results', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'RNA_Seq_Script.Rmd', output_dir = '../results', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'Resistance_and_Sensitivity_Genes_Script.Rmd', output_dir = '../results', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'Figure_6c_Script.Rmd', output_dir = '../results', clean = TRUE)"