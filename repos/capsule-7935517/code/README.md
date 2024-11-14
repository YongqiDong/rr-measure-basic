# Designing Studies and Evaluating Research Results: Type M and Type S Errors for Pearson Correlation Coefficient

Giulia Bertoldo, Claudio Zandonella Callegher, and Gianmarco Altoè

Department of Developmental Psychology and Socialisation, University of Padova, Padova, Italy

**Submitted to *Meta-psychology* **

### Abstract

It is widely appreciated that many studies in psychological science suffer from low statistical power. One of the consequences of analyzing underpowered studies with thresholds of statistical significance, is a high risk of finding exaggerated effect size estimates, in the right or in the wrong direction. These inferential risks can be directly quantified in terms of Type M (magnitude) error and Type S (sign) error, which directly communicate the consequences of design choices on effect size estimation. Given a study design, Type M error is the factor by which a statistically significant effect is on average exaggerated. Type S error is the probability to find a statistically significant result in the opposite direction to the plausible one. Ideally, these errors should be considered during a *prospective design analysis* in the design phase of a study to determine the appropriate sample size. However, they can also be considered when evaluating studies’ results in a *retrospective design analysis*. In the present contribution we aim to facilitate the considerations of these errors in the research practice in psychology. For this reason we illustrate how to consider Type M and Type S errors in a design analysis using one of the most common effect size measures in psychology: Pearson correlation coefficient. We provide various examples and make the R functions freely available to enable researchers to perform design analysis for their research projects.


### Repository Structure

In this repository we collect all the material used in the article. To compile and execute the different scripts, the whole repository is required as there are some dependencies between files. The repository is organized as an R-project, thus we suggest to download the whole repository and open the project in a new R session.

In the folder `Documents/Paper_main/` you can find the scripts to compile the pdf version of the paper:

- [Paper_main.pdf](https://osf.io/5r7g9/) is the the pdf of the paper
- [Paper_main.Rnw](https://osf.io/nkjrv/) is the R script to compile the pdf including R chunk code
- [Paper_main.tex](https://osf.io/24p8j/) is the LaTeX script to compile the pdf
- [Paper_main.bib](https://osf.io/ejb6c/) is the reference list
- `screens/` is a folder with images included in the paper

In the folder `R` you can find the R scripts with the functions code:

- [Design_analysis_r.R](https://osf.io/9q5fr/) the R functions for prospective and retrospective design analysis are defined
- [Auxiliary_functions.R](https://osf.io/7kbm2/) other R functions are defined for the plots included in the paper

In the folder `Data/` you can find the pre-compiled datasets used in the plots and tables to avoid long running times. To obtain these datasets you can run the chunks code where `eval=F`.

In the folder `renv/` there are information about project’s R dependencies. To know more about `renv` see its homepage ([link](https://rstudio.github.io/renv/)).

### CodeOcean

The present project is availabel also on the CodeOcean platform https://codeocean.com/. The CodeOcean platform allows yo generate a standard, secure, and executable research package called a Capsule.

To reproduce the code:

1. Open the Capsule at the following link https://codeocean.com/capsule/7935517
2. Press the blue button "Reproducible Run" on the top right corner (you need to be registered).
3. A new session will be lunched to run the code and all the results will be saved in the folder `results/`

Note that the LaTeX output is created (´.tex` file not the PDF). To obtain the PDF:

1. Download the folder `results/` (from the lower left corner) with all the files required to compile the PDF. 
2. Use your preferred LaTeX distribution (or online editor such as Overleaf https://www.overleaf.com/) to compile the `Paper_main.tex` file.

Note that `pdflatex` engine should be used to compile the `.tex` file and `Biber` backend should be used to compile the bibliography.