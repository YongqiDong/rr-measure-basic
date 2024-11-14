projDir <- ''
rmarkdown::render(input = '/code/getAllNumbers.Rmd', output_dir = '/results/', clean = TRUE) 
source('makeAllFigures.R')
