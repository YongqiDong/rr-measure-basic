projDir <- ""
source(file.path(projDir,'code/global.R'))

#parameters
selOutcomes <- c("FS_TotalGrayVol","FS_CortexVol")
figHome <- file.path(projDir,'results/')
fakeFigHome <- file.path(projDir,'fakeFigs/')
codeHome <- file.path(projDir,'code/07_figures/')
paperWidth <- elsevierSize(3)
paperHeight <- paperWidth*9/16*2 

#to check that all plot files use the same data
#checks that the stuff that is in list1 and 2 is the same
#and then adds the stuff of list1 to list2
#i.e. checks if the data files that are shared between plots
#are the same

checkPlot <- function(list1,list2){
  excempt <- c('getCorrelation','linearModelImp')
  toCompare <- setdiff(intersect(names(list1),names(list2)),excempt)
  stopifnot(identical(list1[toCompare],list2[toCompare]))
  if (any(names(list2) %in% excempt)){
    specialVar <- names(list2)[names(list2) %in% excempt]
    stopifnot(length(specialVar)==1)
    if(specialVar %in% names(list1)){
      list1[[specialVar]] <-c(list1[[specialVar]], list2[[specialVar]])
    }
  }
  newEntries <- setdiff(names(list2),names(list1))
  list1[newEntries] <- list2[newEntries]
  return(list1)  
}
#hack to get empty named list
usedFiles <- list(a=3)
usedFiles <- usedFiles[character()]

#1: ICC
print('1 ICC')
source(file.path(codeHome,'ICCplot.R'))
# #2: SPM
print('2 Corrplot')
source(file.path(codeHome,'CorrPlotNew.R'))
# # #3: within subject corrs
print('3 CorrplotPred')
source(file.path(codeHome,'corrPlotPred.R'))
# #4: Step wise
print('4 Stepwise')
source(file.path(codeHome,'StepwisePlot.R'))
# #5: anova like model selection
print('5 ANOVA')
source(file.path(codeHome,'anova.R'))
print('6 MV')
# #6 multivariate plot
source(file.path(codeHome,'MVPlot.R'))

print('7 LASSO')
# #7 LASSO variable importance0
source(file.path(codeHome,'lassoVarImp.R'))

print('8 RV')
# #8 random forest variable importance0
source(file.path(codeHome,'rfVarImp.R'))

print('9 Partial Residuals')
# #9 partial residual
source(file.path(codeHome,'partialResidual.R'))


print('10 CIs')
# #9 effect sizes in absolute and percent
source(file.path(codeHome,'CIPlot.R'))
CIPlot()