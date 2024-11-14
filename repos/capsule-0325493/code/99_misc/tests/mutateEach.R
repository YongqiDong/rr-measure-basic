betterScale <- function(x){
  y <- scale(x)
  return(as.numeric(y))
}
library(dplyr)
dataSet <- iris
print(class(dataSet$Sepal.Length))
data <-  mutate_at(iris,vars(Sepal.Length),betterScale)
print(class(data$Sepal.Length))