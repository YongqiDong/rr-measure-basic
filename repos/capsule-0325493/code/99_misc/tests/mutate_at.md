``` r
library(dplyr)
dataSet <- iris
print(class(dataSet$Sepal.Length))
```

    ## [1] "numeric"

``` r
data <-  mutate_at(iris,vars(Sepal.Length),scale)
print(class(data$Sepal.Length))
```

    ## [1] "matrix"
