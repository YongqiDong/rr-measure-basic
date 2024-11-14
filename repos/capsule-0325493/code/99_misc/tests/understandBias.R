# detach("package:party", unload=TRUE)
library("partykit")

### linear regression
n <- 100
x <- sort(runif(n, min = -1, max = 1))
y <- rnorm(n, mean = 3 * x, sd = .1)
plot(x, y)

### stump: strong penalisation towards mean(y)
ct <- ctree(y ~ x, control = ctree_control(stump = TRUE))
lines(x, predict(ct), col = "blue")

# ct <- ctree(y ~ x)
# lines(x, predict(ct), col = "red")
# 
# 
# plot(ct)

### forest with small trees: same effect
cf <- cforest(y ~ x, control = ctree_control(minbucket = 20))
lines(x, predict(cf), col = "red")
# 
# ### forest with large trees: much smaller effect but
# ### penalisation still visible for small/large x values
# cf <- cforest(y ~ x, control = ctree_control(mincriterion = 0,
#                                              minbucket = 5, minsplit = 5))
# lines(x, predict(cf), col = "green")