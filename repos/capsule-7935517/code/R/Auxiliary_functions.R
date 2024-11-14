###################################
####    Auxiliary functions    ####
###################################

# Include auxiliary functions used for plots

#----    sample_rho

sample_rho <- function(rho = 0, n = 30, B = 1e4){
  rho_sim <- replicate(B, {
    data <-  mvrnorm(n, mu = c(0,0), Sigma = matrix(c(1,rho,rho,1), ncol = 2))
    cor(data[,1], data[,2])
   })

  return(rho_sim)
}

#----    get_legend    ----

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#----    retrodesignD    ----

retrodesignD <- function(d, n, alpha=.05, df=Inf, B=10000, seed=NULL){

  # Set seed
  if(!is.null(seed)){set.seed(seed = seed)}

  s <- 1/sqrt(n)
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-d/s, df)
  p.lo <- pt(-z-d/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- d + s*rt(B,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/d
  return(list(d=d, n=n, power=power, typeS=typeS, typeM=exaggeration))
}

#----    list2data    ----

list2data <- function(list, transpose=TRUE, select=NULL){
  if(transpose) list <- t(list)

  if(!is.null(select)){
    slected_arg = dimnames(list)[[2]] %in% select

    save_names = dimnames(list)[[2]][slected_arg]
    save_dim = dim(list)
    save_dim[2] = length(save_names)

    list <- list[rep(slected_arg, each=dim(list)[1])]
    dim(list) <- save_dim
    dimnames(list) <- list(NULL,save_names)
  }

  data <- as.data.frame(matrix(unlist(list),ncol=dim(list)[2], dimnames = dimnames(list)))

  return(data)
}

#----



