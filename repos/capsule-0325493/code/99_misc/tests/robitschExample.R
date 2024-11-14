## Not run: 
#############################################################################
# EXAMPLE 1: Imputation of a binary variable
#############################################################################

#--- simulate missing values
set.seed(976)
G <- 30		# number of groups
n <- 8		# number of persons per group
iccx <- .2	# intra-class correlation X
iccy <- .3	# latent intra-class correlation binary outcome
bx <- .4	# regression coefficient
threshy <- stats::qnorm(.70)  # threshold for y
x <- rep( rnorm( G , sd = sqrt( iccx) ) , each=n )  +
  rnorm(G*n , sd = sqrt( 1 - iccx) )
y <- bx * x + rep( rnorm( G , sd = sqrt( iccy) ) , each=n )  +
  rnorm(G*n , sd = sqrt( 1 - iccy) )
y <- 1 * ( y > threshy )
dat <- data.frame( group = 100+rep(1:G , each=n) , x = x , y = y )

#* create some missings
dat1 <- dat
dat1[ seq( 1 , G*n , 3 ) ,"y" ]  <- NA
dat1[ dat1$group == 2 , "y" ] <- NA

#--- prepare imputation in mice
vars <- colnames(dat1)
V <- length(vars)
#* predictor matrix
predmat <- matrix( 0 , nrow=V , ncol=V)
rownames(predmat) <- colnames(predmat) <- vars
predmat["y" , ] <- c(-2,2,0)
#* imputation methods
impmeth <- rep("",V)
names(impmeth) <- vars
impmeth["y"] <- "2l.binary"

#** imputation with logistic regression ('2l.binary')
imp1 <- mice::mice( data = as.matrix(dat1) , imputationMethod = impmeth ,
                    predictorMatrix = predmat , maxit = 1 , m = 5 )
#** imputation with predictive mean matching ('2l.pmm')
impmeth["y"] <- "2l.pmm"
imp2 <- mice::mice( data = as.matrix(dat1) , imputationMethod = impmeth ,
                    predictorMatrix = predmat , maxit = 1 , m = 5 )

#** imputation with logistic regression using blme package                 
blme_args <- list( "cov.prior" = "invwishart")
imp3 <- mice::mice( data = as.matrix(dat1) , imputationMethod = impmeth ,
                    predictorMatrix = predmat , maxit = 1 , m = 5 , 
                    blme_use = TRUE , blme_args = blme_args )

## End(Not run)