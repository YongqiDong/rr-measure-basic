#---------------------------------------------#
#----    Rho Design Analysis functions    ----#
#---------------------------------------------#

#----    Dependencies    ----

# Load needed libraries

library(MASS)
library(docstring)

#----    compute_r_crit    ----


compute_crit_r <- function(alternative, sig_level, n){
  #' Compute Correlation Critical Value
  #'
  #'@description This is an internal function used in \code{retro_r} and
  #'  \code{pro_r}.
  #'
  #' Under the hypothesis \eqn{\rho=0}, Pearson's correlation sampling
  #' distribution follows a \emph{t-}distribution with \eqn{n-2} degrees of
  #' freedom. Given the alternative hypothesis, the significance level, and the
  #' sample size, \code{compute_crit_r()} function computes the corresponding
  #' critical correlation value (i.e., the minimum correlation absolute value
  #' that would result statistically significant).
  #'
  #' @param alternative indicates the alternative hypothesis and must be one of
  #'   "two.sided", "greater" or "less".
  #' @param sig_level a numeric value indicating the significance level on which
  #'   the alternative hypothesis is evaluated.
  #' @param n a numeric value indicating the sample size.
  #'
  #' @return A numeric value indicating the critical correlation value. In the
  #'   case of \code{alternative = "two.sided"}, two values are returned with
  #'   the positive and negative sign.
  #'
  #' @examples
  #' compute_crit_r(alternative = "two.sided", sig_level = .05, n = 20)
  #' compute_crit_r(alternative = "greater", sig_level = .10, n = 10)
  #' compute_crit_r(alternative = "two.sided", sig_level = .001, n = 30)

  if (alternative=="two.sided"){
    t_crit<-qt(1-sig_level/2,n-2)
    rtemp<- t_crit/sqrt(n-2+t_crit^2)
    r_crit<-c(-rtemp,rtemp)
  }else if (alternative=="greater"){
    t_crit<-qt(1-sig_level,n-2)
    rtemp<- t_crit/sqrt(n-2+t_crit^2)
    r_crit<-c(rtemp)
  }else if (alternative=="less"){
    t_crit<-qt(1-sig_level,n-2)
    rtemp<- t_crit/sqrt(n-2+t_crit^2)
    r_crit<-c(-rtemp)
  }

  return(r_crit)
}


#----    retro_r    ----


retro_r<-function(rho, n, alternative = c("two.sided", "less", "greater"), sig_level=.05, B=1e4, seed=NULL){
  #' Retrospective Design Analysis for Perason Correlation Coefficient
  #'
  #' Given the hypothetical population correlation value and sample size, the
  #' function \code{retro_r()} performs a retrospective design analysis
  #' according to the defined alternative hypothesis and significance level.
  #' Power level, Type-M error, and Type-S error are computed together with the
  #' the critical correlation value (i.e., the minimum absolute correlation
  #' value that would result significant).
  #'
  #' @param rho a number indicating the hypothetical population correlation value.
  #' @param n a numeric value indicating the sample size.
  #' @param alternative indicates the alternative hypothesis and must be one of
  #'   "two.sided", "greater" or "less". You can specify just the initial
  #'   letter.
  #' @param sig_level a numeric value indicating the significance level on which
  #'   the alternative hypothesis is evaluated.
  #' @param B a numric  value indicating the number of iterations. Increase the
  #'   number of iterations to obtain more stable results.
  #' @param seed a numeric value indicating the seed for random number
  #'   generation. Set the seed to make results reproducible.
  #'
  #' @return A list containing the following components:
  #'   \item{rho}{the hypothetical population correlation value considered.}
  #'   \item{n}{the sample size.}
  #'   \item{alternative}{the alternative hypothesis considered.}
  #'   \item{sig_level}{the significance level on which the alternative
  #'   hypothesis was evaluated.}
  #'   \item{power}{the resulting power level.}
  #'   \item{typeM}{the resulting Type-M error.}
  #'   \item{typeS}{the resulting Type-S error.}
  #'   \item{crit_r}{the critical correlation value (i.e., the minimum absolute
  #'   correlation value that would result significant). In the case of
  #'   \code{alternative = "two.sided"}, two values are returned with the
  #'   positive and negative sign.}
  #'
  #' @references Gelman, A., & Carlin, J. (2014). Beyond Power Calculations:
  #'  Assessing Type S (Sign) and Type M (Magnitude) Errors. Perspectives on
  #'  Psychological Science, 9(6), 641–651.
  #'  https://doi.org/10.1177/1745691614551642
  #'
  #'  todo: Add our pre-print
  #'
  #' @examples
  #' retro_r(rho = .4, n = 20, alternative = "two.sided")
  #' retro_r(rho = -.3, n = 30, alternative = "less")
  #' retro_r(rho = .4, n = 20, alternative = "greater", sig_level=.10)
  #' retro_r(rho = .3, n = 30, alternative = "two.sided", B=1e5)

  # Set seed
  if(!is.null(seed)){set.seed(seed = seed)}

  # Check on alternative
  alternative = match.arg(alternative)
  if(rho>0 && alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 && alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")

  # Simulate observations and cor.test iterations
  r_sim<-p_val<-vector(mode = "double", length = B)
  for(i in 1:B) {
    d<-mvrnorm(n=n,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    res_cor.test = cor.test(d[,1],d[,2],alternative = alternative)
    r_sim[i] = res_cor.test$estimate
    p_val[i] = res_cor.test$p.value
  }

  # Compute results
  power = sum( p_val < sig_level ) / B
  typeS = 0
  if(alternative == "two.sided") {
    if (rho>0) {
      typeS <- sum((p_val < sig_level) & (r_sim < 0)) / sum(p_val < sig_level)
    } else {
      typeS <- sum((p_val < sig_level) & (r_sim > 0)) / sum(p_val < sig_level)}
  }
  typeM <- mean(abs(r_sim[p_val < sig_level])) / abs(rho)

  # critical r
  crit_r = compute_crit_r(alternative = alternative, sig_level = sig_level, n = n)

  # Result object
  out<-structure(list(rho = rho, n = n,  alternative = alternative, sig_level = sig_level,
                      power = power, typeM = typeM, typeS = typeS, crit_r = crit_r),
                 class = c("design_analysis","list"))

  return(out)
}


#----    my_pro.r    ----

pro_r<-function(rho, power = .80, alternative = c("two.sided", "less", "greater"), sig_level = .05,
                range_n = c(1,1000), B = 1e4, tol = .01, display_message = FALSE, seed = NULL){
  #' Prospective Design Analysis for Perason Correlation Coefficient
  #'
  #' Given the hypothetical population correlation value and the required power
  #' level, the function \code{pro_r()} performs a prospective design analysis
  #' according to the defined alternative hypothesis and significance level. The
  #' required sample size is computed together with the associated Type-M error,
  #' Type-S error, and the the critical correlation value (i.e., the minimum
  #' absolute correlation value that would result significant).
  #'
  #' @param rho a number indicating the hypothetical population correlation value.
  #' @param power a numeric value indicating the required power level.
  #' @param alternative indicates the alternative hypothesis and must be one of
  #'   "two.sided", "greater" or "less". You can specify just the initial
  #'   letter.
  #' @param sig_level a numeric value indicating the significance level on which
  #'   the alternative hypothesis is evaluated.
  #' @param range_n a lenght-2 numeric vector indicating the minimum and maximum
  #'   sample size considered in the evaluation of the required sample size.
  #' @param B a numric  value indicating the number of iterations. Increase the
  #'   number of iterations to obtain more stable results.
  #' @param tol a numeric value indicating the tolerance of required power level.
  #' @param display_message a logical variable indicating whether to display or
  #'   not the information about computational steps.
  #' @param seed a numeric value indicating the seed for random number
  #'   generation. Set the seed to make results reproducible.
  #'
  #' @return A list containing the following components:
  #'   \item{rho}{the hypothetical population correlation value considered.}
  #'   \item{n}{the required sample size.}
  #'   \item{alternative}{the alternative hypothesis considered.}
  #'   \item{sig_level}{the significance level on which the alternative
  #'   hypothesis was evaluated.}
  #'   \item{power}{the resulting power level.}
  #'   \item{typeM}{the resulting Type-M error.}
  #'   \item{typeS}{the resulting Type-S error.}
  #'   \item{crit_r}{the critical correlation value (i.e., the minimum absolute
  #'   correlation value that would result significant). In the case of
  #'   \code{alternative = "two.sided"}, two values are returned with the
  #'   positive and negative sign.}
  #'
  #' @references Gelman, A., & Carlin, J. (2014). Beyond Power Calculations:
  #'  Assessing Type S (Sign) and Type M (Magnitude) Errors. Perspectives on
  #'  Psychological Science, 9(6), 641–651.
  #'  https://doi.org/10.1177/1745691614551642
  #'
  #'  todo: Add our pre-print
  #'
  #' @examples
  #' pro_r(rho = .4, power = .8)
  #' pro_r(rho = .4, power = .8, alternative = "greater")
  #' pro_r(rho = -.4, power = .8, alternative = "less", sig_level=.001)
  #' pro_r(rho = .3, power = .6, tol = .01)

  # Set seed
  if(!is.null(seed)){set.seed(seed = seed)}

  # Check on alternative
  alternative = match.arg(alternative)
  if(rho>0 && alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 && alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")

  # Set values
  r_sim<-p_val<-vector(mode = "double", length = B)
  n_seq <- seq( range_n[1], range_n[2], by = 1 )
  (n_target <- round(median(n_seq)))
  find_power <- FALSE

  # Check with maximum N
  for (i in 1:B){
    d<-mvrnorm(n=range_n[2],mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    p_val[i] = cor.test(d[,1],d[,2],alternative = alternative)$p.value
  }
  est_power<-sum(p_val<sig_level)/B

  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", range_n[2]),"\n")
    cat(paste0("   try to increase maximum of range_n > ", range_n[2],"."),"\n")
    out <- NULL
  } else {

    # Find the required sample size
    while( (!find_power) ) {

      # Simulate values loop
      for(i in 1:B) {
        d<-mvrnorm(n=n_target,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
        res_cor.test = cor.test(d[,1],d[,2],alternative = alternative)
        r_sim[i] = res_cor.test$estimate
        p_val[i] = res_cor.test$p.value
      }

      est_power=sum(p_val<sig_level)/B

      if (display_message == TRUE){
        cat("Evaluate n =", n_target, fill=TRUE)
        cat("Estimated power is", round(est_power,2), fill=TRUE)
        cat("\n")
      }


      # Evaluate if power was obtained according to tolerance value
      if ( (est_power<=(power+tol)) && (est_power>=(power-tol)) ) {
        find_power <- TRUE
      } else {
        if (length(n_seq)==1) { stop("Increase tolerance value")
        } else if (est_power > (power+tol)) {
          (n_seq <- seq( min(n_seq), n_target-1, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq(n_target+1, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }

    # Compute results
    typeS<-0
    if(alternative == "two.sided") {
      if (rho>0) {typeS <- sum((p_val < sig_level) & (r_sim < 0)) / sum(p_val < sig_level)}
      else {typeS <- sum((p_val < sig_level) & (r_sim > 0)) / sum(p_val < sig_level)}
    }
    typeM <- mean(abs(r_sim[p_val < sig_level])) / abs(rho)

    # mean sig
    crit_r = compute_crit_r(alternative = alternative, sig_level = sig_level, n = n_target)

    # Result object
    out <- structure(list( rho = rho, n = n_target, alternative = alternative, sig_level = sig_level,
                           power = est_power, typeM = typeM, typeS = typeS, crit_r = crit_r),
                     class = c("design_analysis","list"))
  }
  if (!is.null(out))  return(out)
}

#----    print.design_analysis (single line)    ----

# print.design_analysis <- function(DA_list){
#   output_text <- DA_list
#
#   # Round
#   arguments <- unlist(lapply(DA_list, is.numeric))
#   if(sum(arguments)!=0){
#     output_text[arguments] <- lapply(output_text[arguments], round, 3)
#   }
#
#   # Combine
#   lenght_2 <- which(unlist(lapply(DA_list,function(x) length(x)!=1)))
#   if(sum(lenght_2)!=0){
#     output_text[lenght_2] <- paste0("±",abs(output_text[[lenght_2]][2]))
#   }
#
#   cat(capture.output(noquote(unlist(output_text))), sep = '\n')
#   invisible(DA_list)
# }

#----    print.design_analysis  (multiple lines)   ----

print.design_analysis <- function(DA_list, prefix="\t"){
  output_text <- DA_list

  # Round
  arguments <- unlist(lapply(DA_list, is.numeric))
  if(sum(arguments)!=0){
    output_text[arguments] <- lapply(output_text[arguments], round, 3)
  }

  # Combine
  lenght_2 <- which(unlist(lapply(DA_list,function(x) length(x)!=1)))
  if(sum(lenght_2)!=0){
    output_text[lenght_2] <- paste0("±",abs(output_text[[lenght_2]][2]))
  }

  cat("\n")
  cat(strwrap("Design Analysis", prefix = prefix), sep = "\n")
  cat("\n")
  cat("Hypothesized effect: rho = ", DA_list$rho, "\n", sep = "")
  cat("\n")
  cat("Study characteristics:",
      capture.output(print(as.data.frame(t(unlist(
        output_text[c("n", "alternative", "sig_level")]))),
        print.gap = 3, right = F, row.names = F)), sep = '\n')
  cat("\n")
  cat("Inferential risks:",
      capture.output(print(as.data.frame(t(unlist(
        output_text[c("power", "typeM", "typeS")]))),
        print.gap = 3, right = F, row.names = F)), sep = '\n')
  cat("\n")
  cat("Critical value(s): r = ", output_text$crit_r)

  invisible(DA_list)
}

#----





