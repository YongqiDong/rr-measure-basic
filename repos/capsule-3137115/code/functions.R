# Loads functions

# function to calculate Wilson CIs on single proportions (using prop.test) then output them as a string with square brackets
propCI <- function(successes, total) {
  CI <- round(prop.test(successes,total)$conf.int*100, 0)
  CIstring <- paste0('CI [', CI[1], ',', CI[2], ']')
  return(CIstring)
}

# function to make first letter of a string uppercase
firstUp <- function(x) {
  x <- as.character(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

# define a "not in" operator
`%notin%` <- Negate(`%in%`)