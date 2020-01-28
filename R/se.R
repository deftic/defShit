#############################################
#                                           #
#             missing Functions             #
#                                           #
#############################################

# requires a vector containing numeric values
# calculates standard error of the mean 

se <- function (x) {
  sem <- sqrt(var(x, na.rm = TRUE)/length(na.omit(x)))
  return(sem)
}

# usage:
#
# se(c(5, 4, 6, 5, 5, 4, 4, 5, 6))
