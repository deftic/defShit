#############################################
#                                           #
#             missing Functions             #
#                                           #
#############################################

se <- function (x)
{
  sqrt(var(x, na.rm = TRUE)/length(na.omit(x)))
}

# usage:
#
# se(c(5, 4, 6, 5, 5, 4, 4, 5, 6))
