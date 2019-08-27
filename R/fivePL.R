#' Five parameters logistic regression
#'
#' @param a  the minimum value that can be obtained (i.e. what happens at 0 dose)
#' @param b  Hill’s slope of the curve (i.e. this is related to the steepness of the curve at point c)
#' @param c  the point of inflection (i.e. the point on the S shaped curve halfway between a and d)
#' @param d  the maximum value that can be obtained (i.e. what happens at infinite dose)
#' @param m  Asymmetry factor. When m=1 we have a symmetrical curve around inflection point and so we have a four-parameters logistic equation.
#' @param y  deltaF / F
#'
#' @return
#' @export
#'
#' @examples

fivePL <- function (a, b, c, d, m, y)
{
  x <-  c * (((a - d) / (y - d))^(1/m) - 1)^(1/b)

  print(x)

}

# usage:
#
# fivePL(0.1403, 1.3780, 2.1588, 2.4301, 0.8301, 0.5)
