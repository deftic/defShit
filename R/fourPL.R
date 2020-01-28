#' Four parameters logistic regression
#'
#' @param a  the minimum value that can be obtained (i.e. what happens at 0 dose)
#' @param b  Hill’s slope of the curve (i.e. this is related to the steepness of the curve at point c)
#' @param c  the point of inflection (i.e. the point on the S shaped curve halfway between a and d)
#' @param d  the maximum value that can be obtained (i.e. what happens at infinite dose)
#' @param y  deltaF / F
#'
#' @return
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3 

fourPL <- function (a, b, c, d, y)
{
  x <-  c * (((a - d) / (y - d)) - 1)^(1/b)

  print(x)

}

# usage:
#
# fourPL(0.1483, 1.2906, 2.6337, 2.4351, 1.5)
