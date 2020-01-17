#' Finds local maxima on peak traces
#'
#' @param z 
#'
#' @return 
#' @export
#'
#' @examples
#' test <- localMaxima(z)
#' 

localMaxima <- function(z) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, z)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (z[[1]] == z[[2]]) {
    y <- y[-1]
  }
  y
}

