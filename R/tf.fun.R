#' Calculates Tailing factor of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column 1 from a csv file
#'
#' @return The TTP value for the peak
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3

tf.fun <- function(z) {
   
  fwhm <- defShit::fwhm.fun(z) 
  hrt <- defShit::hrt.fun(z)
  tf <- fwhm / ((fwhm - hrt) * 2)
  
  return(tf)
   
  }

