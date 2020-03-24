#' Calculates peak asymmetry
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column 1 from a csv file
#'
#' @return The TTP value for the peak
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3

af.fun <- function(z) {
   
  fwhm <- defShit::fwhm.fun(z) 
  hrt <- defShit::hrt.fun(z)
  af <- hrt / (fwhm - hrt)
  
  return(af)
   
  }

