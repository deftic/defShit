#' Calculates TTP of peaks
#'
#' @param z A data.frame containing all peaks as columns and timepoints in column 1 from a csv file
#'
#' @return The TTP value for the peak
#' @export
#'
#' @examples
#' add_numbers(1, 2) ## returns 3

ttp.fun <- function(z) {
   
  xmax <- pracma::findpeaks(z$value, sortstr = TRUE)[1, 2] # timepoint with max value 
  loc.mins <- localMinima.fun(z$value) # store local minima
  ttp <- xmax - loc.mins[which.max(loc.mins[loc.mins < xmax])]  # time to peak
      
  return(ttp)
   
  }

