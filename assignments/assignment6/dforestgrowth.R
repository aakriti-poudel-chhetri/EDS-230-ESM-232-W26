#' Forest growth model as a differential equation
#' @param Time time since start
#' @param C forest size in units of carbon (kgC)
#' @param parms - list with four values:
#' @param r exponential growth rate (before canopy closure)
#' @param g linear growth rate (after canopy closure)
#' @param K carrying capacity (kgC)
#' @param closure canopy closure threshold (kgC)
#' @return derivative of forest size (C) with time

dforestgrowth <- function(Time, C, parms) {
  
  if (C < parms$closure) {
    # Exponential growth (below canopy closure threshold)
    dC <- parms$r * C
  } else {
    # Linear growth toward carrying capacity (at or above canopy closure threshold)
    dC <- parms$g * (1 - C / parms$K)
  }
  
  return(list(dC))
}
