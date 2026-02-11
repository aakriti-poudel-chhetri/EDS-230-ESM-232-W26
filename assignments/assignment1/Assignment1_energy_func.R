#' Compute annual energy production from a photovoltaic system
#'
#' @param A Solar panel area in square meters (m^2)
#' @param H Annual average solar radiation (kWh/m^2)
#' @param r Panel yield, default is 0.20
#' @param PR Performance ratio accounting for site-specific losses, default is 0.75
#'
#' @return Estimated annual energy production in kilowatt-hours (kWh)
#'
#' @examples
#' # Example: 35 m^2 panel area with 1400 kWh/m^2 annual radiation
#' energy(A = 35, H = 1400)
#'
#' @export
energy <- function(A, H, r = 0.2, PR = 0.75) {
  # calculate annual energy production
  E <- A * r * H * PR
  return(E)
}

