source("Assignment3_YieldFun.R")

#' Calculate profit with yield anomaly
#'
#' @param clim A data frame containing daily climate observations
#'   with columns: month, year, tmin_c (minimum temperature), precip (precipitation)
#' @param actual_yield numerical value representing baseline yield in tons/acre
#' @param price numerical value price of 1 ton of almonds
#' @param cost cost of planting (price of land, water, etc) per acre
#' @param acres number of acres planted
#' @param year water year
#'
#' @returns profit in dollar
#' @export
#'
#' @examples
profit <- function(clim, actual_yield = 1, price, cost, acres) {
  # Generate yearly yield anomalies from almond_yield function
  yearly_anomaly <- almond_yield(clim)
  
  # Calculate profit for each year
  profit_analysis <- yearly_anomaly %>%
    mutate(
      # Calculate actual yield
      total_yield = yield + actual_yield,
      
      # Calculate revenue 
      total_revenue = total_yield * acres * price,
      
      # Calculate cost of cultivation
      total_cost = cost * acres,
      
      # Subtract cost from revenue to get profit
      net_profit = total_revenue - total_cost
    )
  
  # return dataframe with profit variables
  return(profit_analysis)
}

