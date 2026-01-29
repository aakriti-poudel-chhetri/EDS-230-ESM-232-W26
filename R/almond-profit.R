#' Compute the almond profit
#'
#' @param climate data with minimum and maximum temperature, and precipitation
#' @param baseline_yield baseline yield per acre (tons per acre) before anomaly adjustment
#' @param price price of almond sold per ton ($ per ton)
#' @param cost_per_acre total annual cost per acre ($ per acre)
#' @param acres total area coverage of the almond farm
#' @param price_inflation annual price inflation rate (default = 0)
#' @param discount discount rate for net present value (NPV) calculation (default = 0)
#'
#' @returns profit estimates from almond production
#' @export
#'
#' @examples
almond_profit <- function(climate, 
                          baseline_yield = 1.0, 
                          price = 3000,          
                          cost_per_acre = 2000, 
                          acres = 100,
                          price_inflation = 0, 
                          discount = 0) {
  
  # Calculate the estimated almond yield anomaly
  yield_result <- almond_yield(climate)
  
  # Calculate actual yield using baseline and anomaly
  actual_yield <- baseline_yield + yield_result$mean_yield_anomaly
  
  # Calculate revenue and profit
  revenue <- actual_yield * acres * price
  total_cost <- cost_per_acre * acres
  profit <- revenue - total_cost
  
  # Return results as a list
  return(list(
    yield_anomaly = yield_result$mean_yield_anomaly,
    actual_yield = actual_yield,
    revenue = revenue,
    total_cost = total_cost,
    profit = profit
  ))
}
