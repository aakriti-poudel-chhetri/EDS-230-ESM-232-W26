#' Almond Yield Anomaly Model
#'
#' Calculates yield anomalies for almonds based on monthly climate data
#'
#' @param clim A data frame containing daily climate observations
#'   with columns: month, year, tmin_c (minimum temperature), precip (precipitation)
#'
#' @return A list containing:
#'   - max_yield_anomaly: Maximum yield anomaly (ton/acre)
#'   - min_yield_anomaly: Minimum yield anomaly (ton/acre)
#'   - mean_yield_anomaly: Mean yield anomaly (ton/acre)
#'
#' @details 
#' The model uses monthly average minimum temperature and precipitation 
#' to calculate yield anomalies. Specifically:
#' - Minimum temperature from February
#' - Precipitation from January
#'
#' The yield anomaly is calculated using the equation from Lobell et al. (2006)
#'
#' @references 
#' Lobell, D. B., et al. (2006). Impacts of future climate change on 
#' California perennial crop yields. Agricultural and Forest Meteorology.
#'
#' @export
almond_yield <- function(clim) {
  # Calculate monthly means
  monthly <- clim %>% 
    group_by(month, year) %>% 
    summarise(
      mean_tmin = mean(tmin_c, na.rm = TRUE),
      sum_precip = sum(precip, na.rm = TRUE),
      .groups = "drop"  # Explicitly drop grouping
    )
  
  # Filter for specific months and calculate yield anomalies
  yield_anomalies <- monthly %>% 
    group_by(year) %>% 
    summarise(
      tmin = mean_tmin[month == 2],  # February minimum temperature
      precip = sum_precip[month == 1]  # January precipitation
    ) %>% 
    mutate(
      yield = (-0.015 * tmin) + 
        (-0.0046 * (tmin ^ 2)) + 
        (-0.07 * precip) + 
        (0.0043 * (precip ^ 2)) +
        0.28  
    )
  
  # Return summary statistics
  list(
    max_yield_anomaly = max(yield_anomalies$yield, na.rm = TRUE),
    min_yield_anomaly = min(yield_anomalies$yield, na.rm = TRUE),
    mean_yield_anomaly = mean(yield_anomalies$yield, na.rm = TRUE)
  )
}

library(tidyverse)

data <- re
