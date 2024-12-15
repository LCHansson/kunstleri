## Libs ----
library(tidyverse)
library(ggpattern)

## Func ----
generalized_logistic <- function(x, A = 1, k, x0, nu = 1, clean = TRUE) {
  # Note: Y values follow an inverted distribution, i.e. y == 1 | x == 0 and y == 0 | x == xmax
  cdf <- 1 - A / (1 + exp(-k * (x - x0)))^nu
  
  if (isTRUE(clean))
    cdf <- cdf[cdf >= 0]
  
  cdf
}

find_x0_in_glogis <- function(x, y, A = 1, k, nu = 1) {
  # Note: Y values follow an inverted distribution, i.e. y == 1 | x == 0 and y == 0 | x == xmax
  (log(A / (1 - y))^(1 / nu) - 1) / k + x
}

fib <- function(n) {
  if (n <= 1)
    return(1)
  else
    return(fib(n - 1) + fib(n - 2))
}

area_from_to <- function(k, A, nu, x_max, x0, from = 1, to = 100) {
  area_parts <- pmax(generalized_logistic(x = from:to, A = A, k = k, x0 = x0, nu = nu, clean = FALSE), 0)
  sum(area_parts)
}


## Model grid ----

best_models <- function(case, n = 20, pdist = TRUE) {
  # best_models <- function(longer_driving_distance, shorter_driving_distance, frequency_above_typical_range, battery_size, electricity_per_10km, n = 20) {
  longer_distance <- case$longer_driving_distance
  dist_at_p <- case$shorter_driving_distance
  p <- case$frequency_above_typical_range
  
  battery_capacity_km <- case$battery_size / (case$electricity_per_10km / 10) * case$total_home_charging
  
  test_grid_diffs <- expand_grid(
    nu = seq(0.25, 3, by = 0.25),
    A = seq(0.5, 3, by = 0.25),
    logis_scale = map_dbl(1:10, fib),
    
    # actual_x0 = point$actual_x0,
    # actual_x_at_p80 = point$actual_p80,
    # actual_x_at_p90 = point$actual_p90,
    # actual_p95 = point$actual_p95,
    # actual_p99 = point$actual_p99,
    
    # k, in this example, is the same as the proportion of days with longer driving range (p)
    # p = 1 - 1 / logis_scale,
    # k = k,
    x_start = 1,
    x_user = dist_at_p,
    x_max = longer_distance,
    p = p,
    rescaled_p = 1 - p/0.4,
    # color = color,
    battery_capacity_km = battery_capacity_km
  ) |>
    mutate(
      k = 1 / logis_scale,
      x0_by_x_user = find_x0_in_glogis(x_user, p, A = A, k = k, nu = nu),
      # x0_by_x_max_with_standard_assumptions = find_x0_in_glogis(x_max, 1, A = 1, k = k, nu = 1),
      y_zero_pred_x_user = generalized_logistic(1, A = A, k = k, x0 = x0_by_x_user, nu = nu, clean = FALSE),
      y_user_pred_x_user = generalized_logistic(x_user, A = A, k = k, x0 = x0_by_x_user, nu = nu, clean = FALSE),
      y_max_pred_x_user = generalized_logistic(x_max, A = A, k = k, x0 = x0_by_x_user, nu = nu, clean = FALSE),
      
      y_zero_pred_x_user_deviation = abs(y_zero_pred_x_user - 1),
      y_user_pred_x_user_deviation = abs(y_user_pred_x_user - p),
      y_max_pred_x_user_deviation = abs(y_max_pred_x_user - 0),
      # x_user_pred_deviation = ,
      # max_pred_deviation = 
      
      # max_pred_xmax = generalized_logistic(x_max, A = A, k = k, x0 = x0_by_x_max_with_standard_assumptions, nu = nu, clean = FALSE),
      # zero_pred_xmax = generalized_logistic(x = 1, A = A, k = k, x0 = x0_by_x_max_with_standard_assumptions, nu = nu, clean = FALSE)
      mean_deviation = (y_zero_pred_x_user_deviation + y_user_pred_x_user_deviation + y_max_pred_x_user_deviation) / 3,
      weighted_deviation = (y_zero_pred_x_user_deviation + y_user_pred_x_user_deviation * 6 + y_max_pred_x_user_deviation) / 8,
    ) |> 
    filter(
      # Deviation tolerances
      y_zero_pred_x_user_deviation <= 0.05,
      y_max_pred_x_user_deviation <= 0.05
      # # y_user_pred_x_user_deviation |> between(0.95, 1.05)
      # if_all(everything(), ~!is.na(.x))
    ) |>
    arrange(weighted_deviation) |>
    slice(1:n) |> 
    mutate(
      # k_weighted_similarity_to_p = abs(k^(x_max / dist_at_p) * 2 - rescaled_p^2)
      k_weighted_similarity_to_p = abs(k^(1/3) - rescaled_p^2)
    ) |> 
    arrange(k_weighted_similarity_to_p) |> 
    # arrange(desc(k_similarity_to_p)) |> 
    mutate(rownum = row_number())
  
  
  # dists <- test_grid_diffs
  dists <- test_grid_diffs |>
    # dists_1 |>
    rowwise() |>
    mutate(
      area_total = integrate(generalized_logistic, lower = 1, upper = x_max, k = k, nu = nu, A = A, x0 = x0_by_x_user, clean = FALSE)$value,
      area_below_x_user = integrate(generalized_logistic, lower = 1, upper = x_user, k = k, nu = nu, A = A, x0 = x0_by_x_user, clean = FALSE)$value,
      area_above_x_user = area_total - area_below_x_user,
      area_below_battery = integrate(generalized_logistic, lower = 1, upper = min(x_max, battery_capacity_km), k = k, nu = nu, A = A, x0 = x0_by_x_user, clean = FALSE)$value,
      area_above_battery = area_total - area_below_battery
      # area_total = area_from_to(k = k, nu = nu, A = A, x_max = x_max, x0 = x0_by_x_user, from = 1, to = x_max),
      # area_below_x_user = area_from_to(k = k, nu = nu, A = A, x_max = x_max, x0 = x0_by_x_user, from = 1, to = x_user),
      # area_above_x_user = area_from_to(k = k, nu = nu, A = A, x_max = x_max, x0 = x0_by_x_user, from = (x_user + 1), to = x_max),
      # area_below_battery = area_from_to(k = k, nu = nu, A = A, x_max = x_max, x0 = x0_by_x_user, from = 1, to = battery_capacity_km),
      # area_above_battery = area_from_to(k = k, nu = nu, A = A, x_max = x_max, x0 = x0_by_x_user, from = (battery_capacity_km + 1), to = x_max)
    ) |> 
    ungroup()
  
  if (pdist == TRUE) {
    dists <- dists |>
      rowwise() |>
      mutate(
        pdist = list(tibble(
          x = 1:x_max,
          y_pred = pmax(generalized_logistic(1:x_max, A = A, k = k, x0 = x0_by_x_user, nu = nu, clean = FALSE), 0)
        ))
      ) |>
      ungroup()
  }
  
  case$dists <- dists
  
  case
  # dists
}


