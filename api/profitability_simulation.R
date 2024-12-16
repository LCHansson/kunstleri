
profitability_simulation <- function(case, optional_params) {
  ## Check prereqs
  if (is.null(case$sim_grid) || is.null(case$dists))
    stop()
  
  
  ## Select Model ----
  
  case$dist_model <- case$dists[1,] |> as.list()
  
  
  ## Calculate totals ----
  if (all(c("include_private_charging", "include_public_charging") %in% case$charge_modes)) {
    # Both charging modes (standard case)
    case$sim_grid$total_private_electricity_consumption = case$dist_model$area_below_battery * case$number_of_operating_days * case$electricity_per_10km / 10
    case$sim_grid$total_public_electricity_consumption = case$dist_model$area_above_battery * case$number_of_operating_days * case$electricity_per_10km / 10
    case$sim_grid$total_private_electricity_cost = case$sim_grid$total_private_electricity_consumption * case$private_charging_cost
    case$sim_grid$total_public_electricity_cost = case$sim_grid$total_public_electricity_consumption * case$public_charging_cost
    case$sim_grid$total_diesel_consumption = case$dist_model$area_total * case$number_of_operating_days * case$diesel_per_10km / 10
    case$sim_grid$total_diesel_cost = case$sim_grid$total_diesel_consumption * case$diesel_cost
  } else if ("include_private_charging" %in% case$charge_modes) {
    # Only private charging
    # NOTE: Should display warning
    case$sim_grid$total_private_electricity_consumption = case$dist_model$area_below_battery * case$sim_grid$number_of_operating_days * case$electricity_per_10km / 10
    case$sim_grid$total_public_electricity_consumption = 0
    case$sim_grid$total_private_electricity_cost = case$sim_grid$total_private_electricity_consumption * case$private_charging_cost
    case$sim_grid$total_public_electricity_cost = case$sim_grid$total_public_electricity_consumption * case$public_charging_cost
    case$sim_grid$total_diesel_consumption = case$dist_model$area_below_battery * case$number_of_operating_days * case$diesel_per_10km / 10
    case$sim_grid$total_diesel_cost = case$sim_grid$total_diesel_consumption * case$diesel_cost
  } else if ("include_public_charging" %in% case$charge_modes) {
    # Only public charging
    case$sim_grid$total_private_electricity_consumption = 0
    case$sim_grid$total_public_electricity_consumption = case$dist_model$area_total * case$number_of_operating_days * case$electricity_per_10km / 10
    case$sim_grid$total_private_electricity_cost = case$sim_grid$total_private_electricity_consumption * case$private_charging_cost
    case$sim_grid$total_public_electricity_cost = case$sim_grid$total_public_electricity_consumption * case$public_charging_cost
    case$sim_grid$total_diesel_consumption = case$dist_model$area_total * case$number_of_operating_days * case$diesel_per_10km / 10
    case$sim_grid$total_diesel_cost = case$sim_grid$total_diesel_consumption * case$diesel_cost
  }
  # No charging? Should display warning
  
  
  ## Optional parameters ----
  
  ## Charging infrastructure ----
  
  
  
  ## Profitability ----
  
  additional_ice_costs <- 0
  additional_bev_costs <- 0
  
  case$sim_grid$bev_total_tco = case$bev_truck_cost - case$bev_climate_premium +
    (case$charger_cost + case$grid_cost) / case$charger_sharing_n +
    case$sim_grid$total_private_electricity_cost +
    case$sim_grid$total_public_electricity_cost +
    additional_bev_costs
  case$sim_grid$ice_total_tco = case$diesel_truck_cost + 
    case$sim_grid$total_diesel_cost +
    additional_ice_costs
  
  case$sim_grid$bev_is_profitable = case$sim_grid$bev_total_tco < case$sim_grid$ice_total_tco
  
  case
}
