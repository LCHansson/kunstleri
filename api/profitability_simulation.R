
profitability_simulation <- function(case) {
  ## Check prereqs
  if (is.null(case$sim_grid) || is.null(case$dists))
    stop()
  
  
  ## Select Model ----
  
  case$dist_model <- case$dists[1,] |> as.list()
  
  
  ## Calculate totals ----
  
  case$sim_grid$total_private_electricity_consumption = case$dist_model$area_below_battery * case$number_of_operating_days * case$electricity_per_10km / 10
  case$sim_grid$total_public_electricity_consumption = case$dist_model$area_above_battery * case$number_of_operating_days * case$electricity_per_10km / 10
  case$sim_grid$total_private_electricity_cost = case$sim_grid$total_private_electricity_consumption * case$private_charging_cost
  case$sim_grid$total_public_electricity_cost = case$sim_grid$total_public_electricity_consumption * case$public_charging_cost
  case$sim_grid$total_diesel_consumption = case$dist_model$area_total * case$number_of_operating_days * case$diesel_per_10km / 10
  case$sim_grid$total_diesel_cost = case$sim_grid$total_diesel_consumption * case$diesel_cost
  
  
  ## Charging infrastructure ----
  if (isTRUE(case$opts$include_charger)) {
    charger_added_cost <- list(ice = 0, bev = (case$charger_cost + case$grid_cost) / case$charger_sharing_n)
  } else {
    charger_added_cost <- list(ice = 0, bev = 0)
  }
  
  
  ## Taxes and levies ----
  if (isTRUE(case$opts$include_taxes)) {
    taxes_added_cost <- case$taxes
  } else {
    taxes_added_cost <- list(ice = 0, bev = 0)
  }
  
  ## Maintenance ----
  if (isTRUE(case$opts$include_service)) {
    maintenance_added_cost <- list(
      ice = case$ice_service_cost * case$dist_model$area_total * case$number_of_operating_days / 10,
      bev = case$bev_service_cost * case$dist_model$area_total * case$number_of_operating_days / 10
    )
  } else {
    maintenance_added_cost <- list(ice = 0, bev = 0)
  }
  
  ## Tires ----
  if (isTRUE(case$opts$include_tires)) {
    tires_added_cost <- list(
      ice = case$ice_tire_cost * case$dist_model$area_total * case$number_of_operating_days / 10,
      bev = case$ice_tire_cost * case$dist_model$area_total * case$number_of_operating_days / 10 * (1 + case$bev_tire_increase)
    )
  } else {
    tires_added_cost <- list(ice = 0, bev = 0)
  }
  
  
  
  ## Profitability ----
  
  case$sim_grid$additional_costs <- list(
    charger_added_cost = charger_added_cost,
    taxes_added_cost = taxes_added_cost,
    maintenance_added_cost = maintenance_added_cost,
    tires_added_cost = tires_added_cost
  )
  
  additional_ice_costs <- charger_added_cost$ice + taxes_added_cost$ice + maintenance_added_cost$ice + tires_added_cost$ice
  additional_bev_costs <- charger_added_cost$bev + taxes_added_cost$bev + maintenance_added_cost$bev + tires_added_cost$bev
  
  case$sim_grid$bev_total_tco = case$bev_truck_cost - case$bev_climate_premium +
    case$sim_grid$total_private_electricity_cost +
    case$sim_grid$total_public_electricity_cost +
    additional_bev_costs
  case$sim_grid$ice_total_tco = case$diesel_truck_cost + 
    case$sim_grid$total_diesel_cost +
    additional_ice_costs
  
  case$sim_grid$bev_is_profitable = case$sim_grid$bev_total_tco < case$sim_grid$ice_total_tco
  
  case
}
