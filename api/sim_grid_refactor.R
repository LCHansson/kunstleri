# case <- sensible_defaults
# case_inputs <- case_inputs_global
# case[names(case_inputs)] <- case_inputs




create_sim_grid <- function(case) {
  
  ## Pre-processing ----
  
  # calculations
  case$number_of_operating_days <- case$vehicle_service_life * case$run_days_per_week * (52 - case$holiday_weeks_per_year)
  case$largest_battery_size <- case$electricity_per_10km / 10 * case$longer_driving_distance
  case$total_home_charging <- as.numeric(case$night_charging) + as.numeric(case$day_extra_home_charging)
  # case$battery_cost <- (case$bev_truck_cost - case$diesel_truck_cost) / case$battery_size
  
  # unit conversion
  case$diesel_conversion_ratio <- case$diesel_per_10km / case$electricity_per_10km
  case$shorter_driving_distance <- case$shorter_driving_distance * 10
  case$longer_driving_distance <- case$longer_driving_distance * 10
  
  
  
  ## Build sim grid ----
  
  all_vars <- c(
    "diesel_cost",
    "private_charging_cost",
    "public_charging_cost",
    "charger_cost",
    "vehicle_service_life",
    "number_of_operating_days",
    "shorter_driving_distance",
    "longer_driving_distance"
  )
  names(all_vars) <- all_vars
  
  sim_grid <- list()
  
  for (var in names(all_vars)) { sim_grid[[var]] <- case[[var]] }
  
  case$sim_grid <- sim_grid
  
  case
}

