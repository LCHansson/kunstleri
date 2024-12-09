# case <- sensible_defaults
# case_inputs <- case_inputs_global
# case[names(case_inputs)] <- case_inputs




create_sim_grid <- function(case, simulation_options = list()) {
  # TODO:
  # - Support three (!) different battery sizes: optimal, maximum and offered
  # X Support battery electricity usage per km
  # - Support simulation options
  # - Support different modes of calculating battery cost (NOTE! How does battery pricing actually work? Find out!)

  
  
  ## Pre-processing ----
  
  # case$number_sims <- 10000
  case$number_sims <- 1
  
  # Only do pre-processing once per case
  case$number_of_operating_days <- case$vehicle_service_life * case$run_days_per_week * (52 - case$holiday_weeks_per_year)
  case$battery_cost <- (case$bev_truck_cost - case$diesel_truck_cost) / case$battery_size
  
  case$diesel_conversion_ratio <- case$diesel_per_10km / case$electricity_per_10km
  
  case$largest_battery_size <- case$electricity_per_10km / 10 * case$longer_driving_distance
  
  case$charger_utilisation <- case$night_charging_capacity
  
  # mil to km
  case$shorter_driving_distance <- case$shorter_driving_distance * 10
  case$longer_driving_distance <- case$longer_driving_distance * 10
  

  ## Simulation: Default options ----
  
  # default_options <- list(
  #   number_sims = case$number_sims,
  #   rpert_height = 6,
  #   # simulate_vars = list("private_charging_cost", "public_charging_cost", "diesel_cost", "shorter_driving_distance", "longer_driving_distance"),
  #   simulate_vars = list("private_charging_cost", "public_charging_cost", "diesel_cost"),
  #   simulate_opts = list(
  #     diesel_cost = list(low = case$diesel_price_span, mode = mean(case$diesel_price_span), high = case$diesel_price_span),
  #     private_charging_cost = list(low = case$private_charging_price_span, mode = mean(case$private_charging_price_span), high = case$private_charging_price_span),
  #     public_charging_cost = list(low = case$public_charging_price_span, mode = mean(case$public_charging_price_span), high = case$public_charging_price_span),
  #     shorter_driving_distance = list(low = case$shorter_driving_distance * 0.75, mode = case$shorter_driving_distance, high = case$shorter_driving_distance * 1.25),
  #     longer_driving_distance = list(low = case$longer_driving_distance * 0.75, mode = case$longer_driving_distance, high = case$longer_driving_distance * 1.25)
  #   )
  # )
  
  # sim_opts <- default_options
  # TODO: Iterate over supplied options here...
  
  
    
  ## Simulated variables ----
  
  # sim_grid <- map(sim_opts$simulate_vars, function(var) {
  #   var_opts <- sim_opts$simulate_opts[[var]]
  #   
  #   d_var <- mc2d::rpert(
  #     n = sim_opts$number_sims,
  #     min = var_opts$low,
  #     mode = var_opts$mode,
  #     max = var_opts$high,
  #     shape = sim_opts$rpert_height
  #   )
  #   
  #   tibble(!!var := d_var)
  # }) |> 
  #   list_cbind()
  
  
  
  ## Build sim grid ----
  
  all_vars <- c(
    "diesel_cost",
    "private_charging_cost",
    "public_charging_cost",
    "battery_cost",
    "charger_cost",
    "vehicle_service_life",
    "time_at_charger",
    "optimal_charging_rate",
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

