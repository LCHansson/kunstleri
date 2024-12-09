

## Create sim grid ----
create_sim_grid <- function(case, simulation_options = list()) {
  # TODO:
  # - Support three (!) different battery sizes: optimal, maximum and offered
  # - Support battery electricity usage per km
  # - Support simulation options
  
  
  ## Pre-processing ----
  
  case$number_of_operating_days <- case$vehicle_service_life * case$run_days_per_week * (52 - case$holiday_weeks_per_year)
  case$battery_cost <- (case$bev_truck_cost - case$diesel_truck_cost) / case$battery_size
  
  case$diesel_conversion_ratio <- case$diesel_per_100km / case$electricity_per_100km
  case$diesel_kwh_cost <- case$diesel_cost * case$diesel_conversion_ratio
  
  case$larger_battery_size <- case$electricity_per_100km / 100 * case$longer_driving_distance
  
  # Only do pre-processing once per case
  # if (is.null(case$r)) case$r = case$shorter_driving_distance / case$longer_driving_distance
  # if (is.null(case$M)) case$M = case$share_days_with_longer_distance * case$number_of_operating_days
  
  # if (is.null(case$plot_M_opt)) {
  #   plot_M_opt <- find_M_opt(
  #     C_b = case$battery_cost,
  #     r_SoC = case$optimal_charging_rate,
  #     Tm = case$vehicle_service_life,
  #     T_ch = case$time_at_charger,
  #     C_ch = case$charger_cost,
  #     C_epub = case$public_charging_cost,
  #     C_e = case$private_charging_cost
  #   )
  
  # case$plot_M_opt <- plot_M_opt
  # }
  
  
  ## Simluation options ----
  
  default_options <- list(
    number_sims = 100000,
    rpert_height = 6,
    simulate_vars = list("C_e", "C_epub", "C_d", "shorter_driving_distance", "longer_driving_distance"),
    # simulate_vars = list("C_e", "C_epub", "C_d"),
    simulate_opts = list(
      C_d = list(low = case$diesel_kwh_cost * 0.75, mode = case$diesel_kwh_cost, high = case$diesel_kwh_cost * 1.5),
      C_e = list(low = case$private_charging_cost * 0.75, mode = case$private_charging_cost, high = case$private_charging_cost * 1.5),
      C_epub = list(low = case$public_charging_cost * 0.75, mode = case$public_charging_cost, high = case$public_charging_cost * 1.5),
      shorter_driving_distance = list(low = case$shorter_driving_distance * 0.75, mode = case$shorter_driving_distance, high = case$shorter_driving_distance * 1.25),
      longer_driving_distance = list(low = case$longer_driving_distance * 0.75, mode = case$longer_driving_distance, high = case$longer_driving_distance * 1.25)
    )
  )
  
  sim_opts <- default_options
  # TODO: Iterate over supplied options here...
  
  
  ## Point estimate ----
  # NOTE: Do we even need this? A point estimate could be achieved by just setting sim_opts$simulate_vars to character(0)
  
  # case$f_BEV_point_estimate <- if_else(
  #   case$M > case$plot_M_opt, 
  #   f_BEV_bi_largerbattery(
  #     M = case$M, r = case$r,
  #     C_e = case$private_charging_cost, C_b = case$battery_cost, C_ch = case$charger_cost,
  #     N_op = case$number_of_operating_days, Tm = case$vehicle_service_life, T_ch = case$time_at_charger
  #   ),
  #   f_BEV_bi_smallerbattery(
  #     M = case$M, r = case$r,
  #     C_e = case$private_charging_cost, C_b = case$battery_cost, C_ch = case$charger_cost, C_epub = case$public_charging_cost,
  #     N_op = case$number_of_operating_days, Tm = case$vehicle_service_life, T_ch = case$time_at_charger
  #   )
  # )
  # case$is_profitable_point_estimate <- if_else(case$f_BEV_point_estimate < case$diesel_kwh_cost, TRUE, FALSE)
  
  
  ## Simulation ----
  map(sim_opts$simulate_vars, function(var) {
    distrib <- list(mc2d::rpert(
      n = sim_opts$number_sims,
      min = sim_opts$simulate_opts[[var]]$low,
      mode = sim_opts$simulate_opts[[var]]$mode,
      max = sim_opts$simulate_opts[[var]]$high,
      shape = sim_opts$rpert_height
    ))
    names(distrib) <- var
    distrib
  }) |> 
    bind_cols()
  
  
  
  # dC_d <- mc2d::rpert(
  #   n = sim_opts$number_sims,
  #   min = sim_opts$simulate_opts$C_d$low,
  #   mode = sim_opts$simulate_opts$C_d$mode,
  #   max = sim_opts$simulate_opts$C_d$high,
  #   shape = sim_opts$rpert_height
  # )
  # dC_e <-     mc2d::rpert(
  #   n = sim_opts$number_sims,
  #   min = sim_opts$simulate_opts$C_e$low,
  #   mode = sim_opts$simulate_opts$C_e$mode,
  #   max = sim_opts$simulate_opts$C_e$high,
  #   shape = sim_opts$rpert_height
  # )
  # dC_epub <-  mc2d::rpert(
  #   n = sim_opts$number_sims,
  #   min = sim_opts$simulate_opts$C_epub$low,
  #   mode = sim_opts$simulate_opts$C_epub$mode,
  #   max = sim_opts$simulate_opts$C_epub$high,
  #   shape = sim_opts$rpert_height
  # )
  # dC_b <-     mc2d::rpert(
  #   n = number_sims,
  #   min = lC_b$low,
  #   mode = lC_b$mode,
  #   max = lC_b$high,
  #   shape = rpert_height
  # )
  # dC_ch <-    mc2d::rpert(
  #   n = number_sims,
  #   min = lC_ch$low,
  #   mode = lC_ch$mode,
  #   max = lC_ch$high,
  #   shape = rpert_height
  # )
  
  
  ## Simulation analysis ----
  sim_grid <- tibble(
    n = 1:sim_opts$number_sims,
    C_d = dC_d,
    C_e = dC_e,
    C_epub = dC_epub,
    C_b = dC_b,
    C_ch = dC_ch
  ) %>% 
    mutate(
      M_opt = find_M_opt(
        r_SoC = case$optimal_charging_rate, Tm = case$vehicle_service_life, T_ch = case$time_at_charger,
        C_ch = C_ch, C_e = C_e, C_epub = C_epub, C_b = C_b),
      M_opt = if_else(M_opt > case$number_of_operating_days, rep(case$number_of_operating_days, length(M_opt)), M_opt),
      M_opt = if_else(M_opt < 1, rep(1, length(M_opt)), M_opt),
      
      f_BEV_larger = f_BEV_bi_largerbattery(
        M = case$M, r = case$r,
        C_e = C_e, C_b = C_b, C_ch = C_ch,
        N_op = case$number_of_operating_days, Tm = case$vehicle_service_life, T_ch = case$time_at_charger
      ),
      f_BEV_smaller = f_BEV_bi_smallerbattery(
        M = case$M, r = case$r,
        C_e = C_e, C_b = C_b, C_ch = C_ch, C_epub = C_epub,
        N_op = case$number_of_operating_days, Tm = case$vehicle_service_life, T_ch = case$time_at_charger
      ),
      # f_BEV_offered = if_else(),
      battery_size_recommendation = if_else(case$M > M_opt, "larger", "smaller"),
      
      contrib_C_e = case$r * C_e,
      contrib_C_epub = (1 - case$r) * C_epub,
      larger_is_profitable = if_else(f_BEV_larger < C_d, 1, 0),
      smaller_is_profitable = if_else(f_BEV_smaller < C_d, 1, 0)
    )
  
  
  case$smaller_battery_recommendation_share <- sum(sim_grid$battery_size_recommendation == "smaller") / nrow(sim_grid)
  case$larger_battery_recommendation_share <- 1 - case$smaller_battery_recommendation_share
  case$recommended_battery <- if_else(case$smaller_battery_recommendation_share >= 0.5, "smaller", "larger")
  
  sim_grid$f_BEV <- if (case$recommended_battery == "smaller") sim_grid$f_BEV_smaller else sim_grid$f_BEV_larger
  sim_grid$is_profitable <- if_else(sim_grid$f_BEV < sim_grid$C_d, 1, 0)
  
  case$share_profitable <- sum(sim_grid$is_profitable) / nrow(sim_grid)
  case$larger_share_profitable <- sum(sim_grid$larger_is_profitable) / nrow(sim_grid)
  case$smaller_share_profitable <- sum(sim_grid$smaller_is_profitable) / nrow(sim_grid)
  
  case$sim_grid <- sim_grid
  
  return(case)
}