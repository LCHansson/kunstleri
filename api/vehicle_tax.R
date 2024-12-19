vehicle_tax_db <- read_csv2("api/data/skattedatabas fordon.csv") |> 
  mutate(
    minvikt = as.integer(str_remove_all(minvikt, " ")),
    maxvikt = as.integer(str_remove_all(maxvikt, " ")),
    belopp = as.integer(str_remove_all(belopp, " ")),
    axlar = as.integer(str_remove_all(axlar, " "))
  )


vehicle_tax <- function(case) {
  if (case$vehicle_class == "van") {
    # Light truck/van
    if (case$ice_rollout_year < lubridate::year(Sys.Date()) - 3) {
      annual_malus <- 0
    } else {
      # Source: https://www.transportstyrelsen.se/sv/vagtrafik/fordon/skatter-och-avgifter/bonus-malus/malus/
      
      # Simplified assumption (ignoring 2022/01/01 - 2022/05/31)
      
      basic_fee <- 360
      
      co2_cutoff <- 75
      co2_price_rate <- if_else(case$ice_co2_value |> between(co2_cutoff, 125), 107, 132)
      co2_fee <- co2_price_rate * case$ice_co2_value
      
      diesel_environment_fee <- 250
      
      diesel_fuel_price_rate <- 13.52
      diesel_fuel_fee <- diesel_fuel_price_rate * case$ice_co2_value
      
      annual_malus <- if_else(
        case$fuel_type == "diesel",
        basic_fee + co2_fee + diesel_environment_fee + diesel_fuel_fee,
        basic_fee + co2_fee
      )
    }
    
    vehicle_weight <- 3500
    
    ice_annual_tax <- vehicle_tax_db |> 
      filter(
        fordonsklass == "lätt",
        drivmedel == case$fuel_type,
        minvikt <= vehicle_weight
      ) |>
      slice_tail(n = 1) |> 
      magrittr::extract2("belopp")
    bev_annual_tax <- vehicle_tax_db |> 
      filter(
        fordonsklass == "lätt",
        drivmedel == "ej diesel",
        minvikt <= vehicle_weight
      ) |>
      slice_tail(n = 1) |> 
      magrittr::extract2("belopp")
     
    ice_years_with_malus <- max(case$ice_rollout_year + 3 - lubridate::year(Sys.Date()), 0)
    ice_years_with_annual_tax <- case$vehicle_service_life - ice_years_with_malus
    
    case$taxes <- list(
      ice = case$vehicle_service_life * ice_annual_tax,
      bev = case$vehicle_service_life * bev_annual_tax
    )
    
    return(case)
    
  } else {
    # Heavy truck
    ice_annual_tax <- vehicle_tax_db |> 
      filter(
        fordonsklass == "tung",
        drivmedel == "diesel",
        axlar == min(case$num_axles, 4),
        draganordning == case$trailer_type,
        vägavgiftsplikt == case$road_toll_duty,
        minvikt <= case$ice_weight * 1000
      ) |> 
      slice_tail(n = 1) |> 
      magrittr::extract2("belopp")

    bev_annual_tax <- vehicle_tax_db |> 
      filter(
        fordonsklass == "tung",
        drivmedel == "ej diesel"
      ) |> 
      magrittr::extract2("belopp")
    
    # simplified assumption: ICE == Euro VI
    if (case$road_toll_duty == "ja") {
      road_toll <- if_else(case$num_axles <= 3, 8687, 14479)
    } else {
      road_toll <- 0
    }
    
    case$taxes <- list(
      ice = case$vehicle_service_life * (ice_annual_tax + road_toll),
      bev = case$vehicle_service_life * (bev_annual_tax + road_toll)
    )
    
    return(case)
  }
}