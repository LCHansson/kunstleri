## Formatting ----
perc <- scales::label_percent(accuracy = 0.1, suffix = " %")
perc_t <- scales::label_percent(accuracy = 1, suffix = " %")
perc_5 <- function(x) {
  if (x < 0.05)
    return("mindre än 5 %")
  if (x > 0.95)
    return("över 95 %")
  
  scales::number(
    x,
    accuracy = 1, scale = 100, prefix = "", 
    suffix =" %", big.mark = " ", decimal.mark = ".", 
    trim = TRUE
  )
}
tsep <- scales::label_comma(accuracy = 100000, big.mark = " ", decimal.mark = ",")
tsep_t <- scales::label_comma(accuracy = 1000, big.mark = " ", decimal.mark = ",")
tsep_h <- scales::label_comma(accuracy = 100, big.mark = " ", decimal.mark = ",")


## Data conversion ----
case_list_to_json <- function(case_list) {
  jsonlite::toJSON(case_list)
}

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}


## Fuel price conversions ----
diesel_kWh_conversion_factor <- 1/4

fuel_price_l_to_kwh <- function(price, fuel_type = "diesel", currency = "sek") {
  # Source: https://www.sciencedirect.com/topics/engineering/diesel-fuel
  density <- case_match(
    fuel_type,
    "diesel" ~ 0.85,
    "gasoline" ~ 0.7,
    "bensin" ~ 0.7,
    .default = 0.85
  )
  
  # Source: https://www.quora.com/How-much-energy-in-kWh-does-diesel-or-petrol-produce/answer/Andrew-Forrest-40
  propulsion_energy <- case_match(
    fuel_type,
    "diesel" ~ 5,
    "gasoline" ~ 3.8,
    "bensin" ~ 3.8,
    .default = 5
  )
  
  exchange_rate <- case_match(
    currency,
    "sek" ~ 11.5,
    "eur" ~ 1L,
    "usd" ~ 0.9,
    .default = 11.5
  )
  
  price / exchange_rate * density / propulsion_energy
}

fuel_price_kwh_to_l <- function(kwh, fuel_type = "diesel", currency = "sek") {
  # Source: https://www.sciencedirect.com/topics/engineering/diesel-fuel
  density <- case_match(
    fuel_type,
    "diesel" ~ 0.85,
    "gasoline" ~ 0.7,
    "bensin" ~ 0.7,
    .default = 0.85
  )
  
  # Source: https://www.quora.com/How-much-energy-in-kWh-does-diesel-or-petrol-produce/answer/Andrew-Forrest-40
  propulsion_energy <- case_match(
    fuel_type,
    "diesel" ~ 5,
    "gasoline" ~ 3.8,
    "bensin" ~ 3.8,
    .default = 5
  )
  
  exchange_rate <- case_match(
    currency,
    "sek" ~ 11.5, 
    "eur" ~ 1L,
    "usd" ~ 0.9,
    .default = 11.5
  )
  
  kwh * ( propulsion_energy * exchange_rate ) / density
}


## Calculate optimal M ----
pC_d <- 0.3 # Diesel cost (€/kWh)
pC_e <- 0.08 # Electricity cost for private charging (€/kWh)
pC_epub <- 0.4 # Electricity cost for public fast charging (€/kWh)
pC_b <- 200 # Cost of battery (€/kWh)
pC_charger <- 400 # Price of charger (€/kW)
pC_g <- 60 # Annual cost of grid (€/kW)
pTm <- 7 # Vehicle service life
pC_ch = pC_charger / pTm + pC_g # Total annual cost of charger (€/kW)

pr_SoC <- 0.8
pN_op <- 1750
pT_ch <- 14

find_M_opt <- function(C_b = pC_b, r_SoC = pr_SoC, Tm = pTm, T_ch = pT_ch, C_ch = pC_ch, C_epub = pC_epub, C_e = pC_e, N_op = pN_op) {
  (C_b / r_SoC + Tm / T_ch * C_ch) / (C_epub - C_e)
  
}


## Cost function for a bimodal EDD ----
f_BEV_bi_smallerbattery <- function(M, r, C_e = pC_e, C_epub = pC_epub, C_b = pC_b, r_SoC = pr_SoC, N_op = pN_op, Tm = pTm, T_ch = pT_ch, C_ch = pC_ch) {
  r_ch = r * N_op / (r * N_op + M * (1 - r))
  
  r_ch * C_e +
    (1 - r_ch) * C_epub +
    r_ch * C_b / (r_SoC * N_op) +
    r_ch * Tm / (N_op * T_ch) * C_ch
}

f_BEV_bi_largerbattery <- function(M, r, C_e = pC_e, C_b = pC_b, r_SoC = pr_SoC, N_op = pN_op, Tm = pTm, T_ch = pT_ch, C_ch = pC_ch) {
  C_e +
    C_b / (r_SoC * (r * N_op + M * (1 - r))) +
    Tm / ((r * N_op + M * (1 - r)) * T_ch) * C_ch
}


## Diesel cost line for stacked area chart ----
cutoff_line_belowMopt <- function(
    M, # r = f(M, ...)
    C_d = pC_d, # parameter to draw the line by
    C_e = pC_e,
    C_epub = 5 * C_e,
    C_b = pC_b,
    r_SoC = pr_SoC,
    N_op = pN_op,
    Tm = pTm,
    T_ch = pT_ch,
    C_ch = pC_ch
) {
  C_ediff = C_epub - C_e
  
  C_stationary = C_b / (r_SoC * N_op) + Tm / (N_op * T_ch) * C_ch
  
  r = M / ( 1 / ( ( C_epub - C_d ) / ( ( C_ediff - C_stationary ) * N_op ) ) + M - N_op )
  r[!is.finite(r)] <- 0
  r[r > 1] <- 1
  r[r < 0] <- 0
  r
}

cutoff_line_aboveMopt <- function(
    M, # r = f(M, ...)
    C_d = pC_d, # parameter to draw the line by
    C_e = pC_e,
    C_epub = 5 * C_e,
    C_b = pC_b,
    r_SoC = pr_SoC,
    N_op = pN_op,
    Tm = pTm,
    T_ch = pT_ch,
    C_ch = pC_ch
) {
  r = ( M - ( C_b / r_SoC + Tm / (T_ch) * C_ch ) / ( C_d - C_e ) ) / (M - N_op)
  r[!is.finite(r)] <- 0
  r[r > 1] <- 1
  r[r < 0] <- 0
  r
}




## Parameter distribution simulation ----
# lC_d <- list(
#   low = 0.25, mode = 0.3, high = 0.40
# )
# lC_e <- list(low = , mode = , high = )
# lC_epub <- list(low = , mode = , high = )
# lC_b <- list(low = , mode = , high = )
# lC_ch <- list(low = , mode = , high = )

## Create plottable data for the original stacked area chart ----
generate_plot_data <- function(case) {
  
  cost_steps <- case$cost_steps
  
  M <- 1:case$number_of_operating_days
  r0 <- rep(1, length(M))
  r <- cost_steps %>%
    map(function(cost_steps, M, case) {
      if_else(
        M < plot_M_opt,
        cutoff_line_belowMopt(
          # M, 0.26 * 11.5,
          M, as.numeric(cost_steps),
          C_e = case$private_charging_cost, C_epub = case$public_charging_cost,
          C_b = case$battery_cost, r_SoC = case$optimal_charging_rate, N_op = case$number_of_operating_days,
          Tm = case$vehicle_service_life, T_ch = case$time_at_charger, C_ch = case$charger_cost
        ),
        cutoff_line_aboveMopt(
          M, as.numeric(cost_steps),
          C_e = case$private_charging_cost, C_epub = case$public_charging_cost,
          C_b = case$battery_cost, r_SoC = case$optimal_charging_rate, N_op = case$number_of_operating_days,
          Tm = case$vehicle_service_life, T_ch = case$time_at_charger, C_ch = case$charger_cost
        )
      )
    }, M, case)
  
  rC_d <- if_else(
    M < plot_M_opt,
    cutoff_line_belowMopt(
      M, case$diesel_cost,
      C_e = case$private_charging_cost, C_epub = case$public_charging_cost,
      C_b = case$battery_cost, r_SoC = case$optimal_charging_rate, N_op = case$number_of_operating_days,
      Tm = case$vehicle_service_life, T_ch = case$time_at_charger, C_ch = case$charger_cost
    ),
    cutoff_line_aboveMopt(
      M, case$diesel_cost,
      C_e = case$private_charging_cost, C_epub = case$public_charging_cost,
      C_b = case$battery_cost, r_SoC = case$optimal_charging_rate, N_op = case$number_of_operating_days,
      Tm = case$vehicle_service_life, T_ch = case$time_at_charger, C_ch = case$charger_cost
    )
  )
  
  plot_data <- suppressMessages(bind_cols(r)) %>% 
    set_names(cost_steps) %>%
    mutate(`0` = r0) %>% 
    bind_cols(M = M, rC_d = rC_d) %>% 
    pivot_longer(!M, values_to = "r")
  
  # Jordens namn-fulhack. Fixa efter Transportforum!
  rC_d_data <- plot_data %>% filter(name == "rC_d")
  res <- plot_data %>%
    filter(name != "rC_d") %>% 
    mutate(
      cs = name,
      cs_index = map(name, \(x) which(cost_steps == x)) %>% unlist(),
      cs2 = c(cost_steps, "+")[cs_index + 1],
      name2 = paste(cs, "-", cs2),
      name2 = if_else(cs == cost_steps[length(cost_steps)], paste(cost_steps[length(cost_steps)], "+"), name2)
    ) %>% 
    mutate(
      name = factor(name2),
      name = fct_relevel(name, function(x) {
        put_last = which(str_detect(x, "\\+"))
        last_label = x[put_last]
        x <- c(x[-put_last], last_label)
        x
      })
    ) %>% 
    select(M, name, r)
  
  list(plot_data = res, diesel_line_data = rC_d_data)
}
