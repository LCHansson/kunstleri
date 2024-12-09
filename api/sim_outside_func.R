

## Sim template ----
# Single sim without case object manipulation

number_sims <- 100000
rpert_height <- 6


# lC_d <- list(low = 0.25, mode = 0.3, high = 0.4)
# lC_e <- list(low = 0.05, mode = 0.08, high = 0.2)
# lC_epub <- list(low = 0.2, mode = 0.4, high = 0.6)
# lC_b <- list(low = 120, mode = 200, high = 250)
# lC_ch <- list(low = 80, mode = 120, high = 200)


lC_d <- list(low = case$diesel_cost * 0.75, mode = case$diesel_cost, high = case$diesel_cost * 1.25)
lC_e <- list(low = case$private_charging_cost * 0.75, mode = case$private_charging_cost, high = case$private_charging_cost * 1.5)
lC_epub <- list(low = case$public_charging_cost * 0.75, mode = case$public_charging_cost, high = case$public_charging_cost * 1.25)
lC_b <- list(low = case$battery_cost * 0.75, mode = case$battery_cost, high = case$battery_cost * 1.25)
lC_ch <- list(low = case$charger_cost * 0.75, mode = case$charger_cost, high = case$charger_cost * 1.25)


dC_d <-     mc2d::rpert(n = number_sims, min = lC_d$low, mode = lC_d$mode, max = lC_d$high, shape = rpert_height)
dC_e <-     mc2d::rpert(n = number_sims, min = lC_e$low, mode = lC_e$mode, max = lC_e$high, shape = rpert_height)
dC_epub <-  mc2d::rpert(n = number_sims, min = lC_epub$low, mode = lC_epub$mode, max = lC_epub$high, shape = rpert_height)
dC_b <-     mc2d::rpert(n = number_sims, min = lC_b$low, mode = lC_b$mode, max = lC_b$high, shape = rpert_height)
dC_ch <-    mc2d::rpert(n = number_sims, min = lC_ch$low, mode = lC_ch$mode, max = lC_ch$high, shape = rpert_height)

# Test (run only if script non-sourced)
if (sys.nframe() == 0) {
  tibble(distrib = dC_e) %>%
    ggplot(aes(x = distrib)) +
    geom_bar(stat = "bin", bins = 50, color = "white") +
    labs(x = "Simulerad fördelning av kWh-kostnaden privat laddning i framtiden", y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
}



## Case simulations ----

# Ingen publik snabbladdning
case_grauers_nopublic       <- case_grauers_proto
case_grauers_blue_nopublic  <- case_grauers_blue_proto
case_grauers_green_nopublic <- case_grauers_green_proto
case_grauers_red_nopublic   <- case_grauers_red_proto
case_grauers_pink_nopublic  <- case_grauers_pink_proto



# "Klimatpremie"
case_grauers_klimatpremie       <- case_grauers_proto
case_grauers_blue_klimatpremie  <- case_grauers_blue_proto
case_grauers_green_klimatpremie <- case_grauers_green_proto
case_grauers_red_klimatpremie   <- case_grauers_red_proto
case_grauers_pink_klimatpremie  <- case_grauers_pink_proto

case_grauers_klimatpremie$battery_cost <- case_grauers_klimatpremie$battery_cost * 0.67
case_grauers_blue_klimatpremie$battery_cost <- case_grauers_blue_klimatpremie$battery_cost * 0.67
case_grauers_green_klimatpremie$battery_cost <- case_grauers_green_klimatpremie$battery_cost * 0.67
case_grauers_red_klimatpremie $battery_cost <- case_grauers_red_klimatpremie$battery_cost * 0.67
case_grauers_pink_klimatpremie$battery_cost <- case_grauers_pink_klimatpremie$battery_cost * 0.67


# "Klimatpremie" *och* ingen publik snabbladdning
case_grauers_klimatpremie_nopublic       <- case_grauers_proto
case_grauers_blue_klimatpremie_nopublic  <- case_grauers_blue_proto
case_grauers_green_klimatpremie_nopublic <- case_grauers_green_proto
case_grauers_red_klimatpremie_nopublic   <- case_grauers_red_proto
case_grauers_pink_klimatpremie_nopublic  <- case_grauers_pink_proto

case_grauers_klimatpremie_nopublic$battery_cost       <- case_grauers_klimatpremie_nopublic$battery_cost * 0.67
case_grauers_blue_klimatpremie_nopublic$battery_cost  <- case_grauers_blue_klimatpremie_nopublic$battery_cost * 0.67
case_grauers_green_klimatpremie_nopublic$battery_cost <- case_grauers_green_klimatpremie_nopublic$battery_cost * 0.67
case_grauers_red_klimatpremie_nopublic$battery_cost   <- case_grauers_red_klimatpremie_nopublic$battery_cost * 0.67
case_grauers_pink_klimatpremie_nopublic$battery_cost  <- case_grauers_pink_klimatpremie_nopublic$battery_cost * 0.67


# BARA publik snabbladdning men den kostar 4/3/2 kr/kWh plus klimatpremie
case_grauers_allpublic_40       <- case_grauers_proto
case_grauers_blue_allpublic_40  <- case_grauers_blue_proto
case_grauers_green_allpublic_40 <- case_grauers_green_proto
case_grauers_red_allpublic_40   <- case_grauers_red_proto
case_grauers_pink_allpublic_40  <- case_grauers_pink_proto

case_grauers_allpublic_40$public_charging_cost <- 4
case_grauers_blue_allpublic_40$public_charging_cost <- 4
case_grauers_green_allpublic_40$public_charging_cost <- 4
case_grauers_red_allpublic_40$public_charging_cost <- 4
case_grauers_pink_allpublic_40$public_charging_cost <- 4

case_grauers_allpublic_40$private_charging_cost       <- case_grauers_allpublic_40$public_charging_cost
case_grauers_blue_allpublic_40$private_charging_cost  <- case_grauers_blue_allpublic_40$public_charging_cost
case_grauers_green_allpublic_40$private_charging_cost <- case_grauers_green_allpublic_40$public_charging_cost
case_grauers_red_allpublic_40$private_charging_cost   <- case_grauers_red_allpublic_40$public_charging_cost
case_grauers_pink_allpublic_40$private_charging_cost  <- case_grauers_pink_allpublic_40$public_charging_cost

case_grauers_allpublic_40$charger_cost <- 0
case_grauers_blue_allpublic_40$charger_cost <- 0
case_grauers_green_allpublic_40$charger_cost <- 0
case_grauers_red_allpublic_40$charger_cost <- 0
case_grauers_pink_allpublic_40$charger_cost <- 0

case_grauers_allpublic_40$battery_cost <- case_grauers_allpublic_40$battery_cost * 0.67
case_grauers_blue_allpublic_40$battery_cost <- case_grauers_blue_allpublic_40$battery_cost * 0.67
case_grauers_green_allpublic_40$battery_cost <- case_grauers_green_allpublic_40$battery_cost * 0.67
case_grauers_red_allpublic_40$battery_cost <- case_grauers_red_allpublic_40$battery_cost * 0.67
case_grauers_pink_allpublic_40$battery_cost <- case_grauers_pink_allpublic_40$battery_cost * 0.67


case_grauers_allpublic_30       <- case_grauers_proto
case_grauers_blue_allpublic_30  <- case_grauers_blue_proto
case_grauers_green_allpublic_30 <- case_grauers_green_proto
case_grauers_red_allpublic_30   <- case_grauers_red_proto
case_grauers_pink_allpublic_30  <- case_grauers_pink_proto

case_grauers_allpublic_30$public_charging_cost <- 3
case_grauers_blue_allpublic_30$public_charging_cost <- 3
case_grauers_green_allpublic_30$public_charging_cost <- 3
case_grauers_red_allpublic_30$public_charging_cost <- 3
case_grauers_pink_allpublic_30$public_charging_cost <- 3

case_grauers_allpublic_30$private_charging_cost       <- case_grauers_allpublic_30$public_charging_cost
case_grauers_blue_allpublic_30$private_charging_cost  <- case_grauers_blue_allpublic_30$public_charging_cost
case_grauers_green_allpublic_30$private_charging_cost <- case_grauers_green_allpublic_30$public_charging_cost
case_grauers_red_allpublic_30$private_charging_cost   <- case_grauers_red_allpublic_30$public_charging_cost
case_grauers_pink_allpublic_30$private_charging_cost  <- case_grauers_pink_allpublic_30$public_charging_cost

case_grauers_allpublic_30$charger_cost <- 0
case_grauers_blue_allpublic_30$charger_cost <- 0
case_grauers_green_allpublic_30$charger_cost <- 0
case_grauers_red_allpublic_30$charger_cost <- 0
case_grauers_pink_allpublic_30$charger_cost <- 0

case_grauers_allpublic_30$battery_cost <- case_grauers_allpublic_30$battery_cost * 0.67
case_grauers_blue_allpublic_30$battery_cost <- case_grauers_blue_allpublic_30$battery_cost * 0.67
case_grauers_green_allpublic_30$battery_cost <- case_grauers_green_allpublic_30$battery_cost * 0.67
case_grauers_red_allpublic_30$battery_cost <- case_grauers_red_allpublic_30$battery_cost * 0.67
case_grauers_pink_allpublic_30$battery_cost <- case_grauers_pink_allpublic_30$battery_cost * 0.67


case_grauers_allpublic_20       <- case_grauers_proto
case_grauers_blue_allpublic_20  <- case_grauers_blue_proto
case_grauers_green_allpublic_20 <- case_grauers_green_proto
case_grauers_red_allpublic_20   <- case_grauers_red_proto
case_grauers_pink_allpublic_20  <- case_grauers_pink_proto

case_grauers_allpublic_20$public_charging_cost <- 2
case_grauers_blue_allpublic_20$public_charging_cost <- 2
case_grauers_green_allpublic_20$public_charging_cost <- 2
case_grauers_red_allpublic_20$public_charging_cost <- 2
case_grauers_pink_allpublic_20$public_charging_cost <- 2

case_grauers_allpublic_20$private_charging_cost       <- case_grauers_allpublic_20$public_charging_cost
case_grauers_blue_allpublic_20$private_charging_cost  <- case_grauers_blue_allpublic_20$public_charging_cost
case_grauers_green_allpublic_20$private_charging_cost <- case_grauers_green_allpublic_20$public_charging_cost
case_grauers_red_allpublic_20$private_charging_cost   <- case_grauers_red_allpublic_20$public_charging_cost
case_grauers_pink_allpublic_20$private_charging_cost  <- case_grauers_pink_allpublic_20$public_charging_cost

case_grauers_allpublic_20$charger_cost <- 0
case_grauers_blue_allpublic_20$charger_cost <- 0
case_grauers_green_allpublic_20$charger_cost <- 0
case_grauers_red_allpublic_20$charger_cost <- 0
case_grauers_pink_allpublic_20$charger_cost <- 0

case_grauers_allpublic_20$battery_cost <- case_grauers_allpublic_20$battery_cost * 0.67
case_grauers_blue_allpublic_20$battery_cost <- case_grauers_blue_allpublic_20$battery_cost * 0.67
case_grauers_green_allpublic_20$battery_cost <- case_grauers_green_allpublic_20$battery_cost * 0.67
case_grauers_red_allpublic_20$battery_cost <- case_grauers_red_allpublic_20$battery_cost * 0.67
case_grauers_pink_allpublic_20$battery_cost <- case_grauers_pink_allpublic_20$battery_cost * 0.67


case_grauers_allpublic_15       <- case_grauers_proto
case_grauers_blue_allpublic_15  <- case_grauers_blue_proto
case_grauers_green_allpublic_15 <- case_grauers_green_proto
case_grauers_red_allpublic_15   <- case_grauers_red_proto
case_grauers_pink_allpublic_15  <- case_grauers_pink_proto

case_grauers_allpublic_15$public_charging_cost <- 1.5
case_grauers_blue_allpublic_15$public_charging_cost <- 1.5
case_grauers_green_allpublic_15$public_charging_cost <- 1.5
case_grauers_red_allpublic_15$public_charging_cost <- 1.5
case_grauers_pink_allpublic_15$public_charging_cost <- 1.5

case_grauers_allpublic_15$private_charging_cost       <- case_grauers_allpublic_15$public_charging_cost
case_grauers_blue_allpublic_15$private_charging_cost  <- case_grauers_blue_allpublic_15$public_charging_cost
case_grauers_green_allpublic_15$private_charging_cost <- case_grauers_green_allpublic_15$public_charging_cost
case_grauers_red_allpublic_15$private_charging_cost   <- case_grauers_red_allpublic_15$public_charging_cost
case_grauers_pink_allpublic_15$private_charging_cost  <- case_grauers_pink_allpublic_15$public_charging_cost

case_grauers_allpublic_15$charger_cost <- 0
case_grauers_blue_allpublic_15$charger_cost <- 0
case_grauers_green_allpublic_15$charger_cost <- 0
case_grauers_red_allpublic_15$charger_cost <- 0
case_grauers_pink_allpublic_15$charger_cost <- 0

case_grauers_allpublic_15$battery_cost <- case_grauers_allpublic_15$battery_cost * 0.67
case_grauers_blue_allpublic_15$battery_cost <- case_grauers_blue_allpublic_15$battery_cost * 0.67
case_grauers_green_allpublic_15$battery_cost <- case_grauers_green_allpublic_15$battery_cost * 0.67
case_grauers_red_allpublic_15$battery_cost <- case_grauers_red_allpublic_15$battery_cost * 0.67
case_grauers_pink_allpublic_15$battery_cost <- case_grauers_pink_allpublic_15$battery_cost * 0.67



## Simuleringar
case_grauers       <- create_sim_grid(case_grauers_proto)
case_grauers_blue  <- create_sim_grid(case_grauers_blue_proto)
case_grauers_green <- create_sim_grid(case_grauers_green_proto)
case_grauers_red   <- create_sim_grid(case_grauers_red_proto)
case_grauers_pink  <- create_sim_grid(case_grauers_pink_proto)

case_grauers_nopublic       <- create_sim_grid(case_grauers_nopublic)
case_grauers_blue_nopublic  <- create_sim_grid(case_grauers_blue_nopublic)
case_grauers_green_nopublic <- create_sim_grid(case_grauers_green_nopublic)
case_grauers_red_nopublic   <- create_sim_grid(case_grauers_red_nopublic)
case_grauers_pink_nopublic  <- create_sim_grid(case_grauers_pink_nopublic)

case_grauers_klimatpremie       <- create_sim_grid(case_grauers_klimatpremie)
case_grauers_blue_klimatpremie  <- create_sim_grid(case_grauers_blue_klimatpremie)
case_grauers_green_klimatpremie <- create_sim_grid(case_grauers_green_klimatpremie)
case_grauers_red_klimatpremie   <- create_sim_grid(case_grauers_red_klimatpremie)
case_grauers_pink_klimatpremie  <- create_sim_grid(case_grauers_pink_klimatpremie)

case_grauers_klimatpremie_nopublic       <- create_sim_grid(case_grauers_klimatpremie_nopublic)
case_grauers_blue_klimatpremie_nopublic  <- create_sim_grid(case_grauers_blue_klimatpremie_nopublic)
case_grauers_green_klimatpremie_nopublic <- create_sim_grid(case_grauers_green_klimatpremie_nopublic)
case_grauers_red_klimatpremie_nopublic   <- create_sim_grid(case_grauers_red_klimatpremie_nopublic)
case_grauers_pink_klimatpremie_nopublic  <- create_sim_grid(case_grauers_pink_klimatpremie_nopublic)


case_grauers_allpublic_40       <- create_sim_grid(case_grauers_allpublic_40)
case_grauers_blue_allpublic_40  <- create_sim_grid(case_grauers_blue_allpublic_40)
case_grauers_green_allpublic_40 <- create_sim_grid(case_grauers_green_allpublic_40)
case_grauers_red_allpublic_40   <- create_sim_grid(case_grauers_red_allpublic_40)
case_grauers_pink_allpublic_40  <- create_sim_grid(case_grauers_pink_allpublic_40)

case_grauers_allpublic_30       <- create_sim_grid(case_grauers_allpublic_30)
case_grauers_blue_allpublic_30  <- create_sim_grid(case_grauers_blue_allpublic_30)
case_grauers_green_allpublic_30 <- create_sim_grid(case_grauers_green_allpublic_30)
case_grauers_red_allpublic_30   <- create_sim_grid(case_grauers_red_allpublic_30)
case_grauers_pink_allpublic_30  <- create_sim_grid(case_grauers_pink_allpublic_30)

case_grauers_allpublic_20       <- create_sim_grid(case_grauers_allpublic_20)
case_grauers_blue_allpublic_20  <- create_sim_grid(case_grauers_blue_allpublic_20)
case_grauers_green_allpublic_20 <- create_sim_grid(case_grauers_green_allpublic_20)
case_grauers_red_allpublic_20   <- create_sim_grid(case_grauers_red_allpublic_20)
case_grauers_pink_allpublic_20  <- create_sim_grid(case_grauers_pink_allpublic_20)

case_grauers_allpublic_15       <- create_sim_grid(case_grauers_allpublic_15)
case_grauers_blue_allpublic_15  <- create_sim_grid(case_grauers_blue_allpublic_15)
case_grauers_green_allpublic_15 <- create_sim_grid(case_grauers_green_allpublic_15)
case_grauers_red_allpublic_15   <- create_sim_grid(case_grauers_red_allpublic_15)
case_grauers_pink_allpublic_15  <- create_sim_grid(case_grauers_pink_allpublic_15)


## Resultat

# Punktestimat av lönsamhet
tribble(
  ~scenario, ~variant, ~is_profitable_point_estimate,
  "grauers", "default", case_grauers$is_profitable_point_estimate,
  "grauers", "klimatpremie", case_grauers_klimatpremie$is_profitable_point_estimate,
  "grauers", "nopublic", case_grauers_nopublic$is_profitable_point_estimate,
  "grauers", "klimatpremie_nopublic", case_grauers_klimatpremie_nopublic$is_profitable_point_estimate,
  "grauers blue", "default", case_grauers_blue$is_profitable_point_estimate,
  "grauers blue", "klimatpremie", case_grauers_blue_klimatpremie$is_profitable_point_estimate,
  "grauers blue", "nopublic", case_grauers_blue_nopublic$is_profitable_point_estimate,
  "grauers blue", "klimatpremie_nopublic", case_grauers_blue_klimatpremie_nopublic$is_profitable_point_estimate,
  "grauers green", "default", case_grauers_green$is_profitable_point_estimate,
  "grauers green", "klimatpremie", case_grauers_green_klimatpremie$is_profitable_point_estimate,
  "grauers green", "nopublic", case_grauers_green_nopublic$is_profitable_point_estimate,
  "grauers green", "klimatpremie_nopublic", case_grauers_green_klimatpremie_nopublic$is_profitable_point_estimate,
  "grauers red", "default", case_grauers_red$is_profitable_point_estimate,
  "grauers red", "klimatpremie", case_grauers_red_klimatpremie$is_profitable_point_estimate,
  "grauers red", "nopublic", case_grauers_red_nopublic$is_profitable_point_estimate,
  "grauers red", "klimatpremie_nopublic", case_grauers_red_klimatpremie_nopublic$is_profitable_point_estimate,
  "grauers pink", "default", case_grauers_pink$is_profitable_point_estimate,
  "grauers pink", "klimatpremie", case_grauers_pink_klimatpremie$is_profitable_point_estimate,
  "grauers pink", "nopublic", case_grauers_pink_nopublic$is_profitable_point_estimate,
  "grauers pink", "klimatpremie_nopublic", case_grauers_pink_klimatpremie_nopublic$is_profitable_point_estimate
) %>% 
  pivot_wider(names_from = scenario, values_from = is_profitable_point_estimate) %>% 
  rename("Is profitable point estimate" = variant)



# Punktestimat av kWh-kostnad
tribble(
  ~scenario, ~variant, ~f_BEV_point_estimate,
  "grauers", "default", case_grauers$f_BEV_point_estimate,
  "grauers", "klimatpremie", case_grauers_klimatpremie$f_BEV_point_estimate,
  "grauers", "nopublic", case_grauers_nopublic$f_BEV_point_estimate,
  "grauers", "klimatpremie_nopublic", case_grauers_klimatpremie_nopublic$f_BEV_point_estimate,
  "grauers blue", "default", case_grauers_blue$f_BEV_point_estimate,
  "grauers blue", "klimatpremie", case_grauers_blue_klimatpremie$f_BEV_point_estimate,
  "grauers blue", "nopublic", case_grauers_blue_nopublic$f_BEV_point_estimate,
  "grauers blue", "klimatpremie_nopublic", case_grauers_blue_klimatpremie_nopublic$f_BEV_point_estimate,
  "grauers green", "default", case_grauers_green$f_BEV_point_estimate,
  "grauers green", "klimatpremie", case_grauers_green_klimatpremie$f_BEV_point_estimate,
  "grauers green", "nopublic", case_grauers_green_nopublic$f_BEV_point_estimate,
  "grauers green", "klimatpremie_nopublic", case_grauers_green_klimatpremie_nopublic$f_BEV_point_estimate,
  "grauers red", "default", case_grauers_red$f_BEV_point_estimate,
  "grauers red", "klimatpremie", case_grauers_red_klimatpremie$f_BEV_point_estimate,
  "grauers red", "nopublic", case_grauers_red_nopublic$f_BEV_point_estimate,
  "grauers red", "klimatpremie_nopublic", case_grauers_red_klimatpremie_nopublic$f_BEV_point_estimate,
  "grauers pink", "default", case_grauers_pink$f_BEV_point_estimate,
  "grauers pink", "klimatpremie", case_grauers_pink_klimatpremie$f_BEV_point_estimate,
  "grauers pink", "nopublic", case_grauers_pink_nopublic$f_BEV_point_estimate,
  "grauers pink", "klimatpremie_nopublic", case_grauers_pink_klimatpremie_nopublic$f_BEV_point_estimate
) %>% 
  mutate(f_BEV_point_estimate = dec(f_BEV_point_estimate)) %>% 
  pivot_wider(names_from = scenario, values_from = f_BEV_point_estimate) %>% 
  rename("f_BEV point estimate" = variant)


# Genomsnittlig kWh-kostnad i scenario
tribble(
  ~scenario, ~variant, ~f_BEV_mean_estimate,
  "grauers", "default", mean(case_grauers$sim_grid$f_BEV),
  "grauers", "klimatpremie", mean(case_grauers_klimatpremie$sim_grid$f_BEV),
  "grauers", "nopublic", mean(case_grauers_nopublic$sim_grid$f_BEV_larger),
  "grauers", "klimatpremie_nopublic", mean(case_grauers_klimatpremie_nopublic$sim_grid$f_BEV_larger),
  "grauers blue", "default", mean(case_grauers_blue$sim_grid$f_BEV),
  "grauers blue", "klimatpremie", mean(case_grauers_blue_klimatpremie$sim_grid$f_BEV),
  "grauers blue", "nopublic", mean(case_grauers_blue_nopublic$sim_grid$f_BEV_larger),
  "grauers blue", "klimatpremie_nopublic", mean(case_grauers_blue_klimatpremie_nopublic$sim_grid$f_BEV_larger),
  "grauers green", "default", mean(case_grauers_green$sim_grid$f_BEV),
  "grauers green", "klimatpremie", mean(case_grauers_green_klimatpremie$sim_grid$f_BEV),
  "grauers green", "nopublic", mean(case_grauers_green_nopublic$sim_grid$f_BEV_larger),
  "grauers green", "klimatpremie_nopublic", mean(case_grauers_green_klimatpremie_nopublic$sim_grid$f_BEV_larger),
  "grauers red", "default", mean(case_grauers_red$sim_grid$f_BEV),
  "grauers red", "klimatpremie", mean(case_grauers_red_klimatpremie$sim_grid$f_BEV),
  "grauers red", "nopublic", mean(case_grauers_red_nopublic$sim_grid$f_BEV_larger),
  "grauers red", "klimatpremie_nopublic", mean(case_grauers_red_klimatpremie_nopublic$sim_grid$f_BEV_larger),
  "grauers pink", "default", mean(case_grauers_pink$sim_grid$f_BEV),
  "grauers pink", "klimatpremie", mean(case_grauers_pink_klimatpremie$sim_grid$f_BEV),
  "grauers pink", "nopublic", mean(case_grauers_pink_nopublic$sim_grid$f_BEV_larger),
  "grauers pink", "klimatpremie_nopublic", mean(case_grauers_pink_klimatpremie_nopublic$sim_grid$f_BEV_larger)
) %>% 
  mutate(f_BEV_mean_estimate = dec(f_BEV_mean_estimate)) %>% 
  pivot_wider(names_from = scenario, values_from = f_BEV_mean_estimate) %>% 
  rename("mean F_BEV sim" = variant)


# Batterirekommendation
tribble(
  ~scenario, ~variant, ~battery_recommendation,
  "grauers", "default", case_grauers$recommended_battery,
  "grauers", "klimatpremie", case_grauers_klimatpremie$recommended_battery,
  "grauers", "nopublic", case_grauers_nopublic$recommended_battery,
  "grauers", "klimatpremie_nopublic", case_grauers_klimatpremie_nopublic$recommended_battery,
  "grauers blue", "default", case_grauers_blue$recommended_battery,
  "grauers blue", "klimatpremie", case_grauers_blue_klimatpremie$recommended_battery,
  "grauers blue", "nopublic", case_grauers_blue_nopublic$recommended_battery,
  "grauers blue", "klimatpremie_nopublic", case_grauers_blue_klimatpremie_nopublic$recommended_battery,
  "grauers green", "default", case_grauers_green$recommended_battery,
  "grauers green", "klimatpremie", case_grauers_green_klimatpremie$recommended_battery,
  "grauers green", "nopublic", case_grauers_green_nopublic$recommended_battery,
  "grauers green", "klimatpremie_nopublic", case_grauers_green_klimatpremie_nopublic$recommended_battery,
  "grauers red", "default", case_grauers_red$recommended_battery,
  "grauers red", "klimatpremie", case_grauers_red_klimatpremie$recommended_battery,
  "grauers red", "nopublic", case_grauers_red_nopublic$recommended_battery,
  "grauers red", "klimatpremie_nopublic", case_grauers_red_klimatpremie_nopublic$recommended_battery,
  "grauers pink", "default", case_grauers_pink$recommended_battery,
  "grauers pink", "klimatpremie", case_grauers_pink_klimatpremie$recommended_battery,
  "grauers pink", "nopublic", case_grauers_pink_nopublic$recommended_battery,
  "grauers pink", "klimatpremie_nopublic", case_grauers_pink_klimatpremie_nopublic$recommended_battery
) %>% 
  pivot_wider(names_from = scenario, values_from = battery_recommendation) %>% 
  rename("Mean sim battery recommendation" = variant)


# Andel lönsamma
tribble(
  ~scenario, ~variant, ~share_profitable,
  "grauers", "default", case_grauers$share_profitable, 
  "grauers", "klimatpremie", case_grauers_klimatpremie$share_profitable, 
  "grauers", "nopublic", case_grauers_nopublic$larger_share_profitable, 
  "grauers", "klimatpremie_nopublic", case_grauers_klimatpremie_nopublic$larger_share_profitable, 
  "grauers blue", "default", case_grauers_blue$share_profitable, 
  "grauers blue", "klimatpremie", case_grauers_blue_klimatpremie$share_profitable, 
  "grauers blue", "nopublic", case_grauers_blue_nopublic$larger_share_profitable, 
  "grauers blue", "klimatpremie_nopublic", case_grauers_blue_klimatpremie_nopublic$larger_share_profitable, 
  "grauers green", "default", case_grauers_green$share_profitable, 
  "grauers green", "klimatpremie", case_grauers_green_klimatpremie$share_profitable, 
  "grauers green", "nopublic", case_grauers_green_nopublic$larger_share_profitable, 
  "grauers green", "klimatpremie_nopublic", case_grauers_green_klimatpremie_nopublic$larger_share_profitable, 
  "grauers red", "default", case_grauers_red$share_profitable, 
  "grauers red", "klimatpremie", case_grauers_red_klimatpremie$share_profitable, 
  "grauers red", "nopublic", case_grauers_red_nopublic$larger_share_profitable, 
  "grauers red", "klimatpremie_nopublic", case_grauers_red_klimatpremie_nopublic$larger_share_profitable, 
  "grauers pink", "default", case_grauers_pink$share_profitable, 
  "grauers pink", "klimatpremie", case_grauers_pink_klimatpremie$share_profitable, 
  "grauers pink", "nopublic", case_grauers_pink_nopublic$larger_share_profitable, 
  "grauers pink", "klimatpremie_nopublic", case_grauers_pink_klimatpremie_nopublic$larger_share_profitable
) %>% 
  mutate(share_profitable = perc(share_profitable)) %>% 
  pivot_wider(names_from = scenario, values_from = share_profitable) %>% 
  rename("Share sim profitable" = variant)



tribble(
  ~scenario, ~variant, ~share_profitable,
  "grauers", "default", case_grauers$share_profitable, 
  "grauers", "allpublic_40", case_grauers_allpublic_40$share_profitable, 
  "grauers", "allpublic_30", case_grauers_allpublic_30$share_profitable, 
  "grauers", "allpublic_20", case_grauers_allpublic_20$share_profitable, 
  "grauers", "allpublic_15", case_grauers_allpublic_15$share_profitable, 
  "grauers blue", "default", case_grauers_blue$share_profitable, 
  "grauers blue", "allpublic_40", case_grauers_blue_allpublic_40$share_profitable, 
  "grauers blue", "allpublic_30", case_grauers_blue_allpublic_30$share_profitable, 
  "grauers blue", "allpublic_20", case_grauers_blue_allpublic_20$share_profitable, 
  "grauers blue", "allpublic_15", case_grauers_blue_allpublic_15$share_profitable, 
  "grauers green", "default", case_grauers_green$share_profitable, 
  "grauers green", "allpublic_40", case_grauers_green_allpublic_40$share_profitable, 
  "grauers green", "allpublic_30", case_grauers_green_allpublic_30$share_profitable, 
  "grauers green", "allpublic_20", case_grauers_green_allpublic_20$share_profitable, 
  "grauers green", "allpublic_15", case_grauers_green_allpublic_15$share_profitable, 
  "grauers red", "default", case_grauers_red$share_profitable, 
  "grauers red", "allpublic_40", case_grauers_red_allpublic_40$share_profitable, 
  "grauers red", "allpublic_30", case_grauers_red_allpublic_30$share_profitable, 
  "grauers red", "allpublic_20", case_grauers_red_allpublic_20$share_profitable, 
  "grauers red", "allpublic_15", case_grauers_red_allpublic_15$share_profitable, 
  "grauers pink", "default", case_grauers_pink$share_profitable, 
  "grauers pink", "allpublic_40", case_grauers_pink_allpublic_40$share_profitable, 
  "grauers pink", "allpublic_30", case_grauers_pink_allpublic_30$share_profitable, 
  "grauers pink", "allpublic_20", case_grauers_pink_allpublic_20$share_profitable, 
  "grauers pink", "allpublic_15", case_grauers_pink_allpublic_15$share_profitable, 
) %>% 
  mutate(share_profitable = perc(share_profitable)) %>% 
  pivot_wider(names_from = scenario, values_from = share_profitable) %>% 
  rename("Share sim profitable" = variant)
