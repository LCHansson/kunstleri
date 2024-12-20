## Libs ----
library(shiny)
library(shinyjs)
library(shinyvalidate)

## Helpers ----
# App helpers
source("api/sensible_defaults.R")
source("api/func.R")

# Case simulation
source("api/sim_grid_refactor.R")
source("api/model_selection.R")
source("api/profitability_simulation.R")
source("api/vehicle_tax.R")

# Scoring
source("api/profitability.R")

# Predefined scenarios
source("api/predefined_cases.R")


## Initialise placeholder ----
scoring_func <- cost_comparison_categories
ui_is_visible <<- FALSE
case_global <<- NULL

## App ----

server <- function(input, output, session) {
  
  iv <- InputValidator$new()
  
  iv$add_rule("p_diesel_truck_cost", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_diesel_truck_cost", sv_integer())
  iv$add_rule("p_bev_truck_cost", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_bev_truck_cost", sv_integer())
  iv$add_rule("p_bev_climate_premium", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_bev_climate_premium", sv_integer())
  iv$add_rule("p_battery_size", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_battery_size", sv_integer())
  iv$add_rule("p_shorter_driving_distance", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_shorter_driving_distance", sv_numeric())
  iv$add_rule("p_longer_driving_distance", sv_required("Fältet måste innehålla ett siffervärde"))
  iv$add_rule("p_longer_driving_distance", sv_numeric())

    
  ## Empty state ----
  
  observe({
    input$run_sim_button
    
    isolate({
      if (input$run_sim_button > 0)
        iv$enable()
      
      
      if (input$run_sim_button > 0) {
        
        # if(ValidateInputs()) {
        shinyjs::hide("empty-state")
        shinyjs::show("result")
        shinyjs::show("sidebar-scenarios")
        shinyjs::hide("run_sim_button")
        ui_is_visible <<- TRUE
        # }
        message("ui_is_visible == ", ui_is_visible)
      } else
        message("ui_is_visible == ", ui_is_visible)
    })
  })
  
  
  ## Vehicle class switch ----
  observe({
    input$p_vehicle_class
    
    isolate({
      
      if (input$p_vehicle_class == "van") {
        show("ice_rollout_year")
        show("fuel_type")
        show("ice_co2_value")
        hide("ice_weight")
        hide("num_axles")
        hide("trailer_type")
        hide("road_toll_duty")
      } else {
        hide("ice_rollout_year")
        hide("fuel_type")
        hide("ice_co2_value")
        show("ice_weight")
        show("num_axles")
        show("trailer_type")
        show("road_toll_duty")
      }
    })
  })
  
  
  ## Case data ----
  
  CaseInputs <- reactive({
    input$run_sim_button
    
    # isolate({
    all_inputs <- reactiveValuesToList(input)
    all_inputs$p_charge_modes <- input$p_charge_modes
    test_cbgi <<- input$p_charge_modes
    
    all_inputs <- all_inputs[order(names(all_inputs))]
    # x <- x |> purrr::discard(stringr::str_detect(names(x), "button"))
    params <- all_inputs |> purrr::keep(stringr::str_detect(names(all_inputs), "^p_"))
    names(params) <- stringr::str_remove(names(params), "^p_")
    
    params$opts <- all_inputs |> purrr::keep(stringr::str_detect(names(all_inputs), "^o_"))
    names(params$opts) <- stringr::str_remove(names(params$opts), "^o_")
    # })
    
    params
  })
  
  ValidateInputs <- reactive({
    
    primary_inputs <- c(
      "p_diesel_truck_cost",
      "p_bev_truck_cost",
      "p_bev_climate_premium",
      "p_battery_size",
      "p_shorter_driving_distance",
      "p_longer_driving_distance",
      "p_frequency_above_typical_range",
      "p_charge_modes"
    )
    
    input_vals <- map(primary_inputs, function(i) input[[i]])
    
    if (any(is.na(input_vals)) || any(is.null(input_vals)))
      return(FALSE)
    else
      return(TRUE)
  })
  
  CaseData <- reactive({
    # input$run_sim_button
    
    message("ValidateInputs() = ", ValidateInputs())
    
    if (ValidateInputs()) {
      
      # isolate({
      ## Get case params
      case <- sensible_defaults
      
      case_inputs <- CaseInputs()
      
      # Pre-processing to deal with selectInput() returning character instead of numeric
      case_inputs$frequency_above_typical_range <- as.numeric(case_inputs$frequency_above_typical_range)
      case_inputs$night_charging <- as.numeric(case_inputs$night_charging)
      case_inputs$day_extra_home_charging <- as.numeric(case_inputs$day_extra_home_charging)
      case_inputs$bev_tire_increase <- as.numeric(case_inputs$bev_tire_increase)
      
      case_inputs_global <<- case_inputs
      
      case[names(case_inputs)] <- case_inputs
      
      ## Do sim
      case <- create_sim_grid(case)
      case <- best_models(case)
      case <- vehicle_tax(case)
      case <- profitability_simulation(case)
      
      case_global <<- case
      
      case$warning <- 1
      # })
      return(case)
    } else {
      return(case_global)
    }
    
  })
  
  BarHeights <- reactive({
    case <- CaseData()
    if (is.null(case))
      return(NULL)
    
    # total_height <- 200
    
    # conf_interval <- c(lower = 0.05, higher = 0.95)
    
    max_bar_height <- max(case$sim_grid$bev_total_tco, case$sim_grid$ice_total_tco)
    
    ice_height_shares <- c(
      vehicle = case$diesel_truck_cost,
      fuel = case$sim_grid$total_diesel_cost,
      taxes = case$sim_grid$additional_costs$taxes_added_cost$ice,
      maintenance = case$sim_grid$additional_costs$maintenance_added_cost$ice,
      tires = case$sim_grid$additional_costs$tires_added_cost$ice
    ) / max_bar_height * 100
    
    bev_height_shares <- c(
      vehicle = case$bev_truck_cost - case$bev_climate_premium,
      private_charging = case$sim_grid$total_private_electricity_cost,
      public_charging = case$sim_grid$total_public_electricity_cost,
      charging_infrastructure = case$sim_grid$additional_costs$charger_added_cost$bev,
      taxes = case$sim_grid$additional_costs$taxes_added_cost$bev,
      maintenance = case$sim_grid$additional_costs$maintenance_added_cost$bev,
      tires = case$sim_grid$additional_costs$tires_added_cost$bev
    ) / max_bar_height * 100
    
    
    return(
      list(
        ice_height_shares = ice_height_shares,
        bev_height_shares = bev_height_shares
      )
    )
  })
  
  
  CaseCoverage <- reactive({
    case <- CaseData()
    
    coverage <- list(
      battery_coverage_of_total = case$dist_model$area_below_battery / case$dist_model$area_total,
      battery_coverage_of_typical_day = case$dist_model$battery_capacity_km / case$shorter_driving_distance,
      battery_overcapacity_energy = (case$dist_model$battery_capacity_km - case$dist_model$area_below_battery) * case$number_of_operating_days,
      battery_overcapacity_share = (case$dist_model$battery_capacity_km - case$dist_model$area_below_battery) / case$dist_model$battery_capacity_km
    )
    
    coverage$battery_coverage_of_total_is_low <- coverage$battery_coverage_of_total < 0.9
    coverage$battery_coverage_of_typical_day_is_low <- coverage$battery_coverage_of_typical_day < 0.95
    coverage$battery_overcapacity_share_is_low <- coverage$battery_overcapacity_share > 0.1
    
    coverage
  })
  
  
  
  ## HTML graphs and UI components ----
  
  output$tco_comparison_bars_ui <- renderUI({
    case <- CaseData()
    if (is.null(case)) return(NULL)
    div_heights <- BarHeights()
    div_heights_global <<- div_heights
    
    div(
      # Hela stapeldiagrammet
      class = "tco-graph",
      div(
        # En av staplarna med etiketter
        class = "tco-bar",
        div(
          # Stapeln
          class = "tco-components",
          div(
            class = "tco-component tco-component-diesel",
            style = glue::glue("height: {div_heights$ice_height_shares[['fuel']]}%;"),
            div(
              class = "tco-component-label right",
              style = if (div_heights$ice_height_shares[['fuel']] == 0) "display: none;" else "",
              "Diesel"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights$ice_height_shares[['vehicle']]}%;"),
            div(
              class =  "tco-component-shape"
            ),
            div(
              class = "tco-component-label right",
              "Fordon"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights$ice_height_shares[['taxes']]}%;"),
            div(
              class = "tco-component-label right",
              style = if (div_heights$ice_height_shares[['taxes']] == 0) "display: none;" else "",
              "Skatt och avgifter"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights$ice_height_shares[['maintenance']]}%;"),
            div(
              class = "tco-component-label right",
              style = if (div_heights$ice_height_shares[['maintenance']] == 0) "display: none;" else "",
              "Service"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights$ice_height_shares[['tires']]}%;"),
            div(
              class = "tco-component-label right",
              style = if (div_heights$ice_height_shares[['tires']] == 0) "display: none;" else "",
              "Däck"
            )
          )
        ),
        div(
          # Stor etikett under stapeln
          class = "tco-bar-label",
          "Dieselbil"
        ),
        div(
          # Kostnadsintervall under stora etiketten
          class = "tco-bar-cost-span",
          glue::glue("{tsep(case$sim_grid$ice_total_tco)} kr")
        )
      ),
      div(
        # En av staplarna med etiketter
        class = "tco-bar",
        div(
          # Stapeln
          class = "tco-components",
          div(
            class = "tco-component tco-component-public-charging",
            style = if (div_heights$bev_height_shares[['public_charging']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['public_charging']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['public_charging']] == 0) "display: none;" else "",
              "Snabbladdning"
            )
          ),
          div(
            class = "tco-component tco-component-private-charging",
            style = if (div_heights$bev_height_shares[['private_charging']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['private_charging']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['private_charging']] == 0) "display: none;" else "",
              "Depåladdning"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights$bev_height_shares[['vehicle']]}%;"),
            div(
              class =  "tco-component-shape"
            ),
            div(
              class = "tco-component-label left",
              "Fordon"
            )
          ),
          div(
            class = "tco-component tco-component-charger",
            style = if (div_heights$bev_height_shares[['charging_infrastructure']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['charging_infrastructure']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['charging_infrastructure']] == 0) "display: none;" else "",
              "Laddinfra"
            )
          ),
          div(
            class = "tco-component tco-component-charger",
            style = if (div_heights$bev_height_shares[['taxes']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['taxes']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['taxes']] == 0) "display: none;" else "",
              "Skatt och avgifter"
            )
          ),
          div(
            class = "tco-component tco-component-charger",
            style = if (div_heights$bev_height_shares[['maintenance']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['maintenance']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['maintenance']] == 0) "display: none;" else "",
              "Service"
            )
          ),
          div(
            class = "tco-component tco-component-charger",
            style = if (div_heights$bev_height_shares[['tires']] != 0) {
              glue::glue("height: {div_heights$bev_height_shares[['tires']]}%;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if (div_heights$bev_height_shares[['tires']] == 0) "display: none;" else "",
              "Däck"
            )
          )
        ),
        div(
          # Stor etikett under stapeln
          class = "tco-bar-label",
          "Elbil"
        ),
        div(
          # Kostnadsintervall under stora etiketten
          class = "tco-bar-cost-span",
          glue::glue("{tsep(case$sim_grid$bev_total_tco)} kr")
        )
      )
    )
  })
  
  
  output$bev_cost_analysis <- renderUI({
    coverage <- CaseCoverage()
    
    div(
      class = "tco-graph",
      div(
        class = "cost-driver-coverage-explanation",
        div(
          class = "cost-driver-coverage-component",
          tags$p(
            tags$strong(
              class = "text-coverage",
              "Leveransförmåga en typisk dag"
            ),
            tooltip(
              bs_icon("question-circle"),
              "Kan du köra bilen hela dagen på den el du laddar på natten?
              En elbil är oftast inte lönsam om du behöver stödladda alltför mycket. Därför bör
              bilens batteri i normalfallet kunna täcka 100% av körsträckan en
              \"vanlig dag på jobet\"."
            )
          ),
          tags$p(
            "Ett fulladdat batteri kan leverera ungefär",
            glue::glue("{perc_t(coverage$battery_coverage_of_typical_day)}"),
            "av den el som krävs för att köra bilen en \"vanlig dag på jobbet\"."
          ),
          (if (coverage$battery_coverage_of_typical_day_is_low) { 
            tags$p(
              class = "cost-warning",
              bs_icon("exclamation-circle", class = "warning"),
              tags$em("Överväg att beställa ett större batteri till bilen.
                      Merkostnaden för ett större batteri kan vara väsentligt
                      mindre än kostnaden för att stödladda bilen.")
            )
          })
        ),
        div(
          class = "cost-driver-coverage-component",
          tags$p(
            tags$strong(
              class = "text-coverage",
              "Batteriets överkapacitet"),
            popover(
              bs_icon("question-circle"),
              "Om du har väldigt ojämna körmönster, eller om batteriet har större
              kapacitet än vad du behöver för att köra bilen en \"vanlig dag
              på jobbet\", kommer bilen att ha energi kvar i bilen när du kommer in
              i depå på kvällen. Då kan det vara kostnadseffektivt att överväga att
              antingen köpa en bil med ett mindre batteri för att få ned ekipagekostnaden,
              eller att se över möjligheten att sälja tillbaka el till nätet som
              \"peak shaving\"-el eller frekvensutjämning."
            )
          ),
          tags$p(
            "Batteriet beräknas ha outnyttjad kapacitet motsvarande ",
            glue::glue("{tsep_h(coverage$battery_overcapacity_energy)} kWh"),
            "vilket motsvarar",
            glue::glue("{perc_t(coverage$battery_overcapacity_share)}"),
            "av den totala potentiella batteriladdningen över bilens livstid."
          ),
          (if (coverage$battery_overcapacity_share_is_low) { 
            tags$p(
              class = "cost-warning",
              bs_icon("exclamation-circle", class = "warning"),
              tags$em("Undersök möjligheten att sälja tillbaka el till nätet för att få ned kostnaden
                      för elbilen. Ett alternativ är att överväga att köpa en bil med ett mindre batteri.
                      Minskningen i inköpskostnad kan vara väsentligt större än kostnaden för att stödladda
                      vissa dagar.")
            )
          })
        ),
        div(
          class = "cost-driver-coverage-component",
          tags$p(
            tags$strong(
              class = "text-coverage",
              "Täckningsgrad depåladdning"
            ),
            popover(
              bs_icon("question-circle"),
              "Mäter hur mycket av bilens beräknade totala energiåtgång som kommer
              från depåladdning. Om täckningagraden är låg betyder det att en stor
              del av elen du tankar måste komma från extern laddning, vanligen
              dyr snabbladdning. De flesta lönsamma transportupplägg använder sig av
              snabbladdning som stöd för dagarna med längst körsträckor och förlitar
              sig helt på depåladdning för en typisk kördag."
            )
          ),
          tags$p(
            "Depåladdning täcker ca ",
            glue::glue("{perc_t(coverage$battery_coverage_of_total)}"),
            "av den totala fordonselen."
          ),
          (if (coverage$battery_coverage_of_total_is_low) { 
            tags$p(
              class = "cost-warning",
              bs_icon("exclamation-circle", class = "warning"),
              tags$em("Överväg att beställa ett större batteri till bilen.
                      Merkostnaden för ett större batteri kan vara väsentligt
                      mindre än kostnaden för att stödladda bilen.")
            )
          })
        )
      )
    )
  })
  
  
  ## UI elements ----
  
  
  output$scoring_header <- renderUI({
    x <- CaseData()
    
    ice_tco <- x$diesel_truck_cost + x$sim_grid$total_diesel_cost[[1]]
    bev_tco <- x$bev_truck_cost +
      x$charger_cost +
      x$sim_grid$total_private_electricity_cost[[1]] +
      x$sim_grid$total_public_electricity_cost[[1]]
    
    bev_to_ice_ratio_diff <- bev_tco/ice_tco - 1
    
    scoring_text <- as.character(scoring_func(bev_to_ice_ratio_diff + 1))
    HTML(scoring_text)
  })
  
  output$scoring_tagline <- renderUI({
    x <- CaseData()
    if (is.null(x)) return(NULL)
    
    ice_tco <- x$diesel_truck_cost + x$sim_grid$total_diesel_cost[[1]]
    bev_tco <- x$bev_truck_cost +
      x$charger_cost +
      x$sim_grid$total_private_electricity_cost[[1]] +
      x$sim_grid$total_public_electricity_cost[[1]]
    
    bev_to_ice_ratio_diff <- bev_tco / ice_tco - 1
    
    tags$div(
      class = "scoring-tagline-result",
      "TCO för elbilen beräknas bli ca",
      tags$strong(perc_t(abs(bev_to_ice_ratio_diff)),
                  if (bev_to_ice_ratio_diff >= 0)
                    "högre"
                  else
                    "lägre"
      ),
      "än för dieselbilen."
    )
    
  })
  
  output$tco_comparison_sum_explanation <- renderUI({
    div(
      class = "tco-bar-cost-span",
      glue::glue("Prognos för den totala kostnaden för fordonet under avskrivningstiden ({input$p_vehicle_service_life} år)")
    )
  })
  
  output$profitability_warning <- renderUI({
    case <- CaseData()
    
    if (!is.null(case$warning))
      display_warning <- "block"
    else
      display_warning <- "none"
    
    div(
      class = "calculator-element-block",
      div(
        style = glue::glue("display: {display_warning};"),
        class = "cost-warning",
        "Lorem odio velit aliquet quis est euismod sollicitudin ridiculus dis facilisi id
enim fermentum? Id magnis velit aliquet rhoncus justo.",
        tooltip(tags$strong("Läs eventuellt mer här"), "Det här kanske blir en text om varför du fått en varning")
      )
    )
  })
  
  
  ## Display inputs (debugging) ----
  
  output$show_case_inputs <- renderTable({
    
    x <- CaseData()
    
    # x$sim_grid
    data.frame(
      names = names(x),
      values = unlist(
        map(x, ~ ( if (class(.x)[[1]] == "tbl_df") "Data frame" else as.character(.x[[1]]) ) )
      )
    )
  })
  
  output$driving_range_plot <- renderPlot({
    x <- CaseData()
    
    x$dists[1,] |> 
      mutate(k_label = format(k, digits = 2)) |> 
      unnest(cols = c(pdist)) |> 
      ggplot(aes(x = x, y = y_pred)) +
      geom_line() +
      lims(
        y = c(0, 1)
      ) +
      geom_point(aes(x = x_max), y = 0) +
      geom_point(aes(x = x0_by_x_user), y = 0.5, color = "red") +
      geom_point(aes(x = x_user, y = y_user_pred_x_user), color = "blue") +
      geom_text(aes(label = paste0("k = ", k_label)), x = 50, y = 0.4, hjust = "inward") +
      geom_text(aes(label = paste0("A = ", A)), x = 50, y = 0.25, hjust = "inward") +
      geom_text(aes(label = paste0("nu = ", nu)), x = 50, y = 0.1, hjust = "inward")
  })
  
  
  
  ## Load scenarios ----
  
  observe({
    isolate({
      loaded_case <- predefined_cases[[1]]
      
      names(loaded_case) <- paste0("p_", names(loaded_case))
      
      purrr::map(
        names(loaded_case),
        function(item) {
          if (is.numeric(loaded_case[[item]]) && length(loaded_case[[item]] == 1))
            updateNumericInput(session, item, value = loaded_case[[item]])
          else if (is.numeric(loaded_case[[item]]))
            updateSliderInput(session, item, value = loaded_case[[item]])
          else if (is.character(loaded_case[[item]]))
            updateCheckboxGroupInput(session, item, selected = loaded_case[[item]])
        }
      )
    })
  }) |> 
    bindEvent(input$scenario_1_button)
  
  observe({
    isolate({
      loaded_case <- predefined_cases[[2]]
      
      names(loaded_case) <- paste0("p_", names(loaded_case))
      
      purrr::map(
        names(loaded_case),
        function(item) {
          if (is.numeric(loaded_case[[item]]) && length(loaded_case[[item]] == 1))
            updateNumericInput(session, item, value = loaded_case[[item]])
          else if (is.numeric(loaded_case[[item]]))
            updateSliderInput(session, item, value = loaded_case[[item]])
          else if (is.character(loaded_case[[item]]))
            updateCheckboxGroupInput(session, item, selected = loaded_case[[item]])
        }
      )
    })
  }) |> 
    bindEvent(input$scenario_2_button)
  
  observe({
    isolate({
      loaded_case <- predefined_cases[[3]]
      
      names(loaded_case) <- paste0("p_", names(loaded_case))
      
      purrr::map(
        names(loaded_case),
        function(item) {
          if (is.numeric(loaded_case[[item]]) && length(loaded_case[[item]] == 1))
            updateNumericInput(session, item, value = loaded_case[[item]])
          else if (is.numeric(loaded_case[[item]]))
            updateSliderInput(session, item, value = loaded_case[[item]])
          else if (is.character(loaded_case[[item]]))
            updateCheckboxGroupInput(session, item, selected = loaded_case[[item]])
        }
      )
    })
  }) |> 
    bindEvent(input$scenario_3_button)
  
  observe({
    isolate({
      loaded_case <- predefined_cases[[4]]
      
      names(loaded_case) <- paste0("p_", names(loaded_case))
      
      purrr::map(
        names(loaded_case),
        function(item) {
          if (is.numeric(loaded_case[[item]]) && length(loaded_case[[item]] == 1))
            updateNumericInput(session, item, value = loaded_case[[item]])
          else if (is.numeric(loaded_case[[item]]))
            updateSliderInput(session, item, value = loaded_case[[item]])
          else if (is.character(loaded_case[[item]]))
            updateCheckboxGroupInput(session, item, selected = loaded_case[[item]])
        }
      )
      
    })
    updateActionButton(session, "run_sim_button")
  }) |> 
    bindEvent(input$scenario_4_button)
  
  
  ## Old/discarded ----
  
}


