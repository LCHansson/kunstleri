## Libs ----
library(shiny)

## Helpers ----
# App helpers
source("api/sensible_defaults.R")
source("api/func.R")

# Case simulation
source("api/sim_grid_refactor.R")
source("api/model_selection.R")
source("api/profitability_simulation.R")

# Scoring
source("api/profitability.R")

# Predefined scenarios
source("api/predefined_cases.R")


## Initialise placeholder ----
scoring_func <- cost_comparison_categories
ui_is_visible <<- FALSE


## App ----

server <- function(input, output, session) {
  
  
  ## dev CSS ----
   
  output$tco_css <- renderUI({
    tags$head(
      tags$style(
        HTML(
          # paste0(c(readLines("www/assets/stylesheets/reboot_style.css")))
          paste0(c(readLines("https://raw.githubusercontent.com/LCHansson/kunstleri/refs/heads/main/www/assets/stylesheets/reboot_style.css")))
        )
      )
    )
  })
  
  
  ## Empty state ----
  
  observe({
    input$run_sim_button
    
    isolate({
      
      if (input$run_sim_button > 0) {
        
        # if(ValidateInputs()) {
        # removeUI("")
        removeUI("#hide-calculator")
        ui_is_visible <<- TRUE
        # }
        message("ui_is_visible == ", ui_is_visible)
      } else
        message("ui_is_visible == ", ui_is_visible)
    })
  })
  
  
  ## Case data ----
  
  CaseInputs <- reactive({
    input$run_sim_button
    
    isolate({
      x <- reactiveValuesToList(input)
      x$p_charge_modes <- input$p_charge_modes
      test_cbgi <<- input$p_charge_modes
      
      x <- x[order(names(x))]
      # x <- x |> purrr::discard(stringr::str_detect(names(x), "button"))
      x <- x |> purrr::keep(stringr::str_detect(names(x), "^p_"))
      names(x) <- stringr::str_remove(names(x), "^p_")
    })
    
    x
  })
  
  ValidateInputs <- reactive({
    input$run_sim_button
    
    isolate({
      primary_inputs <- c(
        "p_diesel_truck_cost",
        "p_bev_truck_cost",
        "p_battery_size",
        "p_shorter_driving_distance",
        "p_longer_driving_distance",
        "p_frequency_above_typical_range",
        "p_charge_modes"
      )
      
      input_vals <- map(primary_inputs, function(i) input[[i]])
      # input_vals_global <<- map(primary_inputs, function(i) input[[i]])
    })
    
    if(any(is.na(input_vals)) || any(is.null(input_vals)))
      return(FALSE)
    else
      return(TRUE)
  })
  
  CaseData <- reactive({
    input$run_sim_button
    
    if (ValidateInputs()) {
      
      isolate({
        ## Get case params
        case <- sensible_defaults
        
        case_inputs <- CaseInputs()
        case_inputs$frequency_above_typical_range <- as.numeric(case_inputs$frequency_above_typical_range)
        case_inputs$night_charging_capacity <- as.numeric(case_inputs$night_charging_capacity)
        
        case_inputs_global <<- case_inputs
        
        case[names(case_inputs)] <- case_inputs
        
        ## Do sim
        case <- create_sim_grid(case)
        case <- best_models(case)
        case <- profitability_simulation(case)
        
        case_global <<- case
      })
      return(case)
    } else{
      return(NULL)
    }
    
  })
  
  BarHeights <- reactive({
    case <- CaseData()
    if (is.null(case))
      return(NULL)
    
    total_height <- 200
    
    # conf_interval <- c(lower = 0.05, higher = 0.95)
    
    cost_params <- tibble( 
      ice = case$diesel_truck_cost,
      diesel = case$sim_grid$total_diesel_cost,
      bev = case$bev_truck_cost,
      charger = case$sim_grid$charger_cost,
      private_charging = case$sim_grid$total_private_electricity_cost,
      public_charging = case$sim_grid$total_public_electricity_cost
    ) |> 
      pivot_longer(cols = everything()) |> 
      mutate(
        category = c(0, 0, 1, 1, 1, 1)
      ) |> 
      mutate(
        grp_share = value / sum(value),
        grp_sum = sum(value),
        .by = "category"
      )
    
    
    ## Div heights
    div_list <- cost_params |> 
      mutate(
        bar_height = grp_sum / max(grp_sum) * total_height,
        div_height = grp_share * bar_height
      ) |> 
      select(name, div_height) |>
      as.list()
    
    div_heights <- round_preserve_sum(div_list$div_height)
    names(div_heights) <- div_list$name
    
    
    list(
      div_heights = div_heights
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
    if(is.null(case)) return(NULL)
    div_heights <- BarHeights()$div_heights
    div_height_global <<- div_heights
    
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
            style = glue::glue("height: {div_heights[['diesel']]}px;"),
            div(
              class = "tco-component-label right",
              style = if(div_heights[['diesel']] == 0) "display: none;" else "",
              "Diesel"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights[['ice']]}px;"),
            div(
              class =  "tco-component-shape"
            ),
            div(
              class = "tco-component-label right",
              "Fordon"
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
            style = if(div_heights[['public_charging']] != 0) {
              glue::glue("height: {div_heights[['public_charging']]}px;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if(div_heights[['public_charging']] == 0) "display: none;" else "",
              "Snabbladdning"
            )
          ),
          div(
            class = "tco-component tco-component-private-charging",
            style = if(div_heights[['private_charging']] != 0) {
              glue::glue("height: {div_heights[['private_charging']]}px;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if(div_heights[['private_charging']] == 0) "display: none;" else "",
              "Depåladdning"
            )
          ),
          div(
            class = "tco-component tco-component-charger",
            style = if(div_heights[['charger']] != 0) {
              glue::glue("height: {div_heights[['charger']]}px;")
            } else {
              "display: none; margin: 0; padding: 0;"
            },
            div(
              class = "tco-component-label left",
              style = if(div_heights[['charger']] == 0) "display: none;" else "",
              "Laddinfra"
            )
          ),
          div(
            class = "tco-component tco-component-vehicle",
            style = glue::glue("height: {div_heights[['bev']]}px;"),
            div(
              class =  "tco-component-shape"
            ),
            div(
              class = "tco-component-label left",
              "Fordon"
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
  
  
  observe({
    case <- CaseData()
    
    if (!is.null(case$warning)) {
      insertUI(
        "#calculator-bar-id",
        where = "afterEnd",
        ui = uiOutput("profitability_warning")
      )
    }
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
    
    bev_to_ice_ratio_diff <- bev_tco/ice_tco - 1
    
    if (!"include_public_charging" %in% x$charge_modes && x$longer_driving_distance > x$dist_model$battery_capacity_km) {
      warning_header <- tagList(tags$div(
        class = "battery-range-warning",
        bs_icon("exclamation-circle", class = "warning"),
        # glue::glue("Beräkningen utgår från att bilen endast laddar i depå. Bilens maximala räckvidd är därmed {round(x$dist_model$battery_capacity_km)} km, vilket innebär att vissa längre körsträckor inte tagits med i beräkningen.")
        glue::glue("Utan tillgång till publik laddning begränsas de möjliga körsträckorna. I beräkningen har därför antagits att ingen bil kör längre än batteriets maximala räckvidd på en dag, vilket är ca {round(x$dist_model$battery_capacity_km)} km.")
      ))
    } else
      warning_header <- NULL
    
    tagList(
      warning_header,
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
    )
    
  })
  
  output$profitability_warning <- renderUI({
    div(
      class = "calculator-element-block",
      div(
        class = "cost-warning",
        "Hej"
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
  
  # output$bev_cost_drivers <- renderUI({
  #   div_heights <- BarHeights()$div_heights_cost_shares
  #   cost_shares  <- BarHeights()$cost_shares 
  #   
  #   div(
  #     # Hela stapeldiagrammet
  #     class = "tco-graph",
  #     div(
  #       # En av staplarna med etiketter
  #       class = "tco-bar",
  #       div(
  #         # Stapeln
  #         class = "tco-components",
  #         div(
  #           class = "tco-component tco-component-public-charging",
  #           style = glue::glue("height: {div_heights[['public_charging']]}px;")
  #         ),
  #         div(
  #           class = "tco-component tco-component-private-charging",
  #           style = glue::glue("height: {div_heights[['private_charging']]}px;")
  #         ),
  #         div(
  #           class = "tco-component tco-component-charger",
  #           style = glue::glue("height: {div_heights[['charger']]}px;")
  #         ),
  #         div(
  #           class = "tco-component tco-component-vehicle",
  #           style = glue::glue("height: {div_heights[['battery']]}px;")
  #         )
  #       )
  #     ),
  #     
  #     div(
  #       class = "cost-driver-explanation",
  #       div(
  #         class = "cost-driver-component",
  #         tags$p(
  #           tags$strong(class = "text-public-charging", "Snabbladdning "),
  #           popover(
  #             bs_icon("question-circle"),
  #             "Ett komplement till den betydligt billigare depåladdningen för de flesta transportupplägg."
  #           )
  #         ),
  #         tags$p(
  #             glue::glue("ca {tsep_t(case$sim_grid$public_charging_cost)} kr"),
  #           "eller",
  #             glue::glue("{perc_t(case$sim_grid$public_charging_cost)}"),
  #           "av bilens totala TCO."
  #         )
  #       ),
  #       div(
  #         class = "cost-driver-component",
  #         tags$p(
  #           tags$strong(class = "text-private-charging", "Depåladdning "),
  #           popover(
  #             bs_icon("question-circle"),
  #             "Den huvudsakliga drivmedelskällan för de flesta transportupplägg med elbil.
  #             Bör vara betydligt billigare per kWh än publik snabbladdning."
  #           )
  #         ),
  #         tags$p(
  #           if_else(
  #             tsep_t(cost_shares$private_charging_cost_lower) == tsep_t(cost_shares$private_charging_cost_higher),
  #             glue::glue("ca {tsep_t(cost_shares$private_charging_cost_higher)} kr"),
  #             glue::glue("ca {tsep_t(cost_shares$private_charging_cost_lower)} - {tsep_t(cost_shares$private_charging_cost_higher)} kr")
  #           ),
  #           "eller",
  #           if_else(
  #             perc_t(cost_shares$private_charging_share_lower) == perc_t(cost_shares$private_charging_share_higher),
  #             glue::glue("{perc_t(cost_shares$private_charging_share_lower)}"),
  #             glue::glue("{perc_t(cost_shares$private_charging_share_lower)} - {perc_t(cost_shares$private_charging_share_higher)}")
  #           ),
  #           "av bilens totala TCO."
  #         )
  #       ),
  #       div(
  #         class = "cost-driver-component",
  #         tags$p(
  #           tags$strong(
  #             class = "text-charger",
  #             "Laddinfrastruktur"
  #           ),
  #           popover(
  #             bs_icon("question-circle"),
  #             "En merkostnad som ofta följer med ett elbilsköp. Därför räknas
  #               den in, nyttjandeviktat, i fordonets TCO.
  #               Denna kostnad utgör vanligen en mindre andel av fordonets TCO."
  #           )
  #         ),
  #         tags$p(
  #           glue::glue("{tsep_t(cost_shares$charger_cost)} kr"),
  #           "eller",
  #           if_else(
  #             perc_t(cost_shares$charger_share_lower) == perc_t(cost_shares$charger_share_higher),
  #             glue::glue("ca {perc_t(cost_shares$charger_share_lower)}"),
  #             glue::glue("ca {perc_t(cost_shares$charger_share_lower)} - {perc_t(cost_shares$charger_share_higher)}")
  #           ),
  #           "av bilens totala TCO."
  #         )
  #       ),
  #       div(
  #         class = "cost-driver-component",
  #         tags$p(
  #           tags$strong(class = "text-vehicle", "Batteri/drivlina "),
  #           popover(
  #             bs_icon("question-circle"),
  #             "Skillnaden i pris mellan diesel- elbil och kan beskrivas som merkostnaden för en batteridriven drivlina. Batteriets merkostnad utgör ofta en stor del av elbilars TCO."
  #           )
  #         ),
  #         tags$p(
  #           glue::glue("{tsep_t(cost_shares$battery_cost)} kr"),
  #           "eller",
  #           if_else(
  #             perc_t(cost_shares$battery_share_lower) == perc_t(cost_shares$battery_share_higher),
  #             glue::glue("ca {perc_t(cost_shares$battery_share_lower)}"),
  #             glue::glue("ca {perc_t(cost_shares$battery_share_lower)} - {perc_t(cost_shares$battery_share_higher)}")
  #           ),
  #           "av bilens totala TCO."
  #         )
  #       )
  #     )
  #   )
  # })
}


