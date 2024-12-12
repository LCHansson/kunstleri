library(shiny)
# library(tidyverse)
library(bslib)
library(bsicons)

ui <- page_fluid(
  # theme = bs_theme(),
  # title = "TF BETARELEASE: TCO-jämförelse", lang = "sv",
  
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "assets/stylesheets/reboot_style.css"),
    tags$meta(charset = "UTF-8"),
    tags$title("Elektrifieringskollen"),
    tags$meta(name = "viewport", content = "width = device-width, initial-scale = 1")
  ),
  
  uiOutput("tco_css"),
  
  
  div(
    class = "app-layout",
    
    div(
      class = "calculator",
      
      ## Primary sidebar ----
      div(
        class = "primary-sidebar",
        
        div(
          class = "app-title",
          "Elektrifieringskollen"
        ),
        
        div(
          class = "primary-inputs",
          
          ## Primary inputs ----
          # TODO: Wrap these in layout div containers to enable rounded corners,
          # width control etc.
          numericInput(
            "p_diesel_truck_cost",
            label = span(
              tags$span(HTML("Dieselbilens pris, exkl. moms (<em>kr</em>)")),
              tooltip(
                bs_icon("question-circle"),
                "Ange priset på den dieselbil som motsvarar elbilen du kan tänkas vilja
                köpa. Lastbilstillverkare anger i allmänhet ett referenspris på en sådan
                bil i alla offerter på en ellastbil."
              )
            ),
            min = 0,
            max = 30000000,
            value = NA,
            step = 25000
          ),
          
          numericInput(
            "p_bev_truck_cost",
            label = span(
              tags$span(HTML("Elbilens pris, exkl. moms (<em>kr</em>)")),
              tooltip(
                bs_icon("question-circle"),
                "Ange pris från offert, exklusive moms."
              )
            ),
            min = 0,
            max = 30000000,
            value = NA,
            step = 25000
          ),
          
          numericInput(
            "p_bev_climate_premium",
            label = span(
              tags$span("Klimatpremie för elbilen", HTML("(<em>kr</em>)")),
              tooltip(
                bs_icon("question-circle"),
                "Om du köper en tung eldriven lastbil har du sannolikt rätt till pengar från Klimatpremien.",
                br(),
                tags$a(href = "https://www.energimyndigheten.se/", target = "_blank", "Läs mer om klimatpremien på Energimyndighetens hemsida")
              )
            ),
            min = 0,
            max = 7000000,
            value = NA,
            step = 25000
          ),
          
          numericInput(
            "p_battery_size",
            label = span(
              HTML("Elbilens batteristorlek (<em>kWh</em>)"),
              tooltip(
                bs_icon("question-circle"),
                "Ange användbar kapacitet. Om batteriet t.ex. har en teoretisk kapacitet
                på 500 kWh men bara kan laddas till 80% på grund av tillverkarens inbyggda
                begränsning anger du 400 kWh."
              )
            ),
            min = 100,
            max = 900,
            value = NA,
            step = 10
          ),
          
          numericInput(
            "p_shorter_driving_distance",
            label = span(
              HTML("Typisk daglig körsträcka (<em>mil</em>)"),
              tooltip(
                bs_icon("question-circle"),
                "Svaret på frågan \"hur långt kör lastbilen en vanlig dag på jobbet?\""
              )
            ),
            min = 50,
            max = 1000,
            value = NA,
            step = 25
          ),
          
          numericInput(
            "p_longer_driving_distance",
            label = span(
              HTML("Längsta daglig körsträcka (<em>mil</em>)"),
              tooltip(
                bs_icon("question-circle"),
                "Den längsta sträcka lastbilen kan tänkas på en dag under hela bilens livslängd."
              )
            ),
            min = 50,
            max = 1000,
            value = NA,
            step = 25
          ),
          
          selectInput(
            "p_frequency_above_typical_range",
            label = span(
              "Hur ofta kör du längre än den typiska körsträckan?"
            ),
            choices = c(
              "Två dagar i veckan" = 0.4,
              "En dag i veckan" = 0.2,
              "En dag varannan vecka" = 0.1,
              "Någon dag i månaden" = 0.05,
              "Några dagar per år" = 0.01
            ), selected = 0.2,
            selectize = TRUE
          ),
          
          checkboxGroupInput(
            "p_charge_modes", 
            label = NULL,
            inline = TRUE,
            # choices = c("Publik snabbladdning" = "include_public_charging", "Depåladdning" = "include_private_charging"),
            choiceValues = list("include_private_charging", "include_public_charging"),
            choiceNames = list("Har tillgång till depåladdare", "Kan ladda publikt vid behov"),
            selected = c("include_public_charging", "include_private_charging")
          )
        ),
        
        div(
          class = "calculate-button",
          actionButton("run_sim_button", "Beräkna", class = "btn-primary")
        ),
        
        
        ## Scenario buttons ----
        div(
          # class = "calculator-element-block",
          class = "scenario-block",
          style = "flex-direction: column; row-gap: 16px;",
          div(class = "divider"),
          div(
            # class = "scenario-block",
            class = "scenario-buttons",
            div(
              class = "fine-print",
              tags$span(
                style = "width: 100%;",
                "Har du inga siffror framför dig just nu?",
                tags$br("Testa ett av följande scenarios:"))
            ),
            div(
              class = "scenario-button-row",
              actionButton("scenario_1_button", "Dagligvaror glesbygd", class = "btn-secondary"),
              actionButton("scenario_2_button", "Skogsbil", class = "btn-secondary")
            ),
            div(
              class = "scenario-button-row",
              actionButton("scenario_3_button", "62 ton fjärrbil", class = "btn-secondary"),
              actionButton("scenario_4_button", "16 ton stadsbil", class = "btn-secondary")
            )
          )
        )
      ),
      
      ## Main view ----
      div(
        class = "main-view",
        
        ## Hide the calculator on load ----
        div(
          id = "hide-calculator",
          div(
            class = "hide-text",
            div(
              class = "hide-text-header",
              "Fyll i värden i menyn till vänster och tryck på Beräkna för att starta appen"
            ),
            div(
              class = "hide-text-subheader",
              "Har du inga siffror framför dig just nu?"
            ),
            div(
              tags$strong(style = "text-align: center", "Testa ett scenario längst ned i menyn till vänster.")
            )
          )
        ),
        
        ## Main view header ----
        div(
          class = "calculator-element-block",
          div(
            class = "column-block",
            div(
              class = "main-result-header",
              uiOutput("scoring_header")
            ),
            div(
              class = "result-tagline",
              uiOutput("scoring_tagline")
            )
          )
        ),
        
        div(
          class = "calculator-element-block",
          id = "calculator-bar-id",
          div(
            class = "tco-results",
            uiOutput("tco_comparison_bars_ui")
          )
        ),
        
        div(class = "divider"),
        
        ## Span inputs ----
        div(
          class = "calculator-element-block vertical-order",
          
          div(
            class = "secondary-input-frame",
            div(
              class = "secondary-input-frame-header",
              "Antaganden"
            ),
            div(
              class = "secondary-span-inputs",
              div(
                class = "span-input span-diesel",
                sliderInput(
                  "p_diesel_price_span",
                  "Dieselpris per liter",
                  min = 10,
                  max = 30,
                  value = 15,
                  step = 1,
                  ticks = FALSE,
                  dragRange = FALSE,
                  post = "kr"
                )),
              div(
                class = "span-input span-private-charging",
                sliderInput(
                  "p_private_charging_price_span",
                  "Pris depåladdning per kWh",
                  min = 0.2,
                  max = 2.5,
                  value = 1.1,
                  step = 0.1,
                  ticks = FALSE,
                  dragRange = FALSE,
                  post = "kr"
                )
              ),
              div(
                class = "span-input span-public-charging",
                sliderInput(
                  "p_public_charging_price_span",
                  "Pris snabbladdning per kWh",
                  min = 1,
                  max = 6,
                  value = 4.5,
                  step = 0.25,
                  ticks = FALSE,
                  dragRange = FALSE,
                  post = "kr"
                )
              )
            ),
            div(
              class = "secondary-inputs",
              div(
                class = "secondary-input",
                numericInput(
                  "p_run_days_per_week",
                  "Kördagar",
                  min = 0,
                  max = 7,
                  value = 5,
                  step = 1
                ),
                p("/vecka")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_holiday_weeks_per_year",
                  "Körningsfria veckor",
                  min = 0,
                  max = 10,
                  value = 2,
                  step = 1
                ),
                p("/år")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_diesel_per_10km",
                  "Dieselförbrukning",
                  min = 0.5,
                  max = 6,
                  value = 3,
                  step = 0.1
                ),
                p("liter/mil")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_charger_cost",
                  "Kostnad laddinfra",
                  min = 0,
                  max = 1500000,
                  value = 250000,
                  step = 10000
                ),
                p("kr")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_electricity_per_10km",
                  "Elförbrukning",
                  min = 6,
                  max = 18,
                  value = 12,
                  step = 0.2
                ),
                p("kWh/mil")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_vehicle_service_life",
                  "Avskrivningstid",
                  min = 5,
                  max = 14,
                  value = 7,
                  step = 1
                ),
                p("år")
              ),
              div(
                class = "secondary-input",
                sliderInput(
                  "p_night_charging_ratio",
                  "Hur mycket laddar bilen över natt?",
                  min = 0,
                  max = 100,
                  value = 100,
                  step = 5,
                  ticks = FALSE,
                  post = "%"
                )
              ),
              div(
                class = "secondary-input",
                sliderInput(
                  "p_day_extra_charging",
                  "Hur mycket laddar bilen mellan skift?",
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 5,
                  ticks = FALSE,
                  post = "%"
                )
              )
            )
          ),
          
          div(
            class = "tertiary-input-metaframe",
            div(
              class = "tertiary-input-frame",
              
              div(
                class = "tertiary-input-frame-header",
                checkboxInput("o_include_charger", "Laddinfrastruktur")
              ),
              div(
                class = "tertiary-inputs",
                div(
                  class = "tertiary-input",
                  numericInput(
                    "p_charger_cost",
                    "Installation",
                    min = 0,
                    max = 1500000,
                    value = 250000,
                    step = 10000
                  ),
                  p("kr")
                ),
                div(
                  class = "tertiary-input",
                  numericInput(
                    "p_net_cost",
                    "Förstärkning elnät",
                    min = 0,
                    max = 1500000,
                    value = 0,
                    step = 10000
                  ),
                  p("kr")
                ),
                div(
                  class = "tertiary-input",
                  numericInput(
                    "p_charger_sharing_n",
                    "Hur många fordon delar på laddaren?",
                    min = 1, max = 20, step = 1, value = 1
                  )
                )
              )
            ),
            div(
              class = "tertiary-input-frame",
              
              div(
                class = "tertiary-input-frame-header",
                checkboxInput("o_include_taxes", "Fordonsskatt")
              ),
              div(
                class = "tertiary-inputs",
                div(
                  class = "tertiary-input",
                  numericInput(
                    "p_rollout_year",
                    "År dieselbilen togs/tas i bruk",
                    min = 2010,
                    max = 2025,
                    value = 2024,
                    step = 1
                  )
                ),
                div(
                  class = "tertiary-input",
                  selectInput(
                    "p_fuel_type",
                    "Bränsletyp",
                    choices = c("Dieselbil", "Bensinbil"),
                    selected = "Dieselbil"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
