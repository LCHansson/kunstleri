## Libs ----
library(shiny)
library(bslib)
library(bsicons)


## inputs ----
source("api/numericInputWithUnit.R")


## UI ----

ui <- page_fluid(
  shinyjs::useShinyjs(),
  
  # theme = bs_theme(),
  # title = "TF BETARELEASE: TCO-jämförelse", lang = "sv",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("assets/stylesheets/style.css?", lubridate::seconds(Sys.time()))),
    # tags$link(rel = "stylesheet", type = "text/css", href = "assets/stylesheets/style.css"),
    tags$meta(charset = "UTF-8"),
    tags$title("Elektrifieringskollen"),
    tags$meta(name = "viewport", content = "width = device-width, initial-scale = 1"),
    
    tags$script(HTML("
    $(document).ready(function() {
      $('#explanation-text').hide();  // Slide down/up the detailed inputs
      $('#view_calculation_explanation').on('click', function() {
        $('#explanation-text').slideToggle('slow');  // Slide down/up the detailed inputs
      });
    });
  ")),
  ),
  
  div(
    class = "app-layout",
    
    div(
      class = "calculator",
      
      ## Primary inputs ----
      div(
        class = "sidebar",
        
        div(
          class = "primary-inputs",
          
          ## Primary inputs ----
          radioButtons(
            "p_vehicle_class",
            NULL,
            choices = c(
              "Lätt lastbil" = "van",
              "Tung lastbil" = "truck"
            ),
            selected = "truck",
            inline = TRUE
          ),
          
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
          
          shinyjs::hidden(checkboxGroupInput(
            "p_charge_modes", 
            label = NULL,
            inline = TRUE,
            # choices = c("Publik snabbladdning" = "include_public_charging", "Depåladdning" = "include_private_charging"),
            choiceValues = list("include_private_charging", "include_public_charging"),
            choiceNames = list("Har tillgång till depåladdare", "Kan ladda publikt vid behov"),
            selected = c("include_public_charging", "include_private_charging")
          ))
        ),
        
        actionButton("run_sim_button", "Beräkna", class = "btn-primary"),
        
        ## Scenario buttons ----
        shinyjs::hidden(div(
          id = "sidebar-scenarios",
          class = "sidebar-scenarios",
          div(
            class = "fine-print",
            tags$span(
              "Har du inga siffror framför dig just nu?",
              tags$br("Testa ett av följande scenarios:"))
          ),
          div(
            class = "sidebar-scenario-buttons",
            actionButton("scenario_1_button", "Dagligvaror glesbygd", class = "btn-secondary"),
            actionButton("scenario_2_button", "Skogsbil", class = "btn-secondary"),
            actionButton("scenario_3_button", "62 ton fjärrbil", class = "btn-secondary"),
            actionButton("scenario_4_button", "16 ton stadsbil", class = "btn-secondary")
          )
        ))
      ),
      
      ## Empty state ----
      
      div(
        id = "empty-state",
        class = "empty-state result-frame",
        p(
          class = "empty-state-heading",
          "Fyll i värden för att beräkna kostnaden"
        ),
        p(
          class = "empty-state-text",
          htmltools::HTML("Har du inga siffror framför dig just nu? <br>Testa ett av följande scenarier:")
        ),
        div(
          class = "empty-state-scenario-buttons",
          actionButton("scenario_1_button", "Dagligvaror glesbygd", class = "btn-secondary"),
          actionButton("scenario_2_button", "Skogsbil", class = "btn-secondary"),
          actionButton("scenario_3_button", "62 ton fjärrbil", class = "btn-secondary"),
          actionButton("scenario_4_button", "16 ton stadsbil", class = "btn-secondary")
        )
      ),
      
      
      ## Result ----
      shinyjs::hidden(div(
        id = "result",
        class = "result result-frame",
        # style = "display: none;",
        
        ## Result header ----
        div(
          class = "result-header",
          div(
            class = "result-header-heading",
            uiOutput("scoring_header")
          ),
          div(
            class = "result-header-tagline",
            uiOutput("scoring_tagline")
          )
        ),
        
        ## Result graph ----
        div(
          class = "result-graph",
          id = "calculator-bar-id",
          uiOutput("tco_comparison_bars_ui"),
          uiOutput("profitability_warning")
        ),
        
        ## Secondary inputs ----
        div(
          class = "frame",
          div(
            class = "frame-header",
            "Antaganden"
          ),
          div(
            class = "frame-section slider-inputs",
            div(
              class = "slider-input span-diesel",
              sliderInput(
                "p_diesel_cost",
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
              class = "slider-input span-private-charging",
              sliderInput(
                "p_private_charging_cost",
                "Pris depåladdning per kWh",
                min = 0.2,
                max = 5,
                value = 1.1,
                step = 0.1,
                ticks = FALSE,
                dragRange = FALSE,
                post = "kr"
              )
            ),
            div(
              class = "slider-input span-public-charging",
              sliderInput(
                "p_public_charging_cost",
                "Pris snabbladdning per kWh",
                min = 1,
                max = 10,
                value = 4.5,
                step = 0.25,
                ticks = FALSE,
                dragRange = FALSE,
                post = "kr"
              )
            )
          ),
          div(
            class = "frame-section columns",
            div(
              class = "secondary-inputs",
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
            ),
            div(
              class = "secondary-inputs",
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
                selectInput(
                  "p_night_charging",
                  "Hur mycket laddar bilen över natt?",
                  choices = setNames(seq(0, 1, 0.1), paste(seq(0, 100, 10), "%")),
                  selected = 1.0
                )
              ),
              div(
                class = "secondary-input",
                selectInput(
                  "p_day_extra_home_charging",
                  "Hur mycket laddar bilen mellan skift?",
                  choices = setNames(seq(0, 1, 0.1), paste(seq(0, 100, 10), "%")),
                  selected = 0
                )
              ),
              div(
                class = "secondary-input",
                selectInput(
                  "p_public_charging_availability",
                  "Kan bilen ladda publikt vid behov?",
                  choices = c("Ja" = "ja", "Nej" = "nej"),
                  selected = "ja"
                )
              )
              
            )
          )
        ),
        
        ## Optional inputs ----
        div(
          class = "columns",
          
          ## Charging infrastructure ----
          div(
            class = "frame",
            div(
              class = "frame-header",
              checkboxInput("o_include_charger", "Laddinfrastruktur")
            ),
            div(
              class = "frame-section secondary-inputs",
              div(
                class = "secondary-input",
                numericInput(
                  "p_charger_cost",
                  "Installation laddare",
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
                  "p_grid_cost",
                  "Förstärkning elnät",
                  min = 0,
                  max = 1500000,
                  value = 0,
                  step = 10000
                ),
                p("kr")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_charger_sharing_n",
                  "Hur många fordon delar på laddaren?",
                  min = 1, max = 20, step = 1, value = 1
                )
              )
            )        
          ),
          
          ## Taxes ----
          div(
            class = "frame",
            div(
              class = "frame-header",
              checkboxInput("o_include_taxes", "Fordonsskatt och vägavgift")
            ),
            div(
              class = "frame-section secondary-inputs",
              shinyjs::hidden(div(
                id = "ice_rollout_year",
                class = "secondary-input",
                numericInput(
                  "p_ice_rollout_year",
                  "Första trafikår, bensin/dieselbil",
                  min = 2010,
                  max = 2025,
                  value = 2024,
                  step = 1
                )
              )),
              shinyjs::hidden(div(
                id = "fuel_type",
                class = "secondary-input",
                selectInput(
                  "p_fuel_type",
                  "Bränsletyp förbränningsmotor",
                  choices = c("Diesel" = "diesel", "Annat bränsle" = "ej diesel"),
                  selected = "Diesel"
                )
              )),
              shinyjs::hidden(div(
                id = "ice_co2_value",
                class = "secondary-input",
                numericInputWithUnit(
                  "p_ice_co2_value",
                  "CO2-utsläpp bensin/dieselbil",
                  min = 10,
                  max = 1000,
                  value = 350,
                  step = 10,
                  unit = "/km"
                )
              )),
              div(
                id = "ice_weight",
                class = "secondary-input",
                numericInputWithUnit(
                  "p_ice_weight",
                  "Totalvikt dieselbil",
                  min = 3.5,
                  max = 64,
                  value = 16,
                  step = 0.5,
                  unit = "ton"
                )
              ),
              div(
                id = "num_axles",
                class = "secondary-input",
                numericInput(
                  "p_num_axles",
                  "Antal axlar",
                  min = 2,
                  max = 5,
                  value = 3,
                  step = 1
                )
              ),
              div(
                id = "trailer_type",
                class = "secondary-input",
                selectInput(
                  "p_trailer_type",
                  "Draganordning",
                  choices = c("Dragbil" = "dragbil", "Ingen" = "utan", "Annat påhäng" = "annan"),
                  selected = "utan"
                )
              ),
              div(
                id = "road_toll_duty",
                class = "secondary-input",
                selectInput(
                  "p_road_toll_duty",
                  "Vägavgiftspliktig",
                  choices = c("Ja" = "ja", "Nej" = "nej"),
                  selected = "ja"
                )
              )
            )    
          )
        ),
        div(
          class = "columns",
          
          ## Maintenance ----
          div(
            class = "frame",
            div(
              class = "frame-header",
              checkboxInput("o_include_service", "Service")
            ),
            div(
              class = "frame-section secondary-inputs",
              div(
                class = "secondary-input",
                numericInput(
                  "p_ice_service_cost",
                  "Servicekostnad per mil, bensin/dieselbil",
                  min = 1,
                  max = 25,
                  value = 8,
                  step = 0.5
                ),
                p("kr")
              ),
              div(
                class = "secondary-input",
                numericInput(
                  "p_bev_service_cost",
                  "Servicekostnad per mil, elbil",
                  min = 1,
                  max = 25,
                  value = 7.5,
                  step = 0.5
                ),
                p("kr")
              )
            )   
          ),
          
          ## Tires ----
          div(
            class = "frame",
            div(
              class = "frame-header",
              checkboxInput("o_include_tires", "Däck")
            ),
            div(
              class = "frame-section secondary-inputs",
              div(
                class = "secondary-input",
                numericInput(
                  "p_ice_tire_cost",
                  "Däckkostnad per mil, diesel/bensinbil",
                  min = 0,
                  max = 10,
                  value = 5,
                  step = 0.1
                ),
                p("kr")
              ),
              div(
                class = "secondary-input",
                selectInput(
                  "p_bev_tire_increase",
                  "Ökat däckslitage med elbil",
                  choices = setNames(seq(0, 1, 0.1), paste(seq(0, 100, 10), "%")),
                  selected = 0.2
                )
              )
            )
          )
        ),
        
        div(
          class = "explanation",
          div(
            class = "explanation-button-container",
            actionButton("view_calculation_explanation", "Se hur vi har räknat")
          ),
          div(
            class = "explanation-text",
            id = "explanation-text",
            p("Adipiscing laoreet ornare habitasse potenti donec lacinia litora. Nunc tortor
faucibus mus tincidunt cursus ornare curabitur, venenatis blandit. Odio dictumst
placerat commodo pretium class urna; egestas tempus – mauris rutrum magnis primis
dictumst! Nunc nibh sociis primis, elementum primis cum mus."),

p("Lorem faucibus pulvinar pretium quis – ante a duis, quisque massa himenaeos potenti
sed ac. Mus ultrices massa; nisi in – diam libero condimentum purus, eros class.
Nisl curabitur convallis lectus vulputate tortor ullamcorper, mus turpis porttitor
nibh lacinia! Sagittis mi rhoncus nulla taciti per! Mattis auctor pharetra est
nostra et. Interdum tellus auctor orci aenean dis magnis sociosqu proin proin,
ornare, tellus per nulla auctor aliquet molestie fringilla etiam."),

p("Dolor justo fames netus: dictum est varius placerat ligula sodales. Cum magnis
dictum aptent cubilia nullam eros! Odio feugiat aenean, quam mus ut senectus ut –
tempus imperdiet orci vel, neque justo porttitor orci ridiculus."),

p("Elit turpis phasellus litora facilisi velit tellus imperdiet. Pulvinar consequat
augue sed purus justo nisi morbi pretium vitae, potenti imperdiet tortor etiam!
Viverra potenti metus, suscipit: tristique litora: erat tristique senectus fringilla
nec cras? Ac cras auctor venenatis elementum imperdiet, eget porta quam purus
facilisi. Consequat ac pharetra pretium potenti, taciti curae; tristique curabitur
habitasse."),

p("Adipiscing habitant fringilla netus; class torquent tellus! Facilisi senectus
lobortis, fusce rutrum risus ligula fermentum duis sagittis. Porttitor condimentum
interdum ad quam luctus quam consequat – volutpat sagittis. Per cum mollis ad odio
hac vivamus, tellus quisque dapibus leo ultricies. Eleifend ultrices luctus magna
aliquam curabitur vulputate platea rutrum rutrum scelerisque. Consequat nisi nunc
vulputate diam, massa nisl odio est eu convallis. Eleifend curabitur velit lacus
augue lobortis ullamcorper commodo, pharetra eleifend imperdiet sed nulla, felis
eros ligula netus elementum commodo venenatis elementum.")
          )
        )
      ))
      
      ## End ----
    )
  )
  
  # tableOutput("show_case_inputs")
)
