## IWN Hackathon 2025
## Data Exploration

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(markdown)
library(shiny)

## -------------------------
## Read data 
## -------------------------
meter_reads <- read_csv("clean_data/meter_reads.csv") |>
  mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Amsterdam"),
         date      = as.Date(timestamp))

households  <- read_csv("clean_data/houshold_meta_data.csv")
appliances  <- read_csv("clean_data/appliances.csv")

## -------------------------
## UI
## -------------------------
ui <- fluidPage(
  tags$head(tags$title("IWN Hackathon 2025")),
  titlePanel(
    tags$div(class = "app-title",
             tags$img(src = "iwn-logo.png", alt = "IWN Logo", height = "40px"),
             tags$span(class = "title-text", "IWN Hackathon 2025")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Digital Meter Data Explorer"),
      p(),
      checkboxInput(
        "only_meta",
        "Only meters with metadata",
        value = FALSE
      ),
      p("Number of random meters to select:"),
      sliderInput(
        inputId = "n_meters",
        label   = "",
        min = 2, max = 20, step = 2,
        value = 6
      ),
      actionButton("resample", "Resample meters"),
      br(), br(),
      dateRangeInput(
        "date_range", "Select date range",
        start = min(meter_reads$date),
        end   = max(meter_reads$date),
        min   = min(meter_reads$date),
        max   = max(meter_reads$date),
        startview = "month"
      ),
      p(),
      p("The data and code are available on ",
        a("GitHub", href = "https://github.com/intelligent-water-networks/iwn-hackathon-2025",
          target = "_blank"), ".")
    ),
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Meter reads",    uiOutput("linePlotUI")),
        tabPanel("Flows",          uiOutput("flowPlotUI")),
        tabPanel("Household Info", tableOutput("householdTable")),
        tabPanel("Appliances",     tableOutput("appliancesTable")),
        tabPanel("About",          includeMarkdown("README.md")),
        tabPanel("Challenge",      tags$iframe(style = "height:700px; width:100%; scrolling=yes",
                                               src = "iwn-hackathon-2025-challenge.pdf"))
      )
    )
  )
)

## -------------------------
## SERVER
## -------------------------
server <- function(input, output, session) {
  
  ## pick random meters helper
  pick_random <- function(av, n) {
    if (length(av) == 0) return(character(0))
    sample(av, size = min(n, length(av)))
  }
  
  ## Available meters based on metadata toggle
  meters_available <- reactive({
    base <- meter_reads |> distinct(smart_meter_id)
    if (isTRUE(input$only_meta)) {
      base <- base |>
        filter(smart_meter_id %in% unique(households$smart_meter_id))
    }
    sort(base$smart_meter_id)
  })
  
  ## ReactiveVal holds the current random selection
  selected_meters <- reactiveVal(character(0))
  
  ## Initialize & auto re-sample when availability or n_meters or only_meta changes
  observeEvent(list(meters_available(), input$n_meters, input$only_meta), {
    av  <- meters_available()
    sel <- pick_random(av, input$n_meters)
    selected_meters(sel)
  }, ignoreInit = FALSE)
  
  ## Manual re-sample button
  observeEvent(input$resample, {
    av  <- meters_available()
    sel <- pick_random(av, input$n_meters)
    selected_meters(sel)
  })
  
  ## Filter reads and calculate flows
  reads_and_flows <- reactive({
    req(length(selected_meters()) > 0, input$date_range)
    meter_reads |>
      filter(
        smart_meter_id %in% selected_meters(),
        date >= as.Date(input$date_range[1]),
        date <= as.Date(input$date_range[2])
      ) |>
      arrange(smart_meter_id, timestamp) |>
      group_by(smart_meter_id) |>
      mutate(
        dt_hours = as.numeric(difftime(timestamp, lag(timestamp), units = "hours")),
        d_read   = meter_reading - lag(meter_reading),
        flow     = ifelse(dt_hours > 0, d_read / dt_hours, NA_real_)
      ) |>
      ungroup() |>
      select(smart_meter_id, timestamp, meter_reading, flow)
  }) |> bindCache(selected_meters(), input$date_range)
  
  # Dynamic height calculator (two columns of facets)
  facet_height_px <- reactive({
    panels <- max(1, length(selected_meters()))
    rows   <- ceiling(panels / 2)
    200 * rows
  })
  
  # Plot heights |> 
  output$linePlotUI <- renderUI({
    plotOutput("linePlot", height = paste0(facet_height_px(), "px"))
  })
  
  output$flowPlotUI <- renderUI({
    plotOutput("flowPlot", height = paste0(facet_height_px(), "px"))
  })
  
  # Meter reads plot
  output$linePlot <- renderPlot({
    df <- reads_and_flows(); req(nrow(df) > 0)
    ggplot(df, aes(timestamp, meter_reading)) +
      geom_line() +
      facet_wrap(~ smart_meter_id, scales = "free_y", ncol = 2) +
      labs(x = "Timestamp", y = "Reading") +
      theme_minimal(base_size = 18)
  }, height = function() facet_height_px()) |>
    bindCache(selected_meters(), input$date_range)
  
  # Flow plot
  output$flowPlot <- renderPlot({
    df <- reads_and_flows(); req(nrow(df) > 0)
    ggplot(df, aes(timestamp, flow)) +
      geom_line(na.rm = TRUE) +
      facet_wrap(~ smart_meter_id, scales = "free_y", ncol = 2) +
      labs(x = "Timestamp", y = "Flow (units/hour)") +
      theme_minimal(base_size = 18)
  }, height = function() facet_height_px()) |>
    bindCache(selected_meters(), input$date_range)
  
  # Household metadata table
  household_data <- reactive({
    req(length(selected_meters()) > 0)
    hh <- households |> 
      filter(smart_meter_id %in% selected_meters())
    
    validate(need(nrow(hh) > 0,
                  "No household metadata for the current selection."))
    
    hh |>
      tidyr::pivot_longer(
        -smart_meter_id,
        names_to  = "Attribute",
        values_to = "Value",
        values_transform = as.character) |>
      tidyr::pivot_wider(
        names_from  = smart_meter_id,
        values_from = Value
      ) |>
      mutate(Attribute = str_replace_all(Attribute, "_", " "),
             Attribute = str_to_sentence(Attribute))
  })
  
  output$householdTable <- renderTable(
    household_data(),
    striped = TRUE
  )
  
  # Appliances table
  appliance_data <- reactive({
    req(length(selected_meters()) > 0)
    app <- appliances |> 
      filter(smart_meter_id %in% selected_meters())
    
    validate(need(nrow(app) > 0,
                  "No appliance data for the current selection."))
    
    app |>
      dplyr::mutate(number = as.integer(round(as.numeric(number)))) |>
      dplyr::group_by(smart_meter_id, .add = TRUE) |>
      tidyr::pivot_wider(
        names_from  = smart_meter_id,
        values_from = number,
        values_fn   = sum,
        values_fill = 0
      ) |> 
      rename(Appliance = appliance)
  })
  
  output$appliancesTable <- renderTable(
    appliance_data(),
    striped = TRUE,
    digits = 0
  )
}

shinyApp(ui, server)
