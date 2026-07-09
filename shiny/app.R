# app.R
# R 4.0-compatible Shiny app

options(shiny.fullstacktrace = TRUE)

library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(scales)
library(bslib) 

# ---- load objects ----

powerdat <- readRDS(here("results", "powerdat.rds"))
powerdat_summ <- readRDS(here("results", "powerdat_summ.rds"))

# ---- cleanup ----

powerdat$error_sd <- as.numeric(powerdat$error_sd)
powerdat_summ$error_sd <- as.numeric(powerdat_summ$error_sd)

errors <- sort(unique(powerdat_summ$error_sd))

get_error_id <- function(x) {
  sapply(x, function(z) which.min(abs(errors - z)))
}

powerdat <- powerdat %>%
  mutate(
    error_id = get_error_id(error_sd),
    error_lab = paste0("SD = ", signif(errors[error_id], 2))
  )

powerdat_summ <- powerdat_summ %>%
  mutate(
    error_id = get_error_id(error_sd),
    error_lab = paste0("SD = ", signif(errors[error_id], 2))
  )

default_error_id <- ifelse(length(errors) >= 2, 2, 1)

# ---- UI ----

ui <- fluidPage(
  
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("Source Sans 3")
  ),
  
  titlePanel("Interactive plots from Fredston 2026 Measuring the edges of species' geographic ranges, Methods in Ecology and Evolution"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "error_id",
        "Residual standard deviation",
        choices = setNames(
          as.character(seq_along(errors)),
          paste0("SD = ", signif(errors, 2))
        ),
        selected = as.character(default_error_id)
      ),
      
      helpText("Click the heatmap to choose a shift rate and time-series length."),
      
      hr(),
      h4("Selected values"),
      tableOutput("selected_values")
    ),
    
    mainPanel(
      h3("Power across parameter space"),
      p("Each cell represents statistical power, calculated across simulation iterations. After simulating a 100-year time-series with a given rate of range edge shift and residual standard deviation, I subsampled the time-series into windows of different lengths to explore under what parameter combinations a study had sufficient power to detect a true trend. Power was defined as the proportion of simulations in which a linear regression detected a significant positive shift in latitude over time. Simulations were run with a range of edge shift rates, residual standard deviations, and window lengths."),
      plotOutput("power_heatmap", height = "360px", click = "heatmap_click"),
      
      hr(),
      
      h3("Power across simulation iterations"),
      p("Relationship between time-series length and statistical power for a selected edge shift rate and residual standard deviation. Each line is an iteration of the simulation, which was run 100 times for every parameter combination."),
      plotOutput("power_iters", height = "360px")
    )
  )
)

# ---- server ----

server <- function(input, output, session) {
  
  selected <- reactiveValues(
    shiftrate = 0.05,
    ts_length = 50
  )
  
  observeEvent(input$heatmap_click, {
    selected$shiftrate <- input$heatmap_click$x
    selected$ts_length <- input$heatmap_click$y
  })
  
  selected_error_id <- reactive({
    as.integer(input$error_id)
  })
  
  selected_error_sd <- reactive({
    errors[selected_error_id()]
  })
  
  selected_shiftrate <- reactive({
    vals <- sort(unique(powerdat$shiftrate))
    vals[which.min(abs(vals - selected$shiftrate))]
  })
  
  selected_ts_length <- reactive({
    vals <- sort(unique(powerdat$ts_length))
    vals[which.min(abs(vals - selected$ts_length))]
  })
  
  selected_power <- reactive({
    dat <- powerdat_summ %>%
      filter(
        error_id == selected_error_id(),
        shiftrate == selected_shiftrate(),
        ts_length == selected_ts_length()
      )
    
    if (nrow(dat) == 0) {
      return(NA_real_)
    } else {
      return(dat$power[1])
    }
  })
  
  output$selected_values <- renderTable({
    data.frame(
      `Residual standard deviation (°lat)` = signif(selected_error_sd(), 3),
      `Shift rate (°lat/yr)` = selected_shiftrate(),
      `Time-series length (yr)` = round(selected_ts_length(), 0),
      `Power shown in heatmap` = round(selected_power(), 3),
      check.names = FALSE
    )
  })
  
  output$power_heatmap <- renderPlot({
    dat <- powerdat_summ %>%
      filter(error_id == selected_error_id())
    
    req(nrow(dat) > 0)
    
    selected_point <- data.frame(
      shiftrate = selected_shiftrate(),
      ts_length = selected_ts_length()
    )
    
    ggplot(dat, aes(x = shiftrate, y = ts_length, fill = power)) +
      geom_tile() +
      geom_point(
        data = selected_point,
        aes(x = shiftrate, y = ts_length),
        inherit.aes = FALSE,
        shape = 21,
        size = 4,
        stroke = 1.2
      ) +
      theme_bw() +
      scale_x_continuous(breaks = seq(0, 0.1, 0.02)) +
      scale_fill_gradientn(
        colours = c("#a50026", "#ffffbf", "#313695"),
        values  = scales::rescale(c(0, 0.8, 1)),
        limits  = c(0, 1),
        breaks  = seq(0, 1, 0.2),
        labels  = seq(0, 1, 0.2),
        na.value = "grey50"
      ) +
      labs(
        x = "Range edge shift rate (°lat/yr)",
        y = "Time-series length",
        fill = "Power"
      ) +
      theme(
        legend.position = "right",
        legend.direction = "vertical"
      )
  })
  
  output$power_iters <- renderPlot({
    dat <- powerdat %>%
      filter(
        error_id == selected_error_id(),
        shiftrate == selected_shiftrate()
      )
    
    req(nrow(dat) > 0)
    
    ggplot(dat, aes(x = ts_length, y = power, group = iter)) +
      geom_hline(yintercept = 0.8, color = "black", linetype = "dashed", size = 1.2) +
      geom_line(alpha = 0.2, color = "grey30") +
      geom_vline(xintercept = selected_ts_length(), linetype = "dotted") +
      scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
      theme_bw() +
      labs(
        x = "Time-series length (years)",
        y = "Power",
        subtitle = paste0(
          "Range edge shift rate = ", selected_shiftrate(),
          " °lat/yr; Residual standard deviation = ",
          signif(selected_error_sd(), 2)
        )
      ) + 
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
