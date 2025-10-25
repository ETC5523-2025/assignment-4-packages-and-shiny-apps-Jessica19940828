# Shiny app for the ausbushfire package
# File path: inst/app/app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

# --- 1. Define UI (User Interface) ---
ui <- fluidPage(
  # Apply a theme using bslib
  theme = bslib::bs_theme(bootswatch = "darkly"),

  titlePanel("Australian Bushfire Weather Risk Explorer"),
  p("Data source: van Oldenborgh et al. (2021) "),

  sidebarLayout(
    sidebarPanel(
      h4("Interactive Options"),

      # Interactive selector
      selectInput("metric_select",
                  label = "1. Select a fire weather metric to display:",
                  choices = c("Fire Weather Index (FWI7x-SM)" = "fwi_sm",
                              "Monthly Severity Rating (MSR-SM)" = "msr_sm")),

      hr(),

      # Field descriptions
      h5("Metric Descriptions"),
      strong("FWI7x-SM:"),
      p("The highest 7-day mean FWI during the fire season. This metric measures fire weather 'intensity'."),
      strong("MSR-SM:"),
      p("The maximum Monthly Severity Rating during the fire season. This metric reflects fire 'duration' and suppression difficulty.")
    ),

    mainPanel(
      # Output changes based on input
      plotOutput("timeSeriesPlot"),

      hr(),

      # Interpretation of output (fulfills "how to interpret" requirement)
      h5("How to Interpret This Plot"),
      p("This plot shows the observed data from 1979 to 2019 (representing the 2019/20 fire season). The dashed line represents the overall linear trend."),
      p("You can clearly see the value for 2019 (red point) is far higher than any previous record and well above the trend line."),
      p("This aligns with the paper's conclusion: anthropogenic climate change has significantly increased the risk of such extreme fire weather events.")
    )
  )
)

# --- 2. Define Server (Server Logic) ---
server <- function(input, output) {

  # Load data from the package
  # Note: In app.R, we use `data()` to load the package data
  data(ausbushfire_data, package = "ausbushfire")

  # Create a new data frame to highlight the 2019 data point
  plot_data <- reactive({
    ausbushfire_data %>%
      mutate(highlight = if_else(year == 2019, "2019/20 Season", "Other Years"))
  })

  # Create a reactive plot title
  plot_title <- reactive({
    if (input$metric_select == "fwi_sm") {
      "Fire Weather Index (FWI7x-SM), 1979-2019"
    } else {
      "Monthly Severity Rating (MSR-SM), 1979-2019"
    }
  })

  # Create a reactive Y-axis label
  y_label <- reactive({
    if (input$metric_select == "fwi_sm") {
      "FWI Value [1]"
    } else {
      "MSR Value [1]"
    }
  })

  # Render the plot
  output$timeSeriesPlot <- renderPlot({

    ggplot(plot_data(), aes(x = year, y = .data[[input$metric_select]])) +
      # Add a linear trend line to help guide the user to a conclusion
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey70", fullrange = TRUE) +
      geom_line(color = "grey80") +
      # Highlight the 2019 point
      geom_point(aes(color = highlight, size = highlight), alpha = 0.8) +
      scale_color_manual(values = c("2019/20 Season" = "#FF5733", "Other Years" = "#00AEEF")) +
      scale_size_manual(values = c("2019/20 Season" = 5, "Other Years" = 3)) +
      labs(
        title = plot_title(),
        x = "Year",
        y = y_label(),
        color = "Fire Season"
      ) +
      theme_dark(base_size = 16) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
}

# --- 3. Run the Application ---
shinyApp(ui = ui, server = server)
