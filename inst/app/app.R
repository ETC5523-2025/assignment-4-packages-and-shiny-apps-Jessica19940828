# -------------------------------------------------
#  Shiny app for the ausbushfire package
# -------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(leaflet)
library(thematic)
library(bsicons)
library(scales)
library(tidyr)
library(glue)
library(sf)

# ---------- Load in data ----------
if (interactive()) {
  devtools::load_all(".")
} else {
  data(ausbushfire_data, package = "ausbushfire")
}

# ---------- 預先計算靜態物件（在 server 外） ----------
# 1. Standardize
ausbushfire_data_scaled <- ausbushfire_data %>%
  mutate(
    fwi_sm_scaled      = (fwi_sm      - mean(fwi_sm,      na.rm = TRUE)) / sd(fwi_sm,      na.rm = TRUE),
    msr_sm_scaled      = (msr_sm      - mean(msr_sm,      na.rm = TRUE)) / sd(msr_sm,      na.rm = TRUE),
    tmax_scaled        = (tmax        - mean(tmax,        na.rm = TRUE)) / sd(tmax,        na.rm = TRUE),
    precip_total_scaled= (precip_total- mean(precip_total,na.rm = TRUE)) / sd(precip_total,na.rm = TRUE),
    iod_mean_scaled    = (iod_mean    - mean(iod_mean,    na.rm = TRUE)) / sd(iod_mean,    na.rm = TRUE),
    sam_mean_scaled    = (sam_mean    - mean(sam_mean,    na.rm = TRUE)) / sd(sam_mean,    na.rm = TRUE),
    area_burned_scaled = ifelse(is.na(area_burned), NA,
                                (area_burned - mean(area_burned, na.rm = TRUE)) /
                                  sd(area_burned, na.rm = TRUE))
  )

# 2. Long-term mean value for value_box
averages <- ausbushfire_data %>%
  summarise(across(
    c(fwi_sm, msr_sm, tmax, precip_total, iod_mean, sam_mean, area_burned),
    ~ mean(., na.rm = TRUE),
    .names = "avg_{.col}"
  )) %>% as.data.frame()

# 3. Build up the study region
study_region_sf <- st_sf(
  geometry = st_sfc(st_polygon(list(
    matrix(c(144, -40,
             155, -40,
             155, -29,
             144, -29,
             144, -40), ncol = 2, byrow = TRUE)
  ))),
  crs = 4326
)


# 4. Burned area range
min_area <- min(ausbushfire_data$area_burned, na.rm = TRUE)
max_area <- max(ausbushfire_data$area_burned, na.rm = TRUE)
if (is.infinite(min_area) || is.infinite(max_area) || min_area == max_area) {
  min_area <- 0; max_area <- 1
}


# -------------------------------------------------
#  UI
# -------------------------------------------------
ui <- navbarPage(
  title = "Australian Bushfire Risk: The Climate Change Story",
  theme = bslib::bs_theme(bootswatch = "lumen"),

  # ---------- Tab 1 ----------
  tabPanel(
    "Risk Map",
    fillPage(
      padding = c(10,10,10,10),
      absolutePanel(
        top = 50, left = 10, width = 300,
        style = "background: #f0f0f0; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
        h4("Did you know?"),
        p("In 2019-2020, the Black Summer bushfires burned over 18 million hectares in southeastern Australia,
          with climate change increasing the risk by at least 30%."),
        p("This app explores fire weather trends and climate drivers from 1979-2019."),
      ),
      h4("Research Area: Southeastern Australia Fire Risk"),
      p("Map shows the study region. Red intensity represents burned area (from 1997)."),
      leafletOutput("studyMapInteractive", height = "70vh"),
      absolutePanel(
        top = 100, right = 10, width = 500,
        style = "background: white; padding: 10px; border-radius: 5px; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
        h5("Select Year:"),
        sliderInput("highlight_year_map", "", min = 1997, max = 2019, value = 2019, step = 1, sep = "", animate = animationOptions(interval = 800))
      ),
      layout_column_wrap(
        width = 1/2,
        uiOutput("map_stats_fwi_v11"),
        uiOutput("map_stats_area_v11")
      )
    )
  ),

  # ---------- Tab 2 ----------
  tabPanel(
    "Explore Trends",
    sidebarLayout(
      sidebarPanel(
        h4("Interactive Controls"),
        sliderInput("year_filter_tab2","Select Year Range:",
                    min = 1979, max = 2019,
                    value = c(1979,2019), step = 1, sep = ""),
        hr(),
        checkboxGroupInput(
          "selected_metrics",
          "Select Metrics",
          choices = c(
            "FWI (Overall Fire Risk))" = "fwi_sm",
            "MSR (Prolonged Fire Risk)" = "msr_sm",
            "Tmax (Extreme Heat)" = "tmax",
            "Precip (Drought Condition)" = "precip_total",
            "IOD (Drought Driver)" = "iod_mean",
            "SAM (Wind & Drought Driver))" = "sam_mean",
            "Area Burned (Actual Fire Impact))" = "area_burned"
          ),
          selected = "fwi_sm"
        ),
        hr(),
        checkboxInput("show_ci_tab2","Show Trend Line (with Confidence Interval)", value = FALSE),
        hr(),
        h5(textOutput("highlight_year_text")),
        uiOutput("dynamic_value_boxes")
      ),
      mainPanel(
        h4("Time Series Comparison"),
        p("This chart shows the selected metrics over time"),
        plotOutput("combinedTimeSeriesPlot", height = "400px"),
        hr(),
        h4("Relationship Between Metrics"),
        fluidRow(
          column(6, selectInput("scatter_x","X Axis:", choices = names(ausbushfire_data)[-1], selected = "fwi_sm")),
          column(6, selectInput("scatter_y","Y Axis:", choices = names(ausbushfire_data)[-1], selected = "area_burned"))
        ),
        plotOutput("scatterPlot", height = "400px")
      )
    )
  ),

  # ---------- Tab 3 ----------
  tabPanel(
    "About this Site",
    h4("Why This App?"),
    p("This app is based on the 2021 paper 'Attribution of the Australian bushfire risk to anthropogenic climate change'..."),
    hr(),
    h4("Data"),
    p("All data from van Oldenborgh et al. (2021) and KNMI Climate Explorer."),
    downloadButton("downloadData_about","Download Full Data (.csv)"),
    hr(),
    h4("Creator"), p("HengHsieh Chang"),
    hr(),
    h4("Session Info"), verbatimTextOutput("sessionInfo")
  )
)

# -------------------------------------------------
#  SERVER
# -------------------------------------------------
server <- function(input, output, session) {

  thematic::thematic_shiny(font = "auto")

  # ----- Global setting -----
  theme_app <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3, color = "grey85"),
        legend.position = "bottom", legend.title = element_blank()
      )
  }
  highlight_color <- "#FF5733"
  base_color      <- "#007bff"
  highlight_size  <- 4
  base_size       <- 2.5

  # ---------- Tab 1 ----------
  map_year_data <- reactive({
    ausbushfire_data %>% filter(year == input$highlight_year_map)
  })

  output$studyMapInteractive <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 149.5, lat = -34.5, zoom = 5) %>%
      addMarkers(
        lng = c(151.2, 149.1, 144.9),
        lat = c(-33.8, -35.2, -37.8),
        popup = c("Sydney: Major city in fire-prone area",
                  "Canberra: Capital, affected by 2019 fires",
                  "Melbourne: High population density"),
        options = markerOptions(riseOnHover = TRUE)
      ) %>%
      addPolygons(
        data = study_region_sf,
        fillColor = "red", color = "darkred", weight = 2,
        fillOpacity = 0.4, layerId = "region",
        label = "Southeastern Australia Study Region") %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric("Reds", domain = c(min_area, max_area)),
        values = c(min_area, max_area),
        title = "Burned Area Intensity (km²)",
        opacity = 0.8
      )
  })

  observe({
    req(input$highlight_year_map)
    d <- ausbushfire_data %>% filter(year == input$highlight_year_map)

    if (nrow(d) == 0) return()

    # Transparent
    opacity <- 0.1
    if (d$year >= 1997 && !is.na(d$area_burned)) {
      scaled <- (d$area_burned - min_area) / (max_area - min_area)
      scaled <- pmax(0, pmin(1, scaled))
      opacity <- 0.1 + scaled * 0.7
    }

    # popup
    popup_txt <- glue(
      "<b>Southeastern Australia ({d$year})</b><br>",
      "FWI: {round(d$fwi_sm, 1)}<br>",
      "Burned Area: {ifelse(d$year < 1997 || is.na(d$area_burned), 'N/A', comma(round(d$area_burned, 0)))} km²"
    )
    leafletProxy("studyMapInteractive") %>%
      clearShapes() %>%
      addPolygons(
        data = study_region_sf,
        fillColor = "red", color = "darkred", weight = 2,
        fillOpacity = opacity, layerId = "region"
      ) %>%
      clearPopups() %>%
      addPopups(lng = 149.5, lat = -34.5, popup = popup_txt)
  })


  output$map_stats_fwi_v11 <- renderUI({
    d <- map_year_data()
    bslib::value_box(
      title = paste(d$year,"FWI"),
      value = round(d$fwi_sm,1),
      p(paste("Long-term Average:",round(averages$avg_fwi_sm,1))),
      theme = if(d$fwi_sm > averages$avg_fwi_sm) "danger" else "primary"
    )
  })

  output$map_stats_area_v11 <- renderUI({
    d <- map_year_data()
    if (d$year < 1997 || is.na(d$area_burned))
      return(bslib::value_box(title = paste(d$year,"Burned Area"), value = "N/A",
                              p("Data starts from 1997"), theme = "secondary"))
    bslib::value_box(
      title = paste(d$year,"Burned Area (km²)"),
      value = scales::comma(round(d$area_burned,0)),
      p(paste("Long-term Average:",scales::comma(round(averages$avg_area_burned,0)))),
      theme = "secondary"
    )
  })

  # ---------- Tab 2 ----------
  output$highlight_year_text <- renderText({
    paste("Key Data (", input$highlight_year_map, "Year):")
  })

  output$dynamic_value_boxes <- renderUI({
    req(input$selected_metrics, input$highlight_year_map)
    yr_data <- ausbushfire_data %>% filter(year == input$highlight_year_map)

    metric_names <- c(
      fwi_sm = "FWI ((Overall Fire Risk)", msr_sm = "MSR (Prolonged Fire Risk)",
      tmax = "Tmax (Extreme Heat)", precip_total = "Precip (Drought Condition)",
      iod_mean = "IOD (Drought Driver)", sam_mean = "SAM (Wind & Drought Driver)",
      area_burned = "Area Burned (Actual Fire Impact))"
    )

    boxes <- lapply(input$selected_metrics, function(m) {
      disp_name <- metric_names[m]
      val <- yr_data[[m]]
      avg_col <- paste0("avg_", m)
      avg <- averages[[avg_col]]

      if (m == "area_burned") {
        val_txt <- ifelse(is.na(val) || input$highlight_year_map < 1997,
                          "N/A", scales::comma(round(val,0)))
        avg_txt <- ifelse(is.na(avg), "", paste("Average:", scales::comma(round(avg,0))))
      } else {
        val_txt <- ifelse(is.na(val), "N/A", round(val,1))
        avg_txt <- ifelse(is.na(avg), "", paste("Average:", round(avg,1)))
      }

      bslib::value_box(
        title = paste(input$highlight_year_map, disp_name),
        value = val_txt,
        p(avg_txt),
        theme = "info", height = "150px"
      )
    })
    tagList(boxes)
  })

  # ----- Time Series plot -----
  output$combinedTimeSeriesPlot <- renderPlot({
    req(input$selected_metrics)
    scaled_cols <- paste0(input$selected_metrics, "_scaled")
    plot_data <- ausbushfire_data_scaled %>%
      filter(year >= input$year_filter_tab2[1],
             year <= input$year_filter_tab2[2]) %>%
      select(year, all_of(scaled_cols)) %>%
      pivot_longer(cols = -year,
                   names_to = "Metric",
                   values_to = "Value") %>%
      mutate(Metric = gsub("_scaled$", "", Metric))%>%
      filter(!is.na(Value), !is.infinite(Value))

    if (nrow(plot_data) == 0) return(NULL)

    p <- ggplot(plot_data, aes(x = year, y = Value, colour = Metric)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      labs(x = "Year", y = "Standardized Value") +
      theme_app()

    if (input$show_ci_tab2) {
      p <- p + geom_smooth(aes(group = Metric), method = "lm",
                           se = TRUE, linetype = "dashed", linewidth = 0.5, na.rm = TRUE)
    }
    p
  })

  # ----- Scatter Plot -----
  output$scatterPlot <- renderPlot({
    req(input$scatter_x, input$scatter_y)
    filtered <- ausbushfire_data %>%
      filter(year >= input$year_filter_tab2[1],
             year <= input$year_filter_tab2[2]) %>%
      mutate(highlight = ifelse(year == input$highlight_year_map,
                                "Highlighted Year", "Other Years"))%>%
      filter(!is.na(.data[[input$scatter_x]]), !is.na(.data[[input$scatter_y]]))

    if (nrow(filtered) == 0) return(NULL)

    p <- ggplot(filtered,
                aes(x = .data[[input$scatter_x]],
                    y = .data[[input$scatter_y]])) +
      geom_point(data = filter(filtered, highlight == "Other Years"),
                 alpha = 0.5, size = base_size, colour = base_color) +
      geom_point(data = filter(filtered, highlight == "Highlighted Year"),
                 alpha = 0.9, size = highlight_size, colour = highlight_color) +
      geom_smooth(method = "lm", se = input$show_ci_tab2,
                  colour = "black", linetype = "dashed") +
      labs(title = paste(input$scatter_y, "vs", input$scatter_x),
           x = input$scatter_x, y = input$scatter_y) +
      theme_app()
    p
  })

  # ---------- Tab 3 ----------
  output$downloadData_about <- downloadHandler(
    filename = function() { paste0("ausbushfire_data_", Sys.Date(), ".csv") },
    content  = function(file) { write.csv(ausbushfire_data, file, row.names = FALSE) }
  )
  output$sessionInfo <- renderPrint({ sessionInfo() })
}

# -------------------------------------------------
shinyApp(ui = ui, server = server)
