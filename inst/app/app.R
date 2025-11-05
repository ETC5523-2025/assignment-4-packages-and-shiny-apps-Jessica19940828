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
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
  }
}

if (!exists("ausbushfire_data", envir = globalenv())) {
  data("ausbushfire_data", package = "ausbushfire", envir = globalenv())
}

# ---------- Outside of server ----------
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
    matrix(c(155, -29, # 29 S, 155 E
             150, -29, # 29 S, 150 E
             144, -40, # 40 S, 144 E
             155, -40, # 40 S, 155 E
             155, -29), ncol = 2, byrow = TRUE)
  ))),
  crs = 4326
)


# 4. Burned area range
min_area <- min(ausbushfire_data$area_burned, na.rm = TRUE)
max_area <- max(ausbushfire_data$area_burned, na.rm = TRUE)
if (is.infinite(min_area) || is.infinite(max_area) || min_area == max_area) {
  min_area <- 0; max_area <- 1
}

#  5. Define legend level
area_bins <- c(0, 1000, 5000, 10000, max_area + 1)
area_pal <- colorBin("Reds", domain = c(0, max_area), bins = area_bins)

# 6. Define metrics name and colour
metric_names_map <- c(
  fwi_sm = "FWI (Overall Fire Risk)",
  msr_sm = "MSR (Prolonged Fire Risk)",
  tmax = "Tmax (Extreme Heat)",
  precip_total = "Precip (Drought Condition)",
  iod_mean = "IOD (Drought Driver)",
  sam_mean = "SAM (Wind & Drought Driver)",
  area_burned = "Area Burned (Actual Fire Impact)"
)

metric_colors <- c(
  fwi_sm = "#EE3B3B",
  msr_sm = "#FF7F24",
  tmax = "#EEAD0E",
  precip_total = "#59B8D8",
  iod_mean = "#60B68C",
  sam_mean = "#A48EE0",
  area_burned = "#8B3A62"
)

NAVBAR_BG_COLOR <- "#004D60"
NAVBAR_FG_COLOR <- "#FFFFFF"
SIDEBAR_BG_COLOR <- "#004D60"

# -------------------------------------------------
#  UI
# -------------------------------------------------
ui <- navbarPage(
  title = "Australian Bushfire Risk Explorer",
  header = tagList(withMathJax()),
  theme = bslib::bs_theme(
    bootswatch = "litera",
    primary = "#B71C1C",
    secondary = "#495057",
    danger = "#FF9800",
    base_font = font_google("Inter"),
    fg = "#333333",
    bg = "#F8F9FA",
    navbar_bg = NAVBAR_BG_COLOR,
    navbar_fg = NAVBAR_FG_COLOR,
    `navbar-padding-y` = "1rem",
    `navbar-link-padding-y` = "0.75rem",
    `navbar-link-padding-x` = "3rem",
    navbar_active_link_bg = "transparent",
    navbar_active_link_color = NAVBAR_FG_COLOR,
    `navbar-link-active-border-bottom` = paste0("3px solid #FFC400"),
    `navbar-link-hover-color` = "rgba(255, 255, 255, 0.75)"
  ),

  # ---------- Tab 1 ----------
  tabPanel(
    title = span(bsicons::bs_icon("fire"), "Fire Risk Map"),
    fillPage(
      padding = c(10,10,10,10),
      absolutePanel(
        top = "30vh", left = 10, width = 350,
        style = "
          background: #FFFFFF;
          padding: 15px;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
          z-index: 1000;
          color: #333333;
        ",
        h4("The Black Summer"),
        p("The 2019-2020 Black Summer bushfires burned over 18 million hectares— one of the most devastating fire seasons on record. Climate change has increased fire risk by nearly 30%, intensifying both frequency and severity."),
        p("Use this explorer to understand how such extreme events fit into broader trends from 1979–2019."),
      ),
      h4("Southeastern Australia Fire Risk Trends"),
      p("Explore the dynamic map to visualize historical bushfire risks across southeastern Australia.Use the year slider to view changes in burned area and FWI over time."),
      leafletOutput("studyMapInteractive", height = "65vh"),
      absolutePanel(
        top = 100, right = 10, width = 350,
        style = "
          background: #FFFFFF;
          padding: 10px 15px;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
          color: #333333;
        ",
        h5("Select Year:"),
        sliderInput("highlight_year_map", "", min = 1997, max = 2018, value = 2018, step = 1, sep = "",
                    animate = animationOptions(interval = 800))
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
    title = span(bsicons::bs_icon("graph-up"), "Explore Trends"),
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = glue::glue("background-color: {SIDEBAR_BG_COLOR};
                           border-right: 1px solid {SIDEBAR_BG_COLOR};
                           overflow-y: auto;
                           padding-top: 15px;
                           color: {NAVBAR_FG_COLOR};"),

        h4("Interactive Controls", style = glue::glue("color: {NAVBAR_FG_COLOR}")),
        sliderInput("year_filter_tab2","Select Year Range:",
                    min = 1979, max = 2019,
                    value = c(1979,2019), step = 1, sep = ""),
        hr(style = "border-top: 1px solid #FFFFFF;"),
        checkboxGroupInput(
          "selected_metrics",
          "Select Metrics",
          choices = metric_names_map,
          selected = metric_names_map[c("fwi_sm", "msr_sm")]
        ),
        hr(style = "border-top: 1px solid #FFFFFF;"),
        checkboxInput("show_ci_tab2","Show Trend Line (with 95% Confidence Interval)", value = FALSE),

        hr(style = "border-top: 1px solid #FFFFFF;"),
        h5("Interpreting Trends:", style = "color: #FFC400;"),
        p(
          "Observe the inter-annual variability and long-term trends for the selected metrics (e.g., FWI or Tmax). If 'Show Trend Line' is checked, the slope of the trend line and the 95% confidence interval (gray area) help determine if the climate risk has significantly increased, decreased, or remained stable over the selected period.",
          style = "font-size: 0.85rem;"
        ),
        hr(style = "border-top: 1px solid #FFFFFF;"),
        p(tags$small("Click ", tags$a(href="#", onclick = "Shiny.setInputValue('switchTab', 4, {priority: 'event'})", "here for detailed definitions and units"), " of all climate metrics."),
          style = glue::glue("color: {NAVBAR_FG_COLOR}; opacity: 0.8;"))
      ),

      mainPanel(
        width = 9,
        style = "overflow-y: auto; height: 90vh; background-color: #F8F9FA;",
        h4("Time Series Comparison: Long-Term Trends", style = "margin-bottom: 0.5rem;"),
        p("Show the annual changes for selected metrics over the study period."),
        plotOutput("combinedTimeSeriesPlot", height = "450px"),
        hr(style = "border-top: 2px solid #DDDDDD; margin: 2rem 0;"),
        h4("", style = "margin-bottom: 0.5rem;"),
        uiOutput("dynamic_value_boxes")

      )
    )
  ),

  # ---------- Tab 3 ----------
  tabPanel(
    title = span(bsicons::bs_icon("graph-up-arrow"), "Correlation Analysis"),
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = glue::glue("background-color: {SIDEBAR_BG_COLOR};
                           border-right: 1px solid {SIDEBAR_BG_COLOR};
                           overflow-y: auto;
                           padding-top: 15px;
                           color: {NAVBAR_FG_COLOR};"),

        h4("Metrics Selection", style = glue::glue("color: {NAVBAR_FG_COLOR}")),
        p("Analyze the relationship between any two annual metrics over the selected period.", style = glue::glue("color: {NAVBAR_FG_COLOR}; opacity: 0.8;")),
        hr(style = "border-top: 1px solid #FFFFFF;"),

        selectInput("scatter_x","X Axis (Driver / Index):", choices = metric_names_map, selected = metric_names_map["fwi_sm"]),
        selectInput("scatter_y","Y Axis (Impact / Outcome):", choices = metric_names_map, selected = metric_names_map["area_burned"]),

        hr(style = "border-top: 1px solid #FFFFFF;"),

        checkboxInput("show_ci_tab3","Show Trend Line (with 95% Confidence Interval)", value = FALSE),
        p(tags$small("Note: Correlation is calculated based on the year range selected in 'Explore Trends' tab."), style = glue::glue("color: {NAVBAR_FG_COLOR}; opacity: 0.6;")),

        hr(style = "border-top: 1px solid #FFFFFF;"),
        h5("Interpreting Correlation:", style = "color: #FFC400;"),
        p(
          "Visualize the linear correlation between a selected driver (X-axis) and a chosen outcome (Y-axis). The large red dot highlights the most recent year in the range. The closer the points align to a straight line, the stronger the correlation. An upward-sloping line indicates a positive correlation (e.g., higher temperature leads to greater burned area); a downward slope indicates a negative correlation (e.g., more precipitation leads to less burned area).",
          style = "font-size: 0.85rem;"
        )
      ),
      mainPanel(
        width = 9,
        style = "overflow-y: auto; height: 90vh; background-color: #F8F9FA;",
        h4("Relationship Between Metrics: Correlation Analysis", style = "margin-bottom: 0.5rem;"),
        p("Visualize the linear correlation between two selected metrics."),
        plotOutput("scatterPlot", height = "700px")
      )
    )
  ),


  # ---------- Tab 4 ----------
  tabPanel(
    title = span(bsicons::bs_icon("info-circle"), "About"),
    div(
      class = "container p-4",
      style = "height: 90vh; overflow-y: auto; padding-right: 15px;",
      h3("Project Overview: Australian Bushfire Risk Attribution"),
      p("This interactive application explores the historical relationship between climate drivers and fire weather risk in Southeastern Australia, covering the period from 1979 to 2019. The analysis is built upon data and methodologies used in climate attribution science, particularly focusing on the role of anthropogenic climate change in increasing fire severity, such as the 2019–2020 Black Summer events."),

      h4("Data and Methodology"),
      tags$ul(
        tags$li(
          strong("Study Region:"),
          uiOutput("study_region_text")),
        tags$li(
          strong("Key Metrics:"),
          " The application tracks annual maximums of the Fire Weather Index (FWI) and Monthly Severity Rating (MSR), alongside major climate drivers including Maximum Temperature (Tmax), Precipitation, Indian Ocean Dipole (IOD), and Southern Annular Mode (SAM)."),
        tags$li(
          strong("Data Source:"),
          " All data is derived from the",
          tags$a(href = 'https://climexp.knmi.nl', 'KNMI Climate Explorer'),
          " and related peer-reviewed literature."),
        tags$li(
          strong("Primary Reference:"),
          "van Oldenborgh, G. J., Krikken, F., Lewis, S., et al. (2021). ",
          em("Attribution of the Australian bushfire risk to anthropogenic climate change."),
          " *Natural Hazards and Earth System Sciences*, 21, 941–960.")
      ),

      h4("Technical Information"),
      p("You can download the full processed dataset used in this analysis:"),
      downloadButton("downloadData_about","Download Full Data (.csv)"),
      hr(),
      h4("Field Descriptions"),
      p("The following table describes each field included in the dataset used by this application."),
      tableOutput("field_description"),
      hr(),
      h5("Session Details"), verbatimTextOutput("sessionInfo"),
      hr(),
      p("Developed by ", strong("Heng-Hsieh Chang"), ". ",
        "For questions or collaboration inquiries, please contact the project author.")
    )
  )
)

# -------------------------------------------------
#  SERVER
# -------------------------------------------------
server <- function(input, output, session) {

  thematic::thematic_shiny(
    font = "auto",
    bg = "transparent",
    fg = "#333333"
  )

  # ----- Global setting -----
  theme_app <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3, color = "grey85"),
        legend.position = "bottom", legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(size = 12)
      )
  }
  highlight_color <- "#B71C1C" # Primary Red
  base_color      <- "#495057" # Secondary Grey
  highlight_size  <- 4
  base_size       <- 2.5

  area_bins <- c(0, 1000, 5000, 10000, max_area + 1)
  area_pal <- colorBin("Reds", domain = c(0, max_area), bins = area_bins)

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
        options = markerOptions(riseOnHover = TRUE),
        group = "cities"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = area_pal,
        values = c(0, max_area),
        title = "Burned Area (km²)",
        opacity = 0.8,
        layerId = "area_legend"
      )
  })

  observe({
    req(input$highlight_year_map)
    d <- map_year_data()

    if (nrow(d) == 0) return()

    leafletProxy("studyMapInteractive") %>%
      removeShape(layerId = "dynamic_region") %>%
      clearPopups()

    area_val <- d$area_burned
    popup_txt <- glue(
      "<b>Southeastern Australia ({d$year})</b><br>",
      "FWI: {round(d$fwi_sm, 1)}(Fire Risk)<br>",
      "Burned Area: {ifelse(d$year < 1997 || is.na(area_val), 'N/A', comma(round(area_val, 0)))} km²"
    )

    fill_color <- if (d$year >= 1997 && !is.na(area_val)) area_pal(area_val) else "#CCCCCC"

    opacity <- 0.1
    if (d$year >= 1997 && !is.na(d$area_burned)) {
      fixed_min <- 1000
      fixed_max <- 20000
      scaled <- (area_val - fixed_min) / (fixed_max - fixed_min)
      scaled <- pmax(0, pmin(1, scaled))
      opacity <- 0.1 + scaled * 0.7

    } else {
      fill_color <- "#EEEEEE"
      opacity <- 0.1
    }

    leafletProxy("studyMapInteractive") %>%
      addPolygons(
        data = study_region_sf,
        fillColor = fill_color,
        color = "#B71C1C",
        weight = 3,
        fillOpacity = opacity,
        layerId = "dynamic_region",
        label = "Southeastern Australia Study Region"
      )


    # popup
    leafletProxy("studyMapInteractive") %>%
      addPopups(lng = 149.5, lat = -34.5, popup = popup_txt)
  })


  output$map_stats_fwi_v11 <- renderUI({
    d <- map_year_data()
    theme_status <- if(d$fwi_sm > averages$avg_fwi_sm) "primary" else "danger"

    bslib::value_box(
      title = paste(d$year,"FWI (Fire Weather Index)"),
      value = round(d$fwi_sm,1),
      p(paste("Long-term Average:",round(averages$avg_fwi_sm,1)), style = "font-size: 20px;"),
      theme = theme_status,
      height = "140px"
    )
  })

  output$map_stats_area_v11 <- renderUI({
    d <- map_year_data()
    if (d$year < 1997 || is.na(d$area_burned))
      return(bslib::value_box(title = paste(d$year,"Burned Area"), value = "N/A",
                              p("Data starts from 1997", style = "font-size: 20px;"),
                              theme = "secondary",
                              height = "140px"))
    bslib::value_box(
      title = paste(d$year,"Burned Area (km²)"),
      value = scales::comma(round(d$area_burned,0)),
      p(paste("Long-term Average:",scales::comma(round(averages$avg_area_burned,0))), style = "font-size: 20px;"),
      theme = "secondary",
      height = "140px"
    )
  })

  # ---------- Tab 2 ----------

  output$dynamic_value_boxes <- renderUI({
    req(input$selected_metrics, input$year_filter_tab2)

    current_year <- input$year_filter_tab2[2]
    yr_data <- ausbushfire_data %>% filter(year == current_year)

    selected_labels <- input$selected_metrics

    selected_keys <- names(metric_names_map)[metric_names_map %in% selected_labels]

    boxes <- lapply(selected_keys, function(m) {

      disp_name <- metric_names_map[m]
      color_code <- metric_colors[m]

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
        title = paste(current_year, disp_name),
        value = val_txt,
        p(avg_txt, style = "font-size: 14px;"),
        fill = TRUE,
        class = "text-white",
        style = glue::glue("background-color: {color_code}; border-color: {color_code};"),
        height = "130px",
        width = NULL
      )
    })

    fluidRow(
      lapply(boxes, function(box) {
        column(
          width = floor(12 / max(length(boxes), 1)),
          box
        )
      })
    )
  })


  # ----- Time Series plot -----
  output$combinedTimeSeriesPlot <- renderPlot({
    req(input$selected_metrics)

    selected_labels <- input$selected_metrics
    selected_metrics_cols <- names(metric_names_map)[metric_names_map %in% input$selected_metrics]
    scaled_cols <- paste0(selected_metrics_cols, "_scaled")

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

    plot_colors <- metric_colors[selected_metrics_cols]

    names(plot_colors) <- selected_metrics_cols

    plot_labels <- metric_names_map[selected_metrics_cols]

    p <- ggplot(plot_data, aes(x = year, y = Value, colour = Metric)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      scale_colour_manual(values = plot_colors,
                          labels = plot_labels,
                          guide = guide_legend(title = NULL)
      ) +
      labs(x = "Year", y = "Standardized Value", colour = "") +
      theme_app()

    if (input$show_ci_tab2) {
      if (nrow(plot_data) > 2) {
      p <- p + geom_smooth(aes(group = Metric), method = "lm",
                           se = TRUE, linetype = "dashed", linewidth = 0.5, na.rm = TRUE, show.legend = FALSE)
      }
      else {
        cat("Warning: Not enough data points to compute smooth line for selected year range.\n")
      }
    }
    p
  })

  # ----- Scatter Plot -----
  output$scatterPlot <- renderPlot({
    req(input$scatter_x, input$scatter_y)

    show_ci <- input$show_ci_tab3

    scatter_x_col <- names(metric_names_map)[metric_names_map == input$scatter_x]
    scatter_y_col <- names(metric_names_map)[metric_names_map == input$scatter_y]

    if (length(scatter_x_col) == 0 || length(scatter_y_col) == 0) return(NULL)

    filtered <- ausbushfire_data %>%
      filter(year >= input$year_filter_tab2[1],
             year <= input$year_filter_tab2[2]) %>%
      mutate(highlight = ifelse(year == input$year_filter_tab2[2],
                                "Highlighted Year", "Other Years"))%>%
      filter(!is.na(.data[[scatter_x_col]]), !is.na(.data[[scatter_y_col]]))

    if (nrow(filtered) == 0) return(NULL)

    highlight_color_scatter <-  metric_colors["fwi_sm"]

    p <- ggplot(filtered,
                aes(x = .data[[scatter_x_col]],
                    y = .data[[scatter_y_col]])) +
      geom_point(data = filter(filtered, highlight == "Other Years"),
                 alpha = 0.5, size = base_size, colour = base_color) +
      geom_point(data = filter(filtered, highlight == "Highlighted Year"),
                 alpha = 0.9, size = highlight_size, colour = highlight_color_scatter) +
      geom_smooth(method = "lm", se = show_ci,
                  colour = highlight_color, linetype = "dashed") +
      labs(title = paste(input$scatter_y, "vs", input$scatter_x),
           x = input$scatter_x, y = input$scatter_y) +
      theme_app() +
      theme(
        plot.title = element_text(size = 22),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14)
      )
    p
  })

    # ---------- Tab 4 ----------
    output$study_region_text <- renderUI({
      tags$span(" The analysis focuses on a key fire-prone region of Southeastern Australia, defined by the polygon ",
                tags$b("29°S, 155°E; 29°S, 150°E; 40°S, 144°E; 40°S, 155°E"),
                "."
       )
    })

    output$downloadData_about <- downloadHandler(
      filename = function() { paste0("ausbushfire_data_", Sys.Date(), ".csv") },
      content  = function(file) { write.csv(ausbushfire_data, file, row.names = FALSE) }
    )
    output$sessionInfo <- renderPrint({ sessionInfo()
    })

    output$field_description <- renderTable({
      data.frame(
        Field = c("year", "region", "fwi_sm", "msr_sm", "tmax", "precip_total",
                  "iod_mean", "sam_mean", "area_burned"),
        Description = c(
          "Calendar year of observation",
          "Study region (Southeastern Australia)",
          "Fire Weather Index (FWI) – indicator of overall fire danger based on weather conditions",
          "Monthly Severity Rating (MSR) – reflects the potential for long-duration fires",
          "Mean maximum temperature during the fire season (°C)",
          "Total precipitation (mm) during the fire season (Sep–Feb)",
          "Indian Ocean Dipole (IOD) mean index – influences drought and rainfall patterns",
          "Southern Annular Mode (SAM) mean index – indicates wind and pressure variations affecting climate",
          "Observed total burned area (km²)"
        ),
        stringsAsFactors = FALSE
      )
    })
  }

# -------------------------------------------------
shinyApp(ui = ui, server = server)
