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

#  5. 定義圖例級距
area_bins <- c(0, 1000, 5000, 10000, max_area + 1)
area_pal <- colorBin("Reds", domain = c(0, max_area), bins = area_bins)

# 6. 定義指標名稱和顏色映射
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
  fwi_sm = "#B71C1C", # Primary Red
  msr_sm = "#FF9800", # Danger Orange
  tmax = "#FFEB3B",   # Yellow Heat
  precip_total = "#2196F3", # Blue Water
  iod_mean = "#4CAF50",     # Green
  sam_mean = "#673AB7",     # Deep Purple
  area_burned = "#E53935"   # Brighter Red
)

# -------------------------------------------------
#  UI
# -------------------------------------------------
ui <- navbarPage(
  title = "Australian Bushfire Risk Explorer",
  theme = bslib::bs_theme(
    bootswatch = "litera", # 選擇一個乾淨、現代的淺色基礎風格
    primary = "#B71C1C",  # 火災紅作為主要警告色
    secondary = "#495057", # 次要色用於中性背景
    danger = "#FF9800",   # 警示色改為橘色
    base_font = font_google("Inter"),
    fg = "#333333",       # 前景文字色
    bg = "#F8F9FA",       # 背景色設為極淺灰
    navbar_bg = "#343a40",    # 深炭灰背景
    navbar_fg = "#FFFFFF",
    # 實現分頁設計感
    navbar_active_link_bg = "transparent", # 點擊時背景透明
    navbar_active_link_color = "#FFFFFF",  # 點擊時文字保持白色
    # 使用 custom CSS 變數創建底線效果
    `navbar-link-padding-y` = "1rem",
    `navbar-padding-y` = "0rem",
    `navbar-link-active-border-bottom` = paste0("3px solid var(--bs-primary)"),
    `navbar-link-hover-color` = "rgba(255, 255, 255, 0.75)"
  ),

  # ---------- Tab 1 ----------
  tabPanel(
    "Risk Map",
    fillPage(
      padding = c(10,10,10,10),
      absolutePanel(
        top = "30vh", left = 10, width = 300,
        style = "
          background: #FFFFFF;
          padding: 15px;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15); /* 柔和陰影 */
          z-index: 1000;
          color: #333333; /* 確保淺色背景下文字是深色 */
        ",
        h4("What's Black Summer"),
        p("In 2019-2020, the Black Summer bushfires burned over 18 million hectares in southeastern Australia,
          with climate change increasing the risk by at least 30%."),
        p("This app explores fire weather trends and climate drivers from 1979-2019."),
      ),
      h4("Research Area: Southeastern Australia Fire Risk"),
      p("Map shows the study region. Red intensity represents burned area (from 1997)."),
      leafletOutput("studyMapInteractive", height = "70vh"),
      absolutePanel(
        top = 100, right = 10, width = 350,
        style = "
          background: #FFFFFF;
          padding: 10px 15px;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15); /* 柔和陰影 */
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
    "Explore Trends",
    fluid = TRUE,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "background: #F0F0F0; border-right: 1px solid #DDDDDD; height: 90vh; overflow-y: auto; padding-top: 15px;",

        h4("Interactive Controls"),
        sliderInput("year_filter_tab2","Select Year Range:",
                    min = 1979, max = 2019,
                    value = c(1979,2019), step = 1, sep = ""),
        hr(),
        checkboxGroupInput(
          "selected_metrics",
          "Select Metrics",
          choices = metric_names_map,
          selected = metric_names_map[c("fwi_sm", "msr_sm", "precip_total")]
        ),
        hr(),
        checkboxInput("show_ci_tab2","Show Trend Line (with 95% Confidence Interval)", value = FALSE),
        hr(),
        h5(textOutput("highlight_year_text")),
        uiOutput("dynamic_value_boxes")
      ),
      mainPanel(
        width = 9,
        # 確保 mainPanel 內容可以滾動
        style = "overflow-y: auto; height: 90vh;",
        h4("Time Series Comparison"),
        p("This chart shows the selected metrics over time"),
        plotOutput("combinedTimeSeriesPlot", height = "400px"),
        hr(),
        h4("Relationship Between Metrics"),
        fluidRow(
          column(6, selectInput("scatter_x","X Axis:", choices = metric_names_map, selected = metric_names_map["fwi_sm"])),
          column(6, selectInput("scatter_y","Y Axis:", choices = metric_names_map, selected = metric_names_map["area_burned"]))
        ),
        plotOutput("scatterPlot", height = "500px")
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
      "FWI: {round(d$fwi_sm, 1)}<br>",
      "Burned Area: {ifelse(d$year < 1997 || is.na(area_val), 'N/A', comma(round(area_val, 0)))} km²"
    )

    #  添加動態多邊形
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
        color = "#B71C1C", # 邊框顏色
        weight = 3,
        fillOpacity = opacity,
        layerId = "dynamic_region", # 使用特定的 Layer ID 以便更新
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
      title = paste(d$year,"FWI"),
      value = round(d$fwi_sm,1),
      p(paste("Long-term Average:",round(averages$avg_fwi_sm,1)), style = "font-size: 20px;"),
      theme = theme_status,
      height = "150px"
    )
  })

  output$map_stats_area_v11 <- renderUI({
    d <- map_year_data()
    if (d$year < 1997 || is.na(d$area_burned))
      return(bslib::value_box(title = paste(d$year,"Burned Area"), value = "N/A",
                              p("Data starts from 1997", style = "font-size: 20px;"),
                              theme = "secondary",
                              height = "150px"))
    bslib::value_box(
      title = paste(d$year,"Burned Area (km²)"),
      value = scales::comma(round(d$area_burned,0)),
      p(paste("Long-term Average:",scales::comma(round(averages$avg_area_burned,0))), style = "font-size: 20px;"),
      theme = "secondary",
      height = "150px"
    )
  })

  # ---------- Tab 2 ----------
  output$highlight_year_text <- renderText({
    req(input$year_filter_tab2)
    current_year <- input$year_filter_tab2[2]
    paste("Key Data (", current_year, "Year):")
  })

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
        height = "130px"
      )
    })
    tagList(boxes)
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
        # 如果數據點不足，印出警告訊息到 R Console
        cat("Warning: Not enough data points to compute smooth line for selected year range.\n")
      }
    }
    p
  })

  # ----- Scatter Plot -----
  output$scatterPlot <- renderPlot({
    req(input$scatter_x, input$scatter_y)

    scatter_x_col <- names(metric_names_map)[metric_names_map == input$scatter_x]
    scatter_y_col <- names(metric_names_map)[metric_names_map == input$scatter_y]

    if (length(scatter_x_col) == 0 || length(scatter_y_col) == 0) return(NULL)

    filtered <- ausbushfire_data %>%
      filter(year >= input$year_filter_tab2[1],
             year <= input$year_filter_tab2[2]) %>%
      # 高亮年份設定為 Tab 2 的上限年份 (Q3)
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
