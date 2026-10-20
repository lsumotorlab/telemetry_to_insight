# Racing Telemetry Analysis (Interactive file load)
# ===================================================================
options(shiny.maxRequestSize = 500 * 1024^2)

suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(gt)
})

# =============================================================================
# Helper: normalize composite reference lap input into a character vector
# =============================================================================
as_syn_laps <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 0) return(character(0))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- x[x != ""]
  unique(x)
}

# =============================================================================
# Helper: per-sample time weights
# w_i = (t[i+1] - t[i-1]) / 2 for interior samples; endpoints get their single
# adjacent interval. Equal to 1/n weighting under uniform sampling, but stays
# correct for irregular sampling and for laps resampled in track position
# (e.g. the composite reference lap). Falls back to equal weights if no
# usable timing is available.
# =============================================================================
time_weights <- function(t) {
  n <- length(t)
  if (n < 2) return(rep(1, n))
  dt <- diff(as.numeric(t))
  dt[!is.finite(dt) | dt < 0] <- NA
  if (all(is.na(dt))) return(rep(1, n))
  w <- rowMeans(cbind(c(NA, dt), c(dt, NA)), na.rm = TRUE)
  w[!is.finite(w)] <- 0
  w
}

# Time share (%) of samples where cond is TRUE; NA cond counts as FALSE
time_pct <- function(cond, w) {
  if (sum(w) <= 0) return(NA_real_)
  sum(w[cond %in% TRUE]) / sum(w) * 100
}

# Time-weighted mean, ignoring NA values
time_mean <- function(x, w) {
  ok <- is.finite(x)
  if (!any(ok) || sum(w[ok]) <= 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# =============================================================================
# DATA LOADING FUNCTION
# =============================================================================
load_telemetry <- function(file_path, flip_horizontal = TRUE, flip_vertical = FALSE) {
  telemetry <- readr::read_csv(file_path, show_col_types = FALSE)
  
  telemetry <- telemetry |>
    dplyr::mutate(
      carCoordinatesX = if (isTRUE(flip_horizontal)) carCoordinatesX * -1 else carCoordinatesX,
      carCoordinatesZ = if (isTRUE(flip_vertical)) carCoordinatesZ * -1 else carCoordinatesZ
    )
  
  telemetry
}

# =============================================================================
# LAP STATISTICS
# =============================================================================
calculate_lap_stats <- function(
    telemetry,
    lap_num,
    full_throttle_threshold = 0.99,
    braking_threshold = 0.01
) {
  lap_data <- telemetry |>
    dplyr::filter(lapCount == lap_num)
  
  if (nrow(lap_data) == 0) return(NULL)

  # Time-weighted metrics: rows sorted by lapTime so weights follow sample order
  lap_data <- lap_data |>
    dplyr::arrange(lapTime) |>
    dplyr::mutate(.w = time_weights(lapTime))

  lap_data |>
    dplyr::summarise(
      Lap = lap_num,
      `Lap Time (s)` = max(lapTime, na.rm = TRUE) / 1000,
      `Full Throttle (%)` = time_pct(gas > full_throttle_threshold, .w),
      `Braking (%)` = time_pct(brake > braking_threshold, .w),
      `Brake/Throttle Overlap (%)` = time_pct(
        gas > full_throttle_threshold & brake > braking_threshold, .w
      ),
      `Avg Speed (km/h)` = time_mean(speedKmh, .w),
      `Max Speed (km/h)` = max(speedKmh, na.rm = TRUE),
      `Min Speed (km/h)` = min(speedKmh, na.rm = TRUE),
      `ABS Active (%)` = time_pct(isAbsInAction == 1, .w),
      `TC Active (%)` = time_pct(isTcInAction == 1, .w),
      `Max Lateral G` = if ("accGHorizontal" %in% names(lap_data)) max(abs(accGHorizontal), na.rm = TRUE) else NA_real_,
      `Max Accel G` = if ("accGFrontal" %in% names(lap_data)) max(accGFrontal, na.rm = TRUE) else NA_real_,
      `Max Brake G` = if ("accGFrontal" %in% names(lap_data)) min(accGFrontal, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}

calculate_multiple_laps <- function(
    telemetry,
    lap_numbers,
    full_throttle_threshold = 0.99,
    braking_threshold = 0.01
) {
  lap_numbers <- as.integer(lap_numbers)
  purrr::map_dfr(
    lap_numbers,
    \(x) calculate_lap_stats(
      telemetry,
      x,
      full_throttle_threshold = full_throttle_threshold,
      braking_threshold = braking_threshold
    )
  )
}

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================
get_summary_stats <- function(lap_stats_table, synthetic_lap = NA) {
  syn <- as_syn_laps(synthetic_lap)
  
  df <- lap_stats_table
  if ("Lap" %in% names(df) && length(syn) > 0) {
    df <- df |> dplyr::filter(!(as.character(.data$Lap) %in% syn))
  }
  
  df |>
    dplyr::select(-dplyr::any_of("Lap")) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(
          Mean = \(x) mean(x, na.rm = TRUE),
          SD = \(x) sd(x, na.rm = TRUE),
          Min = \(x) min(x, na.rm = TRUE),
          Max = \(x) max(x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Metric", values_to = "Value") |>
    tidyr::separate(Metric, into = c("Metric", "Statistic"), sep = "_(?=[^_]+$)") |>
    tidyr::pivot_wider(names_from = Metric, values_from = Value)
}

# =============================================================================
# LABELING FOR TABLES ONLY
# =============================================================================
add_synthetic_labels <- function(lap_stats, synthetic_lap = NA) {
  syn <- as_syn_laps(synthetic_lap)
  
  lap_stats |>
    dplyr::mutate(
      LapType = dplyr::if_else(length(syn) > 0 & as.character(.data$Lap) %in% syn, "Composite reference", "Lap"),
      LapLabel = dplyr::if_else(LapType == "Composite reference", paste0(.data$Lap, " (Composite reference)"), as.character(.data$Lap))
    ) |>
    dplyr::relocate(LapLabel, LapType, .after = Lap)
}

# =============================================================================
# SHINY APP (file loaded from UI, not function argument)
# =============================================================================
run_telemetry_app <- function(
    default_flip_horizontal = TRUE,
    default_flip_vertical = FALSE,
    default_full_throttle_threshold = 0.99,
    default_braking_threshold = 0.01
) {
  
  ui <- fluidPage(
    tags$head(tags$style(HTML("
      body { font-size: 18px; }
      .btn { font-size: 18px; }
      .form-control { font-size: 18px; }
      .selectize-input { font-size: 18px; }
    "))),
    title = "Racing Telemetry Analyzer",
    
    h1("Racing Telemetry Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h3("Data"),
        fileInput("file", "Upload telemetry CSV", accept = ".csv"),
        checkboxInput("flip_horizontal", "Flip horizontal (mirror X)", value = default_flip_horizontal),
        checkboxInput("flip_vertical", "Flip vertical (mirror Z)", value = default_flip_vertical),
        
        hr(),
        h3("Analysis Settings"),
        
        h4("Base Metrics"),
        sliderInput(
          "full_throttle_threshold",
          "Full Throttle Threshold (%)",
          min = 0,
          max = 100,
          value = default_full_throttle_threshold * 100,
          step = 1
        ),
        sliderInput(
          "braking_threshold",
          "Braking Threshold (%)",
          min = 0,
          max = 100,
          value = default_braking_threshold * 100,
          step = 1
        ),
        hr(),
        h4("Select Laps"),
        uiOutput("laps_ui"),
        
        uiOutput("synthetic_ui"),
        
        actionButton("select_all", "Select All", class = "btn-primary"),
        actionButton("deselect_all", "Deselect All", class = "btn-secondary"),
        
        hr(),
        h4("Lap Trend"),
        uiOutput("metric_ui")
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          tabPanel("Lap Statistics", br(), downloadButton("download_lap", "Export CSV"), br(), br(), gt::gt_output("lap_table")),
          tabPanel("Summary Statistics", br(), downloadButton("download_summary", "Export CSV"), br(), br(), gt::gt_output("summary_table")),
          tabPanel("Lap Trend", br(), plotOutput("trend_plot", height = "550px"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    telemetry <- reactive({
      req(input$file)
      df <- load_telemetry(
        input$file$datapath,
        flip_horizontal = input$flip_horizontal,
        flip_vertical = input$flip_vertical
      )
      
      validate(need("lapCount" %in% names(df), "Missing column lapCount in telemetry file"))
      df
    })
    
    available_laps <- reactive({
      req(telemetry())
      laps <- sort(unique(telemetry()$lapCount))
      laps <- laps[!is.na(laps)]
      laps <- laps[laps > 0]
      as.character(laps)
    })
    
    output$laps_ui <- renderUI({
      req(available_laps())
      laps <- available_laps()
      sel <- head(laps, 5)
      checkboxGroupInput(
        "selected_laps",
        "Choose laps to analyze:",
        choices = laps,
        selected = sel,
        inline = FALSE
      )
    })
    
    output$synthetic_ui <- renderUI({
      req(available_laps())
      laps <- available_laps()
      # Pre-select the composite reference lap if the file marks it
      tel <- telemetry()
      marked <- if (".synthetic" %in% names(tel)) {
        as.character(unique(tel$lapCount[tel$.synthetic %in% TRUE]))
      } else {
        character(0)
      }
      selectInput(
        "synthetic_lap",
        "Designate the composite reference lap (optional):",
        choices = c("None" = "", laps),
        selected = if (length(marked) > 0 && marked[1] %in% laps) marked[1] else ""
      )
    })
    
    observeEvent(input$select_all, {
      req(available_laps())
      updateCheckboxGroupInput(session, "selected_laps", selected = available_laps())
    })
    
    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(session, "selected_laps", selected = character(0))
    })
    
    synthetic_laps <- reactive({
      as_syn_laps(input$synthetic_lap)
    })
    
    lap_stats_reactive <- reactive({
      req(telemetry())
      req(input$selected_laps)
      validate(need(length(input$selected_laps) > 0, "Select at least one lap"))
      
      full_throttle_thr <- input$full_throttle_threshold / 100
      braking_thr <- input$braking_threshold / 100
      lap_numbers <- as.integer(input$selected_laps)
      
      calculate_multiple_laps(
        telemetry(),
        lap_numbers,
        full_throttle_threshold = full_throttle_thr,
        braking_threshold = braking_thr
      )
    })
    
    output$metric_ui <- renderUI({
      ls <- lap_stats_reactive()
      if (is.null(ls) || nrow(ls) == 0) return(NULL)
      metrics <- setdiff(names(ls), "Lap")
      selectInput("trend_metric", "Variable to plot:", choices = metrics, selected = metrics[1])
    })
    
    output$lap_table <- gt::render_gt({
      lap_stats <- lap_stats_reactive()
      syn <- synthetic_laps()
      
      lap_stats_disp <- lap_stats |>
        add_synthetic_labels(synthetic_lap = syn) |>
        dplyr::select(-LapLabel) |>
        dplyr::relocate(Lap, LapType)
      
      lap_stats_disp |>
        gt::gt() |>
        gt::fmt_integer(columns = "Lap") |>
        gt::fmt_number(columns = dplyr::where(is.numeric) & !dplyr::any_of("Lap"), decimals = 2) |>
        gt::tab_header(
          title = "Lap Statistics",
          subtitle = paste0(
            "Base thresholds  Full Throttle: ", input$full_throttle_threshold, "%, ",
            "Brake: ", input$braking_threshold, "%"
          )
        ) |>
        gt::opt_interactive(use_pagination = TRUE, pagination_type = "jump")
    })
    
    output$summary_table <- gt::render_gt({
      lap_stats <- lap_stats_reactive()
      syn <- synthetic_laps()
      
      summary_stats <- get_summary_stats(lap_stats, synthetic_lap = syn)
      
      included <- if (length(syn) > 0) {
        sum(!(as.character(lap_stats$Lap) %in% syn))
      } else {
        nrow(lap_stats)
      }
      
      subtitle_txt <- if (length(syn) > 0) {
        paste0(included, " laps included. Composite reference lap excluded: ", paste(syn, collapse = ", "))
      } else {
        paste0(included, " laps included")
      }
      
      summary_stats |>
        gt::gt() |>
        gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 2) |>
        gt::tab_header(
          title = "Summary Statistics",
          subtitle = subtitle_txt
        )
    })
    
    output$trend_plot <- renderPlot({
      req(input$trend_metric)
      lap_stats <- lap_stats_reactive()
      syn <- synthetic_laps()
      
      df <- lap_stats
      if (length(syn) > 0) {
        df <- df |> dplyr::filter(!(as.character(.data$Lap) %in% syn))
      }
      
      df <- df |>
        dplyr::select(Lap, dplyr::all_of(input$trend_metric)) |>
        dplyr::rename(Value = dplyr::all_of(input$trend_metric)) |>
        dplyr::filter(is.finite(Value)) |>
        dplyr::arrange(Lap)
      
      ggplot(df, aes(x = Lap, y = Value)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = df$Lap)+
        labs(
          title = paste0("Lap trend: ", input$trend_metric),
          subtitle = if (length(syn) > 0) paste0("Composite reference lap excluded: ", paste(syn, collapse = ", ")) else NULL,
          x = "Lap",
          y = input$trend_metric
        ) +
        theme_minimal(base_size = 18)
    })
    output$download_lap <- downloadHandler(
      filename = function() paste0("lap_statistics_", Sys.Date(), ".csv"),
      content  = function(file) {
        lap_stats <- lap_stats_reactive()
        syn <- synthetic_laps()
        lap_stats |>
          add_synthetic_labels(synthetic_lap = syn) |>
          dplyr::select(-LapLabel) |>
          dplyr::relocate(Lap, LapType) |>
          readr::write_csv(file)
      }
    )

    output$download_summary <- downloadHandler(
      filename = function() paste0("summary_statistics_", Sys.Date(), ".csv"),
      content  = function(file) {
        lap_stats <- lap_stats_reactive()
        syn <- synthetic_laps()
        get_summary_stats(lap_stats, synthetic_lap = syn) |>
          readr::write_csv(file)
      }
    )
  }

  shinyApp(ui, server)
}

## Run

run_telemetry_app()
