options(shiny.maxRequestSize = 500 * 1024^2)

track_mapping_interactive <- function(
    default_overlap_throttle_threshold = 0.99,
    default_overlap_brake_threshold = 0.01
) {
  
  suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
    library(dplyr)
    library(readr)
    library(ggplot2)
    library(tidyr)
    library(purrr)
    library(viridis)
    library(signal)
  })
  filter <- dplyr::filter
  
  lowpass_filter <- function(x, cutoff_hz, fs) {
    if (is.na(fs) || fs <= 0 || cutoff_hz <= 0 || cutoff_hz >= fs / 2) return(x)
    W <- cutoff_hz / (fs / 2)
    bf <- signal::butter(2, W, type = "low")
    as.numeric(signal::filtfilt(bf, x))
  }

  downsample_for_plot <- function(df, max_points = 12000) {
    if (nrow(df) <= max_points) return(df)
    step <- ceiling(nrow(df) / max_points)
    df[seq(1, nrow(df), by = step), , drop = FALSE]
  }
  
  safe_ggplotly <- function(p, show_hover = FALSE) {
    tooltip_arg <- if (isTRUE(show_hover)) "all" else "none"
    plotly::toWebGL(plotly::layout(ggplotly(p, tooltip = tooltip_arg), hovermode = "closest"))
  }
  
  plot_track_outline <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    lap_data <- downsample_for_plot(lap_data)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(linewidth = 0.9) +
      coord_fixed() +
      labs(
        title = paste("Track Layout | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_speed_map <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("speedKmh" %in% names(lap_data))) stop("Missing column: speedKmh")
    lap_data <- downsample_for_plot(lap_data)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.7) +
      geom_point(aes(color = speedKmh), size = 1.1, alpha = 0.85) +
      scale_color_gradientn(colours = c("red3", "yellow", "limegreen")) +
      coord_fixed() +
      labs(
        title = paste("Speed Map | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Speed\n(km/h)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_gear_map <- function(lap_data, lap_id, ac_gear_remap = FALSE, base_family = "Times New Roman", base_size = 18) {
    if (!("gear" %in% names(lap_data))) stop("Missing column: gear")
    if (ac_gear_remap) {
      lap_data <- lap_data |> mutate(gear_label = case_when(
        gear == 0 ~ "R",
        gear == 1 ~ "N",
        TRUE      ~ as.character(gear - 1)
      ))
      gear_levels <- c("R", "N", as.character(1:6))
      lap_data <- lap_data |> mutate(gear_label = factor(gear_label, levels = gear_levels[gear_levels %in% gear_label]))
    } else {
      lap_data <- lap_data |> mutate(gear_label = factor(gear))
    }
    lap_data <- downsample_for_plot(lap_data)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray85", linewidth = 0.7) +
      geom_point(aes(color = gear_label), size = 1.2, alpha = 0.85) +
      scale_color_brewer(palette = "Set1", type = "qual") +
      coord_fixed() +
      labs(
        title = paste("Gear Map | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Gear"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_brake_zones <- function(lap_data, lap_id, brake_threshold = 0.1, base_family = "Times New Roman", base_size = 18) {
    if (!("brake" %in% names(lap_data))) stop("Missing column: brake")
    lap_data <- downsample_for_plot(lap_data)
    brake_zones <- lap_data %>% filter(brake > brake_threshold)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.9) +
      geom_point(data = brake_zones, aes(color = brake), size = 1.2, alpha = 0.75) +
      scale_color_gradient(low = "orange", high = "red3") +
      coord_fixed() +
      labs(
        title = paste("Braking Zones | Lap", lap_id),
        subtitle = paste0("Threshold: brake > ", brake_threshold),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Brake\nPressure"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_throttle_zones <- function(lap_data, lap_id, throttle_threshold = 0.99, base_family = "Times New Roman", base_size = 18) {
    need <- c("gas", "speedKmh")
    if (!all(need %in% names(lap_data))) stop("Missing columns: gas and speedKmh")
    lap_data <- downsample_for_plot(lap_data)
    throttle_zones <- lap_data %>% filter(gas > throttle_threshold)
    throttle_pct <- if (nrow(lap_data) > 0) (nrow(throttle_zones) / nrow(lap_data)) * 100 else NA_real_
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.9) +
      geom_point(data = throttle_zones, aes(color = speedKmh), size = 1.2, alpha = 0.75) +
      scale_color_gradientn(colours = c("red3", "yellow", "limegreen")) +
      coord_fixed() +
      labs(
        title = paste("Full Throttle Zones | Lap", lap_id),
        subtitle = paste0("Samples above ", throttle_threshold * 100, "% throttle: ", sprintf("%.1f", throttle_pct), "%"),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Speed\n(km/h)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  
  plot_overlap_zones <- function(lap_data, lap_id, throttle_threshold = 0.99, brake_threshold = 0.01, base_family = "Times New Roman", base_size = 18) {
    need <- c("gas", "brake")
    if (!all(need %in% names(lap_data))) stop("Missing columns: gas and brake")
    lap_data <- downsample_for_plot(lap_data)
    lap_data <- lap_data %>%
      mutate(overlap_flag = gas > throttle_threshold & brake > brake_threshold)
    overlap_pts <- lap_data %>% filter(overlap_flag)
    overlap_pct <- if (nrow(lap_data) > 0) (nrow(overlap_pts) / nrow(lap_data)) * 100 else NA_real_
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.9) +
      geom_point(aes(color = overlap_flag), size = 1.3, alpha = 0.75) +
      scale_color_manual(values = c("FALSE" = "gray75", "TRUE" = "red3"), breaks = c("FALSE", "TRUE"), labels = c("No overlap", "Overlap")) +
      coord_fixed() +
      labs(
        title = paste("Brake/Throt Overlap | Lap", lap_id),
        subtitle = paste0("Brake > ", brake_threshold, " and throttle > ", throttle_threshold, ": ", sprintf("%.1f", overlap_pct), "% of samples"),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = NULL
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_abs_points <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("isAbsInAction" %in% names(lap_data))) stop("Missing column: isAbsInAction")
    lap_data <- downsample_for_plot(lap_data)
    abs_points <- lap_data %>% filter(isAbsInAction == 1)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.9) +
      geom_point(data = abs_points, color = "red3", size = 1.6, alpha = 0.65) +
      coord_fixed() +
      labs(
        title = paste("ABS Activation Points | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_tc_points <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("isTcInAction" %in% names(lap_data))) stop("Missing column: isTcInAction")
    lap_data <- downsample_for_plot(lap_data)
    tc_points <- lap_data %>% filter(isTcInAction == 1)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.9) +
      geom_point(data = tc_points, color = "orange", size = 1.6, alpha = 0.65) +
      coord_fixed() +
      labs(
        title = paste("TC Activation Points | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_elevation_profile <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("carCoordinatesY" %in% names(lap_data))) stop("Missing column: carCoordinatesY")
    lap_data <- downsample_for_plot(lap_data)
    df <- lap_data %>%
      mutate(distance = cumsum(sqrt(
        (carCoordinatesX - lag(carCoordinatesX, default = first(carCoordinatesX)))^2 +
          (carCoordinatesZ - lag(carCoordinatesZ, default = first(carCoordinatesZ)))^2
      )))
    ggplot(df, aes(x = distance, y = carCoordinatesY)) +
      geom_line(color = "brown", linewidth = 0.9) +
      labs(
        title = paste("Track Elevation Profile | Lap", lap_id),
        x = "Distance (m)",
        y = "Elevation (m)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_track_with_elevation <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("carCoordinatesY" %in% names(lap_data))) stop("Missing column: carCoordinatesY")
    elev_range <- range(lap_data$carCoordinatesY, na.rm = TRUE)
    if (diff(elev_range) < 1e-6) elev_range <- elev_range + c(-0.5, 0.5)
    lap_data <- downsample_for_plot(lap_data)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray85", linewidth = 0.7) +
      geom_point(aes(color = carCoordinatesY), size = 1.1, alpha = 0.85) +
      scale_color_viridis_c(option = "cividis", limits = elev_range) +
      coord_fixed() +
      labs(
        title = paste("Track with Elevation | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Elevation\n(m)"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }
  
  plot_lateral_g_map <- function(lap_data, lap_id, cutoff_hz = 0, base_family = "Times New Roman", base_size = 18) {
    if (!("accGHorizontal" %in% names(lap_data))) stop("Missing column: accGHorizontal")
    clim <- max(abs(lap_data$accGHorizontal), na.rm = TRUE)
    if ("lapTime" %in% names(lap_data)) {
      dt_ms <- median(diff(lap_data$lapTime), na.rm = TRUE)
      fs <- if (!is.na(dt_ms) && dt_ms > 0) 1000 / dt_ms else NA_real_
      lap_data <- lap_data |> mutate(
        accGHorizontal = lowpass_filter(accGHorizontal, cutoff_hz, fs)
      )
    }
    lap_data <- downsample_for_plot(lap_data)
    subtitle_parts <- if (cutoff_hz > 0) paste0("Low-pass: ", cutoff_hz, " Hz") else "No filter"
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.7) +
      geom_point(aes(color = accGHorizontal), size = 1.1, alpha = 0.85) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-clim, clim)) +
      coord_fixed() +
      labs(
        title = paste("Lateral G Map | Lap", lap_id),
        subtitle = subtitle_parts,
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Lateral G"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }

  plot_lateral_g_time <- function(lap_data, lap_id, cutoff_hz = 0, base_family = "Times New Roman", base_size = 18) {
    need <- c("accGHorizontal", "lapTime")
    if (!all(need %in% names(lap_data))) stop(paste("Missing columns:", paste(setdiff(need, names(lap_data)), collapse = ", ")))
    dt_ms <- median(diff(lap_data$lapTime), na.rm = TRUE)
    fs <- if (!is.na(dt_ms) && dt_ms > 0) 1000 / dt_ms else NA_real_
    if (cutoff_hz > 0) {
      lap_data <- lap_data |> mutate(accGHorizontal = lowpass_filter(accGHorizontal, cutoff_hz, fs))
    }
    lap_data <- lap_data |> mutate(time_s = lapTime / 1000)
    lap_data <- downsample_for_plot(lap_data)
    clim <- max(abs(lap_data$accGHorizontal), na.rm = TRUE)
    subtitle_parts <- if (cutoff_hz > 0) paste0("Low-pass: ", cutoff_hz, " Hz") else "No filter"
    ggplot(lap_data, aes(x = time_s, y = accGHorizontal)) +
      geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
      geom_line(color = "gray75", linewidth = 0.5) +
      geom_point(aes(color = accGHorizontal), size = 0.8, alpha = 0.8) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-clim, clim)) +
      labs(
        title = paste("Lateral G over Time | Lap", lap_id),
        subtitle = subtitle_parts,
        x = "Lap Time (s)",
        y = "Lateral G",
        color = "Lateral G"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }

  plot_steer_time <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    need <- c("steer", "lapTime")
    if (!all(need %in% names(lap_data))) stop(paste("Missing columns:", paste(setdiff(need, names(lap_data)), collapse = ", ")))
    lap_data <- lap_data |> mutate(time_s = lapTime / 1000)
    lap_data <- downsample_for_plot(lap_data)
    clim <- max(abs(lap_data$steer), na.rm = TRUE)
    ggplot(lap_data, aes(x = time_s, y = steer)) +
      geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
      geom_line(color = "gray75", linewidth = 0.5) +
      geom_point(aes(color = steer), size = 0.8, alpha = 0.8) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-clim, clim)) +
      labs(
        title = paste("Steer over Time | Lap", lap_id),
        x = "Lap Time (s)",
        y = "Steer",
        color = "Steer"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }

  plot_longitudinal_g_map <- function(lap_data, lap_id, base_family = "Times New Roman", base_size = 18) {
    if (!("accGFrontal" %in% names(lap_data))) stop("Missing column: accGFrontal")
    lap_data <- downsample_for_plot(lap_data)
    ggplot(lap_data, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray80", linewidth = 0.7) +
      geom_point(aes(color = accGFrontal), size = 1.1, alpha = 0.85) +
      scale_color_gradient2(low = "red", mid = "white", high = "green3", midpoint = 0) +
      coord_fixed() +
      labs(
        title = paste("Longitudinal G Map | Lap", lap_id),
        x = "X Position (m)",
        y = "Z Position (m)",
        color = "Long. G"
      ) +
      theme_classic(base_family = base_family, base_size = base_size)
  }

  plot_catalogue <- list(
    "Track outline" = list(id = "outline", req = c(), fn = function(ld, lap_id, pars) plot_track_outline(ld, lap_id)),
    "Speed map" = list(id = "speed", req = c("speedKmh"), fn = function(ld, lap_id, pars) plot_speed_map(ld, lap_id)),
    "Gear map" = list(id = "gear", req = c("gear"), fn = function(ld, lap_id, pars) plot_gear_map(ld, lap_id, ac_gear_remap = pars$ac_gear_remap)),
    "Braking zones" = list(id = "brake", req = c("brake"), fn = function(ld, lap_id, pars) plot_brake_zones(ld, lap_id, pars$brake_threshold)),
    "Full throttle zones" = list(id = "throttle", req = c("gas", "speedKmh"), fn = function(ld, lap_id, pars) plot_throttle_zones(ld, lap_id, pars$throttle_threshold)),
    "Brake/Throttle Overlap" = list(
      id = "overlap",
      req = c("gas", "brake"),
      fn = function(ld, lap_id, pars) plot_overlap_zones(ld, lap_id, pars$overlap_throttle_threshold, pars$overlap_brake_threshold)
    ),
    "ABS activation points" = list(id = "abs", req = c("isAbsInAction"), fn = function(ld, lap_id, pars) plot_abs_points(ld, lap_id)),
    "TC activation points" = list(id = "tc", req = c("isTcInAction"), fn = function(ld, lap_id, pars) plot_tc_points(ld, lap_id)),
    "Elevation profile" = list(id = "elev_profile", req = c("carCoordinatesY"), fn = function(ld, lap_id, pars) plot_elevation_profile(ld, lap_id)),
    "Track with elevation color" = list(id = "elev_track", req = c("carCoordinatesY"), fn = function(ld, lap_id, pars) plot_track_with_elevation(ld, lap_id)),
    "Lateral G map" = list(id = "lateral_g", req = c("accGHorizontal"), fn = function(ld, lap_id, pars) plot_lateral_g_map(ld, lap_id, cutoff_hz = pars$lateral_g_cutoff)),
    "Longitudinal G map" = list(id = "longitudinal_g", req = c("accGFrontal"), fn = function(ld, lap_id, pars) plot_longitudinal_g_map(ld, lap_id)),
    "Lateral G over time" = list(id = "lateral_g_time", req = c("accGHorizontal", "lapTime"), fn = function(ld, lap_id, pars) plot_lateral_g_time(ld, lap_id, cutoff_hz = pars$lateral_g_cutoff)),
    "Steer over time" = list(id = "steer_time", req = c("steer", "lapTime"), fn = function(ld, lap_id, pars) plot_steer_time(ld, lap_id))
  )
  
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body { font-size: 18px; }
        .btn { font-size: 18px; }
        .form-control { font-size: 18px; }
        .selectize-input { font-size: 18px; }
      "))
    ),
    titlePanel("Track Mapping | Interactive"),
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file", "Upload Telemetry CSV", accept = ".csv"),
        uiOutput("lap_selector"),
        tags$hr(),
        checkboxInput("flip_horizontal", "Flip horizontal (mirror X)", value = TRUE),
        checkboxInput("flip_vertical", "Flip vertical (mirror Z)", value = FALSE),
        tags$hr(),
        selectizeInput(
          "plots",
          "Select up to 4 plots",
          choices = names(plot_catalogue),
          selected = "Track outline",
          multiple = TRUE,
          options = list(maxItems = 4, placeholder = "Pick 1 to 4 plots")
        ),
        tags$hr(),
        conditionalPanel(
          condition = "input.plots && input.plots.indexOf('Gear map') >= 0",
          checkboxInput("ac_gear_remap", "Set gear encoding (0=R, 1=N, 2–7 = gears 1–6)", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.plots && input.plots.indexOf('Braking zones') >= 0",
          numericInput("brake_threshold", "Brake threshold", value = 0.01, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.plots && input.plots.indexOf('Full throttle zones') >= 0",
          numericInput("throttle_threshold", "Throttle threshold", value = 0.99, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.plots && (input.plots.indexOf('Lateral G map') >= 0 || input.plots.indexOf('Lateral G over time') >= 0)",
          sliderInput("lateral_g_cutoff", "Lateral G low-pass cutoff (Hz)  [0 = no filter]", min = 0, max = 20, value = 0, step = 0.5),
          helpText("A cutoff of 2–5 Hz captures the driver's intentional steering inputs. Values above 5 Hz likely reflect suspension dynamics or road surface irregularities rather than deliberate steering actions. Set to 0 to view the raw signal.")
        ),
        conditionalPanel(
          condition = "input.plots && input.plots.indexOf('Brake/Throttle Overlap') >= 0",
          tagList(
            sliderInput("overlap_throttle_threshold", "Overlap Full Throttle Threshold (%)", min = 0, max = 100, value = default_overlap_throttle_threshold * 100, step = 1),
            sliderInput("overlap_brake_threshold", "Overlap Brake Threshold (%)", min = 0, max = 100, value = default_overlap_brake_threshold * 100, step = 1)
          )
        ),
        tags$hr(),
        checkboxInput("show_hover", "Show hover data", value = FALSE),
        helpText("Plots that require missing columns will show an error panel instead of freezing the UI.")
      ),
      mainPanel(
        h3(textOutput("lap_time_display")),
        fluidRow(
          column(6, plotlyOutput("plot1", height = "420px")),
          column(6, plotlyOutput("plot2", height = "420px"))
        ),
        fluidRow(
          column(6, plotlyOutput("plot3", height = "420px")),
          column(6, plotlyOutput("plot4", height = "420px"))
        ),
        tags$hr(),
        verbatimTextOutput("status")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    telemetry_raw <- reactive({
      req(input$file)
      read_csv(input$file$datapath, show_col_types = FALSE)
    })
    
    telemetry_oriented <- reactive({
      df <- telemetry_raw()
      need <- c("lapCount", "carCoordinatesX", "carCoordinatesZ")
      miss <- setdiff(need, names(df))
      validate(need(length(miss) == 0, paste("Missing required columns:", paste(miss, collapse = ", "))))
      df %>%
        mutate(
          carCoordinatesX = if (isTRUE(input$flip_horizontal)) -carCoordinatesX else carCoordinatesX,
          carCoordinatesZ = if (isTRUE(input$flip_vertical)) -carCoordinatesZ else carCoordinatesZ
        )
    })
    
    output$lap_selector <- renderUI({
      req(telemetry_oriented())
      laps <- sort(unique(telemetry_oriented()$lapCount))
      current_lap <- isolate(input$lap)
      selected_lap <- if (!is.null(current_lap) && current_lap %in% laps) current_lap else laps[1]
      selectInput("lap", "Select lap", choices = laps, selected = selected_lap)
    })
    
    lap_data <- reactive({
      req(telemetry_oriented(), input$lap)
      telemetry_oriented() %>% filter(lapCount == input$lap)
    })
    
    output$lap_time_display <- renderText({
      req(lap_data(), input$lap)
      ld <- lap_data()
      if (!("lapTime" %in% names(ld))) return(paste("Lap", input$lap))
      lt_ms <- max(ld$lapTime, na.rm = TRUE)
      if (!is.finite(lt_ms)) return(paste("Lap", input$lap))
      lt_sec <- lt_ms / 1000
      formatted <- sprintf("%d:%06.3f", floor(lt_sec / 60), lt_sec %% 60)
      paste0("Lap ", input$lap, "  |  ", formatted)
    })

    make_plot <- function(plot_name) {
      req(plot_name)
      ld <- lap_data()
      lap_id <- input$lap
      
      pars <- list(
        brake_threshold = if (!is.null(input$brake_threshold)) input$brake_threshold else 0.1,
        throttle_threshold = if (!is.null(input$throttle_threshold)) input$throttle_threshold else 0.99,
        overlap_throttle_threshold = if (!is.null(input$overlap_throttle_threshold)) input$overlap_throttle_threshold / 100 else default_overlap_throttle_threshold,
        overlap_brake_threshold = if (!is.null(input$overlap_brake_threshold)) input$overlap_brake_threshold / 100 else default_overlap_brake_threshold,
        lateral_g_cutoff = if (!is.null(input$lateral_g_cutoff)) input$lateral_g_cutoff else 20,
        ac_gear_remap = isTRUE(input$ac_gear_remap)
      )
      
      cfg <- plot_catalogue[[plot_name]]
      missing <- setdiff(cfg$req, names(ld))
      if (length(missing) > 0) {
        p <- ggplot() +
          annotate("text", x = 0, y = 0, label = paste0("Cannot draw: ", plot_name, "\nMissing: ", paste(missing, collapse = ", ")), size = 6) +
          theme_void(base_size = 18)
        return(safe_ggplotly(p, show_hover = isTRUE(input$show_hover)))
      }
      
      p <- tryCatch({
        cfg$fn(ld, lap_id, pars)
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0, y = 0, label = paste("Plot failed:", e$message), size = 6) +
          theme_void(base_size = 18)
      })
      
      safe_ggplotly(p, show_hover = isTRUE(input$show_hover))
    }
    
    render_slot <- function(i) {
      renderPlotly({
        req(input$plots)
        if (length(input$plots) < i) return(NULL)
        make_plot(input$plots[[i]])
      })
    }
    
    output$plot1 <- render_slot(1)
    output$plot2 <- render_slot(2)
    output$plot3 <- render_slot(3)
    output$plot4 <- render_slot(4)
    
    output$status <- renderPrint({
      req(telemetry_oriented())
      df <- telemetry_oriented()
      cat("Rows:", nrow(df), "\n")
      cat("Columns:", ncol(df), "\n")
      if (!is.null(input$lap)) {
        cat("Selected lap:", input$lap, "\n")
        cat("Lap rows:", nrow(lap_data()), "\n")
      }
      if (!is.null(input$plots)) {
        cat("Selected plots:", paste(input$plots, collapse = " | "), "\n")
      }
      cat("Plot downsampling cap:", 12000, "points\n")
    })
  }
  
  shinyApp(ui, server)
}

## Run
track_mapping_interactive()
