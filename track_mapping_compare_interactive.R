options(shiny.maxRequestSize = 500 * 1024^2)

# Shared calculation functions (telemetry_core.R, same folder as this script)
local({
  script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  candidates <- c(file.path(script_dir, "telemetry_core.R"), "telemetry_core.R")
  core <- candidates[file.exists(candidates)][1]
  if (is.na(core)) {
    stop("telemetry_core.R not found. Keep all toolkit files in one folder and open ",
         "telemetry_to_insight.Rproj (or setwd() to that folder) before running this script.",
         call. = FALSE)
  }
  source(core)
})

track_mapping_compare <- function() {
  
  suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
    library(dplyr)
    library(readr)
    library(ggplot2)
    library(tidyr)
    library(purrr)
    library(viridis)
    library(scales)
    library(lubridate)
    library(tibble)
  })
  filter <- dplyr::filter

  # --------------------------------------------------
  # Helpers
  # Calculations (has_cols, prepare_lap_for_compare, resample_two_laps,
  # compute_lap_time_seconds, compute_seg_times_synthetic/real) are in
  # telemetry_core.R
  # --------------------------------------------------

  downsample_for_plot <- function(df, max_points = 12000) {
    if (nrow(df) <= max_points) return(df)
    step <- ceiling(nrow(df) / max_points)
    df[seq(1, nrow(df), by = step), , drop = FALSE]
  }
  
  safe_ggplotly <- function(p, show_hover = FALSE) {
    tooltip_arg <- if (isTRUE(show_hover)) "all" else "none"
    gg <- ggplotly(p, tooltip = tooltip_arg)

    gg <- layout(
      gg,
      hovermode = "closest",
      legend = list(
        orientation = "v",
        x = 1.02,
        xanchor = "left",
        y = 0.5,
        yanchor = "middle"
      )
    )

    plotly::toWebGL(gg)
  }
  
  make_error_plot <- function(msg) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = msg, size = 6) +
      theme_void(base_size = 18)
  }

  attr_label <- function(x) {
    labels <- c(accGHorizontal = "Lat. G", accGFrontal = "Long. G")
    if (x %in% names(labels)) labels[[x]] else x
  }
  
  format_seconds <- function(sec) {
    if (is.na(sec)) return("NA")
    
    minutes <- floor(sec / 60)
    seconds <- sec %% 60
    
    sprintf("%d:%05.2f", minutes, seconds)
  }
  
  # --------------------------------------------------
  # Categorical attribute config
  # --------------------------------------------------

  # Fixed named palette — same gear always gets the same color across both lap maps
  gear_palette <- c(
    "0" = "#999999",
    "1" = "#E41A1C",
    "2" = "#FF7F00",
    "3" = "#FFFF33",
    "4" = "#4DAF4A",
    "5" = "#377EB8",
    "6" = "#984EA3",
    "7" = "#A65628",
    "8" = "#F781BF"
  )

  # Palette for AC-remapped gears: R, N, 1–6
  gear_palette_remapped <- c(
    "R" = "#999999",
    "N" = "#E41A1C",
    "1" = "#FF7F00",
    "2" = "#FFFF33",
    "3" = "#4DAF4A",
    "4" = "#377EB8",
    "5" = "#984EA3",
    "6" = "#A65628"
  )

  # Binary flag palette: 0 = inactive (gray), 1 = active (red)
  flag_palette <- c("0" = "#CCCCCC", "1" = "#E41A1C")

  categorical_attributes <- c("gear", "isAbsInAction", "isTcInAction")

  attr_palette <- function(attribute, ac_gear_remap = FALSE) {
    if (attribute == "gear") {
      if (ac_gear_remap) gear_palette_remapped else gear_palette
    } else if (attribute %in% c("isAbsInAction", "isTcInAction")) {
      flag_palette
    } else {
      NULL
    }
  }

  # --------------------------------------------------
  # Plot functions
  # --------------------------------------------------

  plot_attribute_map_single <- function(lap_df, attribute, lap_label,
                                        ac_gear_remap = FALSE,
                                        base_family = "Times New Roman", base_size = 18) {
    if (!(attribute %in% names(lap_df))) {
      return(make_error_plot(paste("Missing attribute:", attribute)))
    }

    df <- lap_df |>
      mutate(lap_role = lap_label) |>
      downsample_for_plot()

    if (attribute %in% categorical_attributes) {
      if (attribute == "gear" && ac_gear_remap) {
        df <- df |> mutate(.attr_fac = case_when(
          .data[[attribute]] == 0 ~ "R",
          .data[[attribute]] == 1 ~ "N",
          TRUE ~ as.character(as.integer(.data[[attribute]]) - 1)
        ))
        gear_levels_r <- c("R", "N", as.character(1:6))
        present <- unique(df$.attr_fac)
        df <- df |> mutate(.attr_fac = factor(.attr_fac, levels = gear_levels_r[gear_levels_r %in% present]))
      } else {
        df <- df |> mutate(.attr_fac = factor(as.character(.data[[attribute]])))
      }

      ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
        geom_path(color = "gray40", linewidth = 0.8, alpha = 0.8) +
        geom_point(aes(color = .attr_fac), size = 1.4, alpha = 0.95) +
        scale_color_manual(values = attr_palette(attribute, ac_gear_remap), drop = TRUE) +
        coord_fixed() +
        labs(
          title = lap_label,
          x = "X Position",
          y = "Z Position",
          color = attr_label(attribute)
        ) +
        theme_classic(base_family = base_family, base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "grey92", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    } else if (attribute %in% c("accGHorizontal", "accGFrontal")) {
      clim <- max(abs(lap_df[[attribute]]), na.rm = TRUE)
      col_scale <- if (attribute == "accGFrontal") {
        scale_color_gradient2(low = "red", mid = "white", high = "green3", midpoint = 0, limits = c(-clim, clim))
      } else {
        scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-clim, clim))
      }
      ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
        geom_path(color = "gray40", linewidth = 0.8, alpha = 0.8) +
        geom_point(aes(color = .data[[attribute]]), size = 1.4, alpha = 0.95) +
        col_scale +
        coord_fixed() +
        labs(
          title = lap_label,
          x = "X Position",
          y = "Z Position",
          color = attr_label(attribute)
        ) +
        theme_classic(base_family = base_family, base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "grey92", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    } else if (attribute %in% c("speedKmh", "engineRPM", "gas")) {
      ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
        geom_path(color = "gray40", linewidth = 0.8, alpha = 0.8) +
        geom_point(aes(color = .data[[attribute]]), size = 1.4, alpha = 0.95) +
        scale_color_gradientn(colors = c("red3", "yellow", "green3")) +
        coord_fixed() +
        labs(
          title = lap_label,
          x = "X Position",
          y = "Z Position",
          color = attr_label(attribute)
        ) +
        theme_classic(base_family = base_family, base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "grey92", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    } else if (attribute == "brake") {
      ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
        geom_path(color = "gray40", linewidth = 0.8, alpha = 0.8) +
        geom_point(aes(color = .data[[attribute]]), size = 1.4, alpha = 0.95) +
        scale_color_gradient(low = "gray85", high = "red3") +
        coord_fixed() +
        labs(
          title = lap_label,
          x = "X Position",
          y = "Z Position",
          color = attr_label(attribute)
        ) +
        theme_classic(base_family = base_family, base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "grey92", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    } else {
      ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
        geom_path(color = "gray40", linewidth = 0.8, alpha = 0.8) +
        geom_point(aes(color = .data[[attribute]]), size = 1.4, alpha = 0.95) +
        scale_color_viridis_c(option = "plasma") +
        coord_fixed() +
        labs(
          title = lap_label,
          x = "X Position",
          y = "Z Position",
          color = attr_label(attribute)
        ) +
        theme_classic(base_family = base_family, base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "grey92", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
  }
  
  plot_attribute_trace <- function(comp_df, attribute, ref_label, cmp_label,
                                   base_family = "Times New Roman", base_size = 18) {
    ref_label <- sub(" \\| (composite )?reference.*", "", ref_label)
    cmp_label <- sub(" \\| comparison", "", cmp_label)
    ref_col <- paste0(attribute, "_ref")
    cmp_col <- paste0(attribute, "_cmp")
    
    if (!(ref_col %in% names(comp_df)) || !(cmp_col %in% names(comp_df))) {
      return(make_error_plot(paste("Missing comparison columns for:", attribute)))
    }
    
    long_df <- comp_df |>
      select(carPositionNormalized, all_of(ref_col), all_of(cmp_col)) |>
      rename(
        reference = all_of(ref_col),
        comparison = all_of(cmp_col)
      ) |>
      pivot_longer(cols = c(reference, comparison), names_to = "lap_role", values_to = "value") |>
      mutate(lap_role = recode(lap_role, reference = ref_label, comparison = cmp_label))
    
    ggplot(long_df, aes(x = carPositionNormalized, y = value, color = lap_role)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste(attr_label(attribute), "Trace"),
        x = "Normalized lap position",
        y = attr_label(attribute),
        color = NULL
      ) +
      theme_classic(base_family = base_family, base_size = base_size) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal"
      )
  }
  
  plot_delta_track <- function(comp_df, ref_df, cmp_df, attribute,
                               base_family = "Times New Roman", base_size = 18) {
    need <- c("carCoordinatesX", "carCoordinatesZ", "carPositionNormalized")
    if (!has_cols(ref_df, c(need, attribute)) || !has_cols(cmp_df, c(need, attribute))) {
      return(make_error_plot(paste("Missing columns for:", attribute)))
    }
    
    ref_line <- ref_df |>
      select(carPositionNormalized, carCoordinatesX, carCoordinatesZ) |>
      arrange(carPositionNormalized)
    
    map_df <- tibble(
      carPositionNormalized = comp_df$carPositionNormalized,
      carCoordinatesX = interp_numeric(
        ref_line$carPositionNormalized,
        ref_line$carCoordinatesX,
        comp_df$carPositionNormalized
      ),
      carCoordinatesZ = interp_numeric(
        ref_line$carPositionNormalized,
        ref_line$carCoordinatesZ,
        comp_df$carPositionNormalized
      ),
      delta = comp_df[[paste0(attribute, "_delta")]]
    ) |>
      downsample_for_plot()
    
    delta_scale <- if (attribute %in% c("speedKmh", "engineRPM", "gas", "accGFrontal")) {
      scale_color_gradient2(
        low = "red3",
        mid = "white",
        high = "green3",
        midpoint = 0
      )
    } else {
      scale_color_gradient2(
        low = "blue3",
        mid = "white",
        high = "red3",
        midpoint = 0
      )
    }
    
    ggplot(map_df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray35", linewidth = 1.0, alpha = 0.9) +
      geom_point(aes(color = delta), size = 1.8, alpha = 0.95) +
      delta_scale +
      coord_fixed() +
      labs(
        title = paste(attr_label(attribute), "Delta Map"),
        subtitle = "Comparison minus reference",
        x = "X Position",
        y = "Z Position",
        color = "Delta"
      ) +
      theme_classic(base_family = base_family, base_size = base_size) +
      theme(
        panel.background = element_rect(fill = "grey92", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
  
  plot_segment_time_delta <- function(ref_df, seg_times_ref, seg_times_cmp,
                                      ref_label, cmp_label,
                                      base_family = "Times New Roman", base_size = 18) {
    need_ref <- c("seg", "carPositionNormalized", "carCoordinatesX", "carCoordinatesZ")
    if (!has_cols(ref_df, need_ref)) {
      return(make_error_plot("Composite reference lap missing required columns"))
    }
    if (length(seg_times_ref) == 0) {
      return(make_error_plot("No segment times found in composite reference lap"))
    }
    
    segs <- names(seg_times_ref)
    
    seg_df <- tibble(
      seg         = segs,
      lapTime_ref = seg_times_ref[segs],
      lapTime_cmp = seg_times_cmp[segs]
    ) |>
      mutate(delta_s = (lapTime_cmp - lapTime_ref) / 1000) |>
      mutate(delta_s = ifelse(abs(delta_s) < 1e-9, 0, delta_s))
    
    if (all(is.na(seg_df$delta_s))) {
      return(make_error_plot("Could not compute segment deltas"))
    }
    
    track_df <- ref_df |>
      filter(!is.na(seg)) |>
      mutate(seg = as.character(seg)) |>
      left_join(
        seg_df |>
          mutate(seg = as.character(seg)) |>
          select(seg, delta_s),
        by = "seg"
      )
    
    abs_max <- suppressWarnings(max(abs(track_df$delta_s), na.rm = TRUE))
    if (!is.finite(abs_max) || abs_max == 0) abs_max <- 1
    
    # Segment boundary markers and label positions
    seg_marks <- ref_df |>
      filter(!is.na(seg),
             is.finite(lapTime),
             is.finite(carCoordinatesX),
             is.finite(carCoordinatesZ)) |>
      mutate(
        seg = as.character(seg),
        # order each segment from its start boundary (keeps a segment that
        # crosses the start/finish line in one piece)
        .ord = if ("start" %in% names(ref_df)) (carPositionNormalized - start) %% 1 else lapTime
      ) |>
      semi_join(
        seg_df |>
          mutate(seg = as.character(seg)) |>
          select(seg),
        by = "seg"
      ) |>
      arrange(seg, .ord) |>
      group_by(seg) |>
      summarise(
        start_x = dplyr::first(carCoordinatesX),
        start_z = dplyr::first(carCoordinatesZ),
        mid_x   = carCoordinatesX[ceiling(dplyr::n() / 2)],
        mid_z   = carCoordinatesZ[ceiling(dplyr::n() / 2)],
        .groups = "drop"
      ) |>
      left_join(
        seg_df |>
          mutate(seg = as.character(seg)) |>
          select(seg, delta_s),
        by = "seg"
      ) |>
      mutate(
        delta_label = ifelse(
          is.na(delta_s),
          "NA",
          sprintf("%+.1fs", delta_s)
        )
      )
    
    boundary_marks <- seg_marks |>
      transmute(x = start_x, z = start_z)
    
    ggplot(track_df, aes(x = carCoordinatesX, y = carCoordinatesZ)) +
      geom_path(color = "gray35", linewidth = 1.0, alpha = 0.9) +
      geom_point(aes(color = delta_s), size = 1.8, alpha = 0.95) +
      geom_point(
        data = boundary_marks,
        aes(x = x, y = z),
        inherit.aes = FALSE,
        shape = 21,
        size = 2.6,
        stroke = 0.8,
        fill = "white",
        color = "black"
      ) +
      geom_text(
        data = seg_marks,
        aes(x = mid_x, y = mid_z, label = delta_label),
        inherit.aes = FALSE,
        size = 4.2,
        vjust = -0.8,
        color = "black"
      ) +
      scale_color_gradient2(
        low = "green3",
        mid = "white",
        high = "red3",
        midpoint = 0,
        limits = c(-abs_max, abs_max),
        name = "Delta (s)"
      ) +
      coord_fixed() +
      labs(
        title = "Segment Time Delta Map",
        subtitle = paste0(cmp_label, "  −  ", ref_label, "  [positive = comparison slower]"),
        x = "X Position",
        y = "Z Position"
      ) +
      theme_classic(base_family = base_family, base_size = base_size) +
      theme(
        panel.background = element_rect(fill = "grey92", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }
  # --------------------------------------------------
  # Plot catalog
  # --------------------------------------------------
  plot_catalogue <- list(
    "Reference map" = list(
      fn = function(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute, ac_gear_remap = FALSE) {
        plot_attribute_map_single(ref_df, attribute, ref_label, ac_gear_remap = ac_gear_remap)
      }
    ),
    "Comparison map" = list(
      fn = function(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute, ac_gear_remap = FALSE) {
        plot_attribute_map_single(cmp_df, attribute, cmp_label, ac_gear_remap = ac_gear_remap)
      }
    ),
    "Delta map" = list(
      fn = function(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute) {
        plot_delta_track(comp_df, ref_df, cmp_df, attribute)
      }
    ),
    "Attribute trace" = list(
      fn = function(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute) {
        plot_attribute_trace(comp_df, attribute, ref_label, cmp_label)
      }
    ),
    "Segment time delta" = list(
      fn = function(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute,
                    seg_times_ref, seg_times_cmp) {
        plot_segment_time_delta(ref_df, seg_times_ref, seg_times_cmp, ref_label, cmp_label)
      }
    )
  )
  
  # --------------------------------------------------
  # UI
  # --------------------------------------------------
  
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body { font-size: 18px; }
        .btn { font-size: 18px; }
        .form-control { font-size: 18px; }
        .selectize-input { font-size: 18px; }
        .lap-times { font-weight: bold; font-size: 18px; margin-bottom: 10px; }
      "))
    ),
    titlePanel("Track Mapping | Lap Comparison"),
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file", "Upload Telemetry CSV", accept = ".csv"),
        uiOutput("lap_selector_ui"),
        tags$hr(),
        radioButtons(
          "reference_choice",
          "Reference lap",
          choices = c("Lap A", "Lap B"),
          selected = "Lap A"
        ),
        checkboxInput("reference_is_synthetic", "Reference lap is the composite reference lap", value = FALSE),
        tags$hr(),
        checkboxInput("flip_horizontal", "Flip horizontal (mirror X)", value = TRUE),
        checkboxInput("flip_vertical", "Flip vertical (mirror Z)", value = FALSE),
        tags$hr(),
        uiOutput("attribute_selector_ui"),
        conditionalPanel(
          condition = "input.attribute == 'gear'",
          checkboxInput("ac_gear_remap", "Set gear encoding (0=R, 1=N, 2–7 = gears 1–6)", value = FALSE)
        ),
        numericInput("n_interp", "Interpolation points", value = 3000, min = 200, max = 10000, step = 100),
        checkboxInput("show_hover", "Show hover data", value = FALSE),
        tags$hr(),
        helpText(tags$b("Deltas are computed as comparison minus reference on a common normalized lap axis."))
      ),
      mainPanel(
        fluidRow(
          column(6, div(class = "lap-times", textOutput("lap_time_a"))),
          column(6, div(class = "lap-times", textOutput("lap_time_b")))
        ),
        fluidRow(
          column(6, plotlyOutput("plot1", height = "420px")),
          column(6, plotlyOutput("plot2", height = "420px"))
        ),
        fluidRow(
          column(6, plotlyOutput("plot3", height = "420px")),
          column(6, plotlyOutput("plot4", height = "420px"))
        ),
        uiOutput("plot5_ui"),
        tags$hr(),
        verbatimTextOutput("status")
      )
    )
  )
  
  # --------------------------------------------------
  # Server
  # --------------------------------------------------
  
  server <- function(input, output, session) {
    
    telemetry_raw <- reactive({
      req(input$file)
      read_csv(input$file$datapath, show_col_types = FALSE)
    })
    
    telemetry_oriented <- reactive({
      df <- telemetry_raw()
      need <- c("lapCount", "carCoordinatesX", "carCoordinatesZ", "carPositionNormalized")
      miss <- setdiff(need, names(df))
      validate(need(length(miss) == 0, paste("Missing required columns:", paste(miss, collapse = ", "))))
      
      df |>
        mutate(
          carCoordinatesX = if (isTRUE(input$flip_horizontal)) -carCoordinatesX else carCoordinatesX,
          carCoordinatesZ = if (isTRUE(input$flip_vertical)) -carCoordinatesZ else carCoordinatesZ
        )
    })
    
    output$lap_selector_ui <- renderUI({
      req(telemetry_oriented())
      laps <- sort(unique(telemetry_oriented()$lapCount))
      
      tagList(
        selectInput("lap_a", "Select Lap A", choices = laps, selected = laps[1]),
        selectInput("lap_b", "Select Lap B", choices = laps, selected = laps[min(2, length(laps))])
      )
    })
    
    output$attribute_selector_ui <- renderUI({
      req(telemetry_oriented())
      
      allowed_attributes <- c(
        "speedKmh",
        "engineRPM",
        "gear",
        "gas",
        "brake",
        "steer",
        "isAbsInAction",
        "isTcInAction",
        "accGHorizontal",
        "accGFrontal"
      )
      
      available_attributes <- intersect(allowed_attributes, names(telemetry_oriented()))
      
      validate(need(length(available_attributes) > 0, "None of the requested comparison attributes are available in the file"))
      
      default_attr <- if ("speedKmh" %in% available_attributes) "speedKmh" else available_attributes[1]
      
      attr_display <- setNames(available_attributes, sapply(available_attributes, attr_label))
      selectInput("attribute", "Comparison attribute", choices = attr_display, selected = default_attr)
    })
    
    lap_a_data <- reactive({
      req(telemetry_oriented(), input$lap_a)
      telemetry_oriented() |>
        filter(lapCount == input$lap_a) |>
        prepare_lap_for_compare()
    })
    
    lap_b_data <- reactive({
      req(telemetry_oriented(), input$lap_b)
      telemetry_oriented() |>
        filter(lapCount == input$lap_b) |>
        prepare_lap_for_compare()
    })
    
    output$lap_time_a <- renderText({
      req(lap_a_data())
      sec <- compute_lap_time_seconds(lap_a_data())
      paste0("Lap A (", input$lap_a, ") time: ", format_seconds(sec))
    })
    
    output$lap_time_b <- renderText({
      req(lap_b_data())
      sec <- compute_lap_time_seconds(lap_b_data())
      paste0("Lap B (", input$lap_b, ") time: ", format_seconds(sec))
    })
    
    ref_cmp_data <- reactive({
      req(lap_a_data(), lap_b_data(), input$reference_choice, input$attribute)
      
      if (identical(input$reference_choice, "Lap A")) {
        ref_df   <- lap_a_data()
        cmp_df   <- lap_b_data()
        ref_base <- paste("Lap", input$lap_a)
        cmp_base <- paste("Lap", input$lap_b)
        raw_ref  <- telemetry_oriented() |> filter(lapCount == input$lap_a)
        raw_cmp  <- telemetry_oriented() |> filter(lapCount == input$lap_b)
      } else {
        ref_df   <- lap_b_data()
        cmp_df   <- lap_a_data()
        ref_base <- paste("Lap", input$lap_b)
        cmp_base <- paste("Lap", input$lap_a)
        raw_ref  <- telemetry_oriented() |> filter(lapCount == input$lap_b)
        raw_cmp  <- telemetry_oriented() |> filter(lapCount == input$lap_a)
      }
      
      ref_label <- if (isTRUE(input$reference_is_synthetic)) {
        paste0(ref_base, " | composite reference")
      } else {
        paste0(ref_base, " | reference")
      }
      
      cmp_label <- paste0(cmp_base, " | comparison")
      
      vars_needed <- unique(c(
        "carCoordinatesX", "carCoordinatesZ", "carCoordinatesY",
        "speedKmh", "engineRPM", "gear", "gas", "brake", "steer",
        "isAbsInAction", "isTcInAction",
        "accGHorizontal", "accGFrontal",
        input$attribute
      ))
      
      abs_preprocess <- function(df) {
        if ("accGHorizontal" %in% names(df)) df <- mutate(df, accGHorizontal = abs(accGHorizontal))
        if ("steer" %in% names(df))          df <- mutate(df, steer = abs(steer))
        df
      }
      comp_df <- resample_two_laps(abs_preprocess(ref_df), abs_preprocess(cmp_df), vars = vars_needed, n_points = input$n_interp)
      syn <- compute_seg_times_synthetic(raw_ref)
      
      list(
        ref_df        = ref_df,
        cmp_df        = cmp_df,
        comp_df       = comp_df,
        ref_label     = ref_label,
        cmp_label     = cmp_label,
        seg_times_ref = syn$times,
        seg_times_cmp = compute_seg_times_real(raw_cmp, syn$bounds)
      )
    })
    
    output$plot5_ui <- renderUI({
      if (isTRUE(input$reference_is_synthetic)) {
        tagList(
          tags$hr(),
          fluidRow(
            column(12, plotlyOutput("plot5", height = "420px"))
          )
        )
      }
    })
    
    output$plot5 <- renderPlotly({
      req(input$reference_is_synthetic)
      make_plot("Segment time delta")
    })
    
    make_plot <- function(plot_name) {
      req(plot_name)
      
      dat <- ref_cmp_data()
      ref_df        <- dat$ref_df
      cmp_df        <- dat$cmp_df
      comp_df       <- dat$comp_df
      ref_label     <- dat$ref_label
      cmp_label     <- dat$cmp_label
      attribute     <- input$attribute
      seg_times_ref <- dat$seg_times_ref
      seg_times_cmp <- dat$seg_times_cmp
      
      cfg <- plot_catalogue[[plot_name]]
      
      p <- tryCatch({
        if (plot_name == "Segment time delta") {
          cfg$fn(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute,
                 seg_times_ref, seg_times_cmp)
        } else if (plot_name %in% c("Reference map", "Comparison map")) {
          cfg$fn(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute,
                 ac_gear_remap = isTRUE(input$ac_gear_remap))
        } else {
          cfg$fn(ref_df, cmp_df, comp_df, ref_label, cmp_label, attribute)
        }
      }, error = function(e) {
        make_error_plot(paste("Plot failed:", e$message))
      })
      
      safe_ggplotly(p, show_hover = isTRUE(input$show_hover))
    }
    
    output$plot1 <- renderPlotly({
      make_plot("Reference map")
    })
    
    output$plot2 <- renderPlotly({
      make_plot("Comparison map")
    })
    
    output$plot3 <- renderPlotly({
      make_plot("Delta map")
    })
    
    output$plot4 <- renderPlotly({
      make_plot("Attribute trace")
    })
    
    output$status <- renderPrint({
      req(telemetry_oriented(), ref_cmp_data())
      dat <- ref_cmp_data()
      
      cat("Rows:", nrow(telemetry_oriented()), "\n")
      cat("Columns:", ncol(telemetry_oriented()), "\n")
      cat("Lap A rows:", nrow(lap_a_data()), "\n")
      cat("Lap B rows:", nrow(lap_b_data()), "\n")
      cat("Reference:", dat$ref_label, "\n")
      cat("Comparison:", dat$cmp_label, "\n")
      cat("Attribute:", input$attribute, "\n")
      cat("Interpolation points:", input$n_interp, "\n")
      cat("Plot set: Reference map | Comparison map | Delta map | Attribute trace",
          if (isTRUE(input$reference_is_synthetic)) "| Segment time delta" else "", "\n")
      cat("Plot downsampling cap:", 12000, "points\n")
      if (isTRUE(input$reference_is_synthetic)) {
        cat("Composite reference segments available:", length(dat$seg_times_ref), "\n")
        cat("Comparison segment matches:", sum(is.finite(dat$seg_times_cmp)), "\n")
      }
    })
  }
  
  shinyApp(ui, server)
}

# to run:
track_mapping_compare()
