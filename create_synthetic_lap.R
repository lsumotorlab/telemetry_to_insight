##### create_synthetic_lap.R - composite reference lap builder
# Single-file interactive workflow to:
# 1) Upload telemetry CSV
# 2) Select reference lap
# 3) Select sector boundaries on the track (click workflow)
# 4) Build and append a composite reference lap: the fastest observed sector
#    from any lap, each keeping its source lap's own timing
# 5) Download the updated dataset
#
# The composite lap is marked with .synthetic = TRUE in the output CSV.

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

# ----------------------------
# Core functions
# ----------------------------

choose_bounds_shiny <- function(
    telemetry,
    lap_id,
    lap_col = "lapCount",
    pos_col = "carPositionNormalized",
    x_col = "carCoordinatesX",
    z_col = "carCoordinatesZ",
    time_col = "lapTime",
    snap_k = 50
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(shiny)
    library(plotly)
  })
  
  df <- telemetry %>%
    filter(.data[[lap_col]] == lap_id) %>%
    arrange(.data[[pos_col]], .data[[time_col]]) %>%
    mutate(
      .x = .data[[x_col]],
      .y = .data[[z_col]],
      .pos = .data[[pos_col]]
    )
  
  if (nrow(df) < 50) stop("Not enough samples for lap ", lap_id)
  
  ui <- fluidPage(
    tags$h4(paste0("Click boundaries for lap ", lap_id)),
    plotlyOutput("track", height = "650px"),
    fluidRow(
      column(4, actionButton("undo", "Undo last")),
      column(4, actionButton("clear", "Clear")),
      column(4, actionButton("done", "Done"))
    ),
    tags$hr(),
    verbatimTextOutput("status")
  )
  
  server <- function(input, output, session) {
    clicks <- reactiveVal(numeric(0))
    
    output$track <- renderPlotly({
      v <- clicks()

      marker_df <- if (length(v) > 0) {
        rows <- vapply(v, function(p) which.min(abs(df$.pos - p)), integer(1))
        df[rows, ]
      } else {
        df[integer(0), ]
      }

      plot_ly(df, x = ~.x, y = ~.y, type = "scatter", mode = "lines", source = "A") %>%
        add_markers(
          data = marker_df,
          x = ~.x, y = ~.y,
          marker = list(color = "red", size = 10, symbol = "circle"),
          showlegend = FALSE
        ) %>%
        layout(
          xaxis = list(title = x_col),
          yaxis = list(title = z_col, scaleanchor = "x", scaleratio = 1)
        )
    })
    
    observeEvent(event_data("plotly_click", source = "A"), ignoreNULL = TRUE, {
      ed <- event_data("plotly_click", source = "A")
      if (is.null(ed) || nrow(ed) == 0) return()
      
      cx <- ed$x[1]
      cy <- ed$y[1]
      
      d2 <- (df$.x - cx)^2 + (df$.y - cy)^2
      k <- min(snap_k, length(d2))
      cand <- order(d2)[seq_len(k)]
      j <- cand[1]
      
      clicks(c(clicks(), df$.pos[j]))
    })
    
    observeEvent(input$undo, {
      v <- clicks()
      if (length(v) > 0) clicks(v[-length(v)])
    })
    
    observeEvent(input$clear, {
      clicks(numeric(0))
    })
    
    output$status <- renderPrint({
      v <- sort(unique(round(clicks(), 6)))
      list(n_clicks = length(v), positions = v)
    })
    
    observeEvent(input$done, {
      v <- sort(unique(round(clicks(), 6)))
      if (length(v) < 2) {
        showNotification("Need at least 2 boundaries", type = "error")
        return()
      }
      v_ext <- c(v, v[1])
      bounds <- tibble(seg = seq_len(length(v)), start = v, end = v_ext[-1])
      stopApp(list(bounds = bounds, clicked_positions = v, lap_id = lap_id))
    })
  }
  
  shinyApp(ui, server) %>% runApp(launch.browser = TRUE)
}

# Calculations (sector times, fastest-sector selection, composite lap
# retiming) are in telemetry_core.R: build_synthetic_lap_from_bounds(),
# resample_and_retime_synthetic_lap(), unify_from_speed_output()

plot_synth_segments_xy <- function(
    synth,
    x_col = "carCoordinatesX",
    y_col = "carCoordinatesZ",
    seg_col = "seg",
    lap_col = "source_lap",
    order_col = "lapTime",
    show_endpoints = TRUE,
    label_segments = TRUE
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
  })
  
  need <- c(x_col, y_col, seg_col, lap_col)
  miss <- setdiff(need, names(synth))
  if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  df <- synth %>%
    mutate(
      .x = .data[[x_col]],
      .y = .data[[y_col]],
      .seg = .data[[seg_col]],
      .lap = factor(.data[[lap_col]])
    )
  
  if (all(c("start", "carPositionNormalized") %in% names(df))) {
    # Order each sector from its start boundary; keeps a sector that crosses
    # the start/finish line in one piece
    df <- df %>% arrange(.seg, (carPositionNormalized - start) %% 1, carPositionNormalized)
  } else if (!is.null(order_col) && order_col %in% names(df)) {
    df <- df %>% arrange(.data[[order_col]])
  } else {
    if ("carPositionNormalized" %in% names(df)) {
      df <- df %>% arrange(carPositionNormalized)
    }
  }
  
  bnd <- df %>%
    group_by(.seg) %>%
    slice(c(1, dplyr::n())) %>%
    ungroup() %>%
    mutate(.endpoint = rep(c("start", "end"), times = dplyr::n_distinct(df$.seg)))
  
  p <- ggplot(df, aes(x = .x, y = .y)) +
    geom_path(aes(group = .seg, colour = .lap), linewidth = 0.9) +
    coord_equal() +
    labs(
      x = "X (m)",
      y = "Z (m)",
      colour = "Lap used",
      title = "Track Layout - composite reference lap"
    ) +
    theme_minimal()
  
  if (show_endpoints) {
    p <- p +
      geom_point(
        data = bnd,
        aes(colour = .lap),
        shape = 16,
        size = 2.6
      )
  }
  
  if (label_segments) {
    labs_df <- df %>%
      group_by(.seg) %>%
      slice(1) %>%
      ungroup()
    p <- p + geom_text(data = labs_df, aes(label = .seg), size = 3, vjust = -0.8)
  }
  
  p
}

# ----------------------------
# Small gadgets to keep things sequential
# ----------------------------

.upload_select_gadget <- function() {
  suppressPackageStartupMessages({
    library(shiny)
    library(readr)
    library(dplyr)
    library(plotly)
  })
  
  ui <- fluidPage(
    tags$h3("Composite reference lap builder"),
    tags$p("Assembles the fastest observed sector from each lap. The result is a reference",
           "built from recorded sectors, not a lap shown to be drivable as a whole."),
    fileInput("csv", "Upload telemetry CSV", accept = c(".csv")),
    fluidRow(
      column(4, checkboxInput("flip_h", "Flip horizontal (X)", value = TRUE)),
      column(4, checkboxInput("flip_v", "Flip vertical (Z)", value = FALSE)),
      column(4, numericInput("snap_k", "Snap search points", value = 50, min = 1, step = 1))
    ),
    tags$hr(),
    uiOutput("lap_ui"),
    tags$h4("Reference lap preview"),
    plotlyOutput("lap_preview", height = "320px"),
    tags$hr(),
    fluidRow(
      column(6, numericInput("n_bins", "Resample bins (points in composite lap)", value = NA, min = 200, step = 50)),
      column(6, numericInput("min_samples", "Min samples per segment per lap", value = 10, min = 1, step = 1))
    ),
    tags$hr(),
    actionButton("go", "Continue to segment selection", class = "btn-primary"),
    tags$hr(),
    verbatimTextOutput("status")
  )
  
  server <- function(input, output, session) {
    telemetry_rv <- reactiveVal(NULL)
    
    observeEvent(input$csv, {
      req(input$csv$datapath)
      df <- readr::read_csv(input$csv$datapath, show_col_types = FALSE)
      
      need <- c("lapCount", "carPositionNormalized", "carCoordinatesX", "carCoordinatesZ", "lapTime")
      miss <- setdiff(need, names(df))
      if (length(miss) > 0) {
        showNotification(paste("Missing required columns:", paste(miss, collapse = ", ")), type = "error")
        telemetry_rv(NULL)
        return()
      }
      
      telemetry_rv(df)
      
      # default n_bins based on first lap length
      first_lap <- df$lapCount[which(df$lapCount > 0)[1]]
      if (!is.na(first_lap)) {
        nb <- df %>% dplyr::filter(lapCount == first_lap) %>% dplyr::count() %>% dplyr::pull(n)
        updateNumericInput(session, "n_bins", value = nb)
      }
    })
    
    output$lap_ui <- renderUI({
      df <- telemetry_rv()
      if (is.null(df)) return(tags$em("Upload a CSV to continue."))
      laps <- sort(unique(df$lapCount))
      laps <- laps[laps > 0]
      selectInput("lap_id", "Reference lap for track and boundary selection", choices = laps, selected = laps[1])
    })
    
    
    
    output$lap_preview <- renderPlotly({
      df <- telemetry_rv()
      req(df)
      req(input$lap_id)
      
      dff <- df
      if (isTRUE(input$flip_h)) dff$carCoordinatesX <- -dff$carCoordinatesX
      if (isTRUE(input$flip_v)) dff$carCoordinatesZ <- -dff$carCoordinatesZ
      
      lap_df <- dff %>%
        dplyr::filter(as.character(lapCount) == as.character(input$lap_id)) %>%
        dplyr::arrange(carPositionNormalized)
      
      if (nrow(lap_df) < 2) {
        return(plotly::plot_ly() %>% plotly::layout(
          annotations = list(list(text = "No rows found for selected lap", x = 0.5, y = 0.5, showarrow = FALSE)),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        ) %>% plotly::config(displayModeBar = FALSE))
      }
      
      plotly::plot_ly(lap_df, x = ~carCoordinatesX, y = ~carCoordinatesZ,
                      type = "scatter", mode = "lines") %>%
        plotly::layout(
          xaxis = list(title = "carCoordinatesX"),
          yaxis = list(title = "carCoordinatesZ", scaleanchor = "x", scaleratio = 1),
          margin = list(l = 60, r = 20, t = 10, b = 60)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$status <- renderPrint({
      df <- telemetry_rv()
      if (is.null(df)) return(list(status = "waiting for file"))
      laps <- sort(unique(df$lapCount))
      laps <- laps[laps > 0]
      list(
        rows = nrow(df),
        laps = length(laps),
        columns = length(names(df))
      )
    })
    
    observeEvent(input$go, {
      df <- telemetry_rv()
      req(df)
      req(input$lap_id)
      req(input$n_bins)
      
      stopApp(list(
        telemetry = df,
        lap_id = as.integer(input$lap_id),
        flip_h = isTRUE(input$flip_h),
        flip_v = isTRUE(input$flip_v),
        snap_k = as.integer(input$snap_k),
        n_bins = as.integer(input$n_bins),
        min_samples = as.integer(input$min_samples)
      ))
    })
  }
  
  shinyApp(ui, server) %>% runApp(launch.browser = TRUE)
}

.preview_download_gadget <- function(telemetry_with_synth, synth_lap_plot, default_name = "telemetry_with_composite_lap.csv") {
  suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
    library(readr)
  })
  
  ui <- fluidPage(
    tags$h3("Composite reference lap created"),
    plotOutput("plot", height = "650px"),
    tags$hr(),
    downloadButton("dl", "Download updated CSV"),
    tags$hr(),
    actionButton("close", "Close")
  )
  
  server <- function(input, output, session) {
    output$plot <- renderPlot({
      synth_lap_plot
    })
    
    output$dl <- downloadHandler(
      filename = function() default_name,
      content = function(file) {
        readr::write_csv(telemetry_with_synth, file)
      }
    )
    
    observeEvent(input$close, {
      stopApp(TRUE)
    })
  }
  
  shinyApp(ui, server) %>% runApp(launch.browser = TRUE)
}

# ----------------------------
# Public entry point
# ----------------------------

synthetic_lap_interactive <- function(
    include_boundary = "left",
    keep_provenance = TRUE,
    new_lap_id = NULL
) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  # Step 1: upload and basic options
  sel <- .upload_select_gadget()
  telemetry <- sel$telemetry
  
  # Apply track orientation transforms BEFORE selection and processing
  telemetry <- telemetry %>%
    mutate(
      carCoordinatesX = if (isTRUE(sel$flip_h)) -carCoordinatesX else carCoordinatesX,
      carCoordinatesZ = if (isTRUE(sel$flip_v)) -carCoordinatesZ else carCoordinatesZ
    )
  
  # Step 2: pick segment boundaries on reference lap
  manual <- choose_bounds_shiny(
    telemetry = telemetry,
    lap_id = sel$lap_id,
    snap_k = sel$snap_k
  )
  
  # Step 3: build composite reference lap
  out <- build_synthetic_lap_from_bounds(
    telemetry = telemetry,
    bounds = manual$bounds,
    include_boundary = include_boundary,
    min_samples_per_seg = sel$min_samples
  )
  
  # Resample on a position grid, keeping each source lap's own timing
  # (positions are already in the original lap coordinate system)
  out$synthetic_lap <- resample_and_retime_synthetic_lap(
    telemetry = telemetry,
    segments = out$segments,
    n_bins = sel$n_bins,
    keep_provenance = keep_provenance
  )
  
  if (is.null(new_lap_id)) {
    new_lap_id <- max(telemetry$lapCount, na.rm = TRUE) + 1L
  }
  
  u <- unify_from_speed_output(
    speed_output = out,
    telemetry = telemetry,
    new_lap = new_lap_id,
    keep_provenance = keep_provenance
  )
  
  syn <- u$synthetic_lap
  
  # Plot (uses the same plotting function)
  p <- plot_synth_segments_xy(syn)
  
  # Optional: flip back for output so it matches the original coordinate convention
  telemetry_out <- u$telemetry_with_synth %>%
    mutate(
      carCoordinatesX = if (isTRUE(sel$flip_h)) -carCoordinatesX else carCoordinatesX,
      carCoordinatesZ = if (isTRUE(sel$flip_v)) -carCoordinatesZ else carCoordinatesZ
    )
  
  # Step 4: preview and download
  .preview_download_gadget(
    telemetry_with_synth = telemetry_out,
    synth_lap_plot = p
  )
  
  invisible(list(
    telemetry_with_synth = telemetry_out,
    synthetic_lap = syn,
    segments = out$segments,
    bounds = manual$bounds
  ))
}
## Run
synthetic_lap_interactive()
