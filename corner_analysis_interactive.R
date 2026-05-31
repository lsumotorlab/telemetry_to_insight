options(shiny.maxRequestSize = 500 * 1024^2)

# ---- Top-level utilities ----

.ca_has_cols <- function(df, cols) all(cols %in% names(df))

.ca_interp <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2) return(rep(NA_real_, length(xout)))
  ord <- order(x)
  x <- x[ord]; y <- y[ord]
  dup <- duplicated(x)
  x <- x[!dup]; y <- y[!dup]
  if (length(x) < 2) return(rep(NA_real_, length(xout)))
  approx(x, y, xout, rule = 2)$y
}

.ca_lowpass <- function(x, cutoff_hz, fs) {
  if (is.na(fs) || fs <= 0 || cutoff_hz <= 0 || cutoff_hz >= fs / 2) return(x)
  W  <- cutoff_hz / (fs / 2)
  bf <- signal::butter(2, W, type = "low")
  as.numeric(signal::filtfilt(bf, x))
}

# ---- Window 1: Upload & Setup ----

.ca_upload_gadget <- function() {
  suppressPackageStartupMessages({
    library(shiny)
    library(readr)
    library(dplyr)
    library(plotly)
  })

  ui <- fluidPage(
    tags$head(tags$style(HTML("body { font-size: 16px; }"))),
    tags$h3("Corner Analysis — Step 1: Upload & Setup"),
    fileInput("csv", "Upload telemetry CSV", accept = ".csv"),
    fluidRow(
      column(6, checkboxInput("flip_h", "Flip horizontal (X)", value = TRUE)),
      column(6, checkboxInput("flip_v", "Flip vertical (Z)", value = FALSE))
    ),
    tags$hr(),
    uiOutput("lap_ui"),
    tags$h4("Reference lap preview"),
    plotlyOutput("preview", height = "320px"),
    tags$hr(),
    actionButton("go", "Continue to corner selection", class = "btn-primary"),
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
        showNotification(paste("Missing columns:", paste(miss, collapse = ", ")), type = "error")
        telemetry_rv(NULL)
        return()
      }
      telemetry_rv(df)
    })

    oriented_rv <- reactive({
      df <- telemetry_rv()
      req(df)
      df |> mutate(
        carCoordinatesX = if (isTRUE(input$flip_h)) -carCoordinatesX else carCoordinatesX,
        carCoordinatesZ = if (isTRUE(input$flip_v)) -carCoordinatesZ else carCoordinatesZ
      )
    })

    output$lap_ui <- renderUI({
      df <- telemetry_rv()
      if (is.null(df)) return(tags$em("Upload a CSV to continue."))
      laps <- sort(unique(df$lapCount))
      laps <- laps[laps > 0]
      selectInput("lap_id", "Reference lap for track map", choices = laps, selected = laps[1])
    })

    output$preview <- renderPlotly({
      df <- oriented_rv()
      req(df, input$lap_id)

      lap_df <- df |>
        dplyr::filter(lapCount == as.integer(input$lap_id)) |>
        arrange(carPositionNormalized)

      if (nrow(lap_df) < 2) {
        return(plotly::plot_ly() |> plotly::layout(
          annotations = list(list(text = "No rows for this lap", x = 0.5, y = 0.5, showarrow = FALSE))
        ))
      }

      plotly::plot_ly(lap_df, x = ~carCoordinatesX, y = ~carCoordinatesZ,
                      type = "scatter", mode = "lines") |>
        plotly::layout(
          xaxis = list(title = "X"),
          yaxis = list(title = "Z", scaleanchor = "x", scaleratio = 1),
          margin = list(l = 40, r = 20, t = 10, b = 40)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$status <- renderPrint({
      df <- telemetry_rv()
      if (is.null(df)) return("Waiting for file...")
      laps <- sort(unique(df$lapCount[df$lapCount > 0]))
      list(rows = nrow(df), laps = laps, columns = ncol(df))
    })

    observeEvent(input$go, {
      df <- telemetry_rv()
      req(df, input$lap_id)

      oriented <- df |> mutate(
        carCoordinatesX = if (isTRUE(input$flip_h)) -carCoordinatesX else carCoordinatesX,
        carCoordinatesZ = if (isTRUE(input$flip_v)) -carCoordinatesZ else carCoordinatesZ
      )

      stopApp(list(
        telemetry = oriented,
        lap_id    = as.integer(input$lap_id),
        flip_h    = isTRUE(input$flip_h),
        flip_v    = isTRUE(input$flip_v)
      ))
    })
  }

  shinyApp(ui, server) |> runApp(launch.browser = TRUE)
}

# ---- Window 2: Corner Selection ----

.ca_corner_select_gadget <- function(telemetry, lap_id) {
  suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
    library(dplyr)
  })

  ref_df <- telemetry |>
    dplyr::filter(lapCount == lap_id) |>
    arrange(carPositionNormalized) |>
    mutate(.x = carCoordinatesX, .z = carCoordinatesZ, .pos = carPositionNormalized)

  if (nrow(ref_df) < 20) stop("Not enough samples for lap ", lap_id)

  ui <- fluidPage(
    tags$head(tags$style(HTML("body { font-size: 16px; }"))),
    tags$h3(paste0("Corner Analysis — Step 2: Define Corner (Lap ", lap_id, ")")),
    fluidRow(
      column(8, plotlyOutput("track", height = "600px")),
      column(4,
        wellPanel(
          tags$h4("Instructions"),
          uiOutput("instructions"),
          tags$hr(),
          uiOutput("marker_summary"),
          tags$hr(),
          actionButton("reset", "Reset", class = "btn-warning"),
          actionButton("done", "Confirm corner", class = "btn-primary",
                       style = "margin-left: 10px;")
        )
      )
    )
  )

  server <- function(input, output, session) {
    entry_pos <- reactiveVal(NULL)
    exit_pos  <- reactiveVal(NULL)

    state <- reactive({
      if (is.null(entry_pos())) 0L
      else if (is.null(exit_pos())) 1L
      else 2L
    })

    output$instructions <- renderUI({
      switch(state() + 1L,
        tags$p(style = "color: green; font-weight: bold;",
               "Step 1: Click the corner ENTRY point on the track."),
        tags$p(style = "color: #cc6600; font-weight: bold;",
               "Step 2: Click the corner EXIT point on the track."),
        tags$p(style = "color: #333;",
               "Corner defined. Click Confirm or Reset to start over.")
      )
    })

    output$marker_summary <- renderUI({
      tagList(
        tags$p(paste("Entry position:", if (is.null(entry_pos())) "—" else round(entry_pos(), 4))),
        tags$p(paste("Exit position:",  if (is.null(exit_pos()))  "—" else round(exit_pos(),  4)))
      )
    })

    output$track <- renderPlotly({
      ep <- entry_pos()
      xp <- exit_pos()

      p <- plot_ly(source = "corner_map") |>
        add_trace(
          data = ref_df, x = ~.x, y = ~.z,
          type = "scatter", mode = "lines",
          line = list(color = "grey60", width = 2),
          showlegend = FALSE, hoverinfo = "none"
        )

      if (!is.null(ep) && !is.null(xp)) {
        lo <- min(ep, xp); hi <- max(ep, xp)
        section <- ref_df |> dplyr::filter(.pos >= lo & .pos <= hi)
        if (nrow(section) > 0) {
          p <- p |> add_trace(
            data = section, x = ~.x, y = ~.z,
            type = "scatter", mode = "lines",
            line = list(color = "orange", width = 5),
            name = "Corner", showlegend = TRUE, hoverinfo = "none"
          )
        }
      }

      if (!is.null(ep)) {
        erow <- ref_df[which.min(abs(ref_df$.pos - ep)), ]
        p <- p |> add_trace(
          data = erow, x = ~.x, y = ~.z,
          type = "scatter", mode = "markers",
          marker = list(color = "green", size = 14, symbol = "circle"),
          name = "Entry", showlegend = TRUE
        )
      }

      if (!is.null(xp)) {
        xrow <- ref_df[which.min(abs(ref_df$.pos - xp)), ]
        p <- p |> add_trace(
          data = xrow, x = ~.x, y = ~.z,
          type = "scatter", mode = "markers",
          marker = list(color = "red", size = 14, symbol = "circle"),
          name = "Exit", showlegend = TRUE
        )
      }

      p |> layout(
        xaxis = list(title = "", scaleanchor = "y", scaleratio = 1),
        yaxis = list(title = ""),
        legend = list(orientation = "h", x = 0, y = -0.08)
      )
    })

    observeEvent(event_data("plotly_click", source = "corner_map"), ignoreNULL = TRUE, {
      ed <- event_data("plotly_click", source = "corner_map")
      if (is.null(ed) || nrow(ed) == 0) return()
      if (state() == 2L) return()

      cx <- ed$x[1]; cy <- ed$y[1]
      d2 <- (ref_df$.x - cx)^2 + (ref_df$.z - cy)^2
      j  <- which.min(d2)
      snapped <- ref_df$.pos[j]

      if (state() == 0L) entry_pos(snapped)
      else exit_pos(snapped)
    })

    observeEvent(input$reset, {
      entry_pos(NULL)
      exit_pos(NULL)
    })

    observeEvent(input$done, {
      if (state() < 2L) {
        showNotification("Click both entry and exit points first.", type = "warning")
        return()
      }
      stopApp(list(entry_pos = entry_pos(), exit_pos = exit_pos()))
    })
  }

  shinyApp(ui, server) |> runApp(launch.browser = TRUE)
}

# ---- Window 3: Analysis ----

.ca_analysis_app <- function(telemetry, entry_pos, exit_pos) {
  suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
    library(dplyr)
    library(ggplot2)
    library(tidyr)
    library(purrr)
    library(scales)
    library(gt)
    library(tibble)
    library(signal)
  })
  filter <- dplyr::filter

  BF <- "Times New Roman"
  BS <- 18

  laps_all <- sort(unique(telemetry$lapCount))
  laps_all <- laps_all[laps_all > 0]
  if (".synthetic" %in% names(telemetry)) {
    synth_laps <- unique(telemetry$lapCount[telemetry$.synthetic == TRUE])
    laps_all   <- setdiff(laps_all, synth_laps)
  }

  g_axis_lim <- if (.ca_has_cols(telemetry, c("accGHorizontal", "accGFrontal"))) {
    raw_max <- max(abs(c(telemetry$accGHorizontal, telemetry$accGFrontal)), na.rm = TRUE)
    ceiling(raw_max * 10) / 10
  } else {
    NULL
  }

  max_brake_g <- if (.ca_has_cols(telemetry, "accGFrontal")) {
    braking <- telemetry$accGFrontal[is.finite(telemetry$accGFrontal) & telemetry$accGFrontal < 0]
    if (length(braking) > 0) max(abs(braking)) else NA_real_
  } else {
    NA_real_
  }
  recommended_grip <- if (is.finite(max_brake_g)) round(max_brake_g * 0.9, 2) else NA_real_

  # ---- data ----

  get_corner_data <- function(pre_sec, post_sec, selected_laps) {
    purrr::map_dfr(selected_laps, function(lid) {
      lap_df <- telemetry |>
        filter(lapCount == lid) |>
        filter(is.finite(lapTime)) |>
        arrange(carPositionNormalized)

      if (nrow(lap_df) < 2) return(tibble())

      entry_idx <- which.min(abs(lap_df$carPositionNormalized - entry_pos))
      exit_idx  <- which.min(abs(lap_df$carPositionNormalized - exit_pos))
      entry_t   <- lap_df$lapTime[entry_idx]
      exit_t    <- lap_df$lapTime[exit_idx]

      if (!is.finite(entry_t) || !is.finite(exit_t) || entry_t >= exit_t) {
        filtered <- lap_df |>
          filter(carPositionNormalized >= min(entry_pos, exit_pos),
                 carPositionNormalized <= max(entry_pos, exit_pos))
      } else {
        filtered <- lap_df |>
          filter(lapTime >= entry_t - pre_sec * 1000,
                 lapTime <= exit_t  + post_sec * 1000)
      }

      filtered |> mutate(lap_label = paste0("Lap ", lid))
    })
  }

  # ---- shared helpers ----

  make_error_plot <- function(msg) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = msg, size = 5, hjust = 0.5) +
      theme_void(base_size = BS)
  }

  safe_plot <- function(p, show_hover = FALSE) {
    tryCatch({
      tooltip <- if (show_hover) "all" else "none"
      plotly::toWebGL(ggplotly(p, tooltip = tooltip))
    }, error = function(e) {
      plotly::plot_ly() |> plotly::layout(
        annotations = list(list(
          text = paste("Plot error:", e$message),
          x = 0.5, y = 0.5, showarrow = FALSE
        ))
      )
    })
  }

  # ---- traction circle ----

  plot_traction_circle <- function(df, grip_limit, axis_lim = NULL) {
    if (!.ca_has_cols(df, c("accGHorizontal", "accGFrontal"))) {
      return(make_error_plot("accGHorizontal or accGFrontal not found in data"))
    }

    theta     <- seq(0, 2 * pi, length.out = 300)
    radius    <- grip_limit

    p <- ggplot(df, aes(x = accGHorizontal, y = accGFrontal, color = lap_label))

    if (is.numeric(radius) && is.finite(radius) && radius > 0) {
      circle_df <- tibble(x = radius * cos(theta), y = radius * sin(theta))
      p <- p + geom_path(data = circle_df, aes(x = x, y = y), inherit.aes = FALSE,
                         color = "grey50", linetype = "dashed", linewidth = 0.7)
    }

    p <- p +
      geom_point(alpha = 0.5, size = 0.9) +
      { if (!is.null(axis_lim) && is.finite(axis_lim))
          coord_fixed(xlim = c(-axis_lim, axis_lim), ylim = c(-axis_lim, axis_lim))
        else
          coord_fixed() } +
      labs(
        title   = "Traction Circle",
        x       = "Lateral G",
        y       = "Longitudinal G",
        color   = NULL,
        caption = if (is.numeric(radius) && is.finite(radius) && radius > 0)
                    sprintf("Dashed circle = %.2f G (user defined)", radius)
                  else
                    NULL
      ) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(legend.position = "right")
  }

  # ---- speed trace ----

  plot_speed_trace <- function(df) {
    if (!.ca_has_cols(df, c("carPositionNormalized", "speedKmh"))) {
      return(make_error_plot("speedKmh not found in data"))
    }

    ggplot(df, aes(x = carPositionNormalized, y = speedKmh, color = lap_label)) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_vline(xintercept = entry_pos, linetype = "dashed", color = "green4", linewidth = 0.7) +
      geom_vline(xintercept = exit_pos,  linetype = "dashed", color = "red3",   linewidth = 0.7) +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title = "Speed Through Corner",
        x     = "Normalized lap position",
        y     = "Speed (km/h)",
        color = NULL
      ) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  }

  # ---- throttle & brake ----

  plot_throttle_brake <- function(df, show_hover = FALSE) {
    has_gas   <- "gas"   %in% names(df)
    has_brake <- "brake" %in% names(df)

    if (!has_gas && !has_brake) {
      return(safe_plot(make_error_plot("Neither gas nor brake columns found in data"), show_hover))
    }

    tooltip <- if (show_hover) "all" else "none"

    make_panel_gg <- function(col, panel_title) {
      ggplot(df, aes(x = carPositionNormalized, y = .data[[col]], color = lap_label)) +
        geom_line(linewidth = 0.8, alpha = 0.85) +
        geom_vline(xintercept = entry_pos, linetype = "dashed", color = "green4", linewidth = 0.7) +
        geom_vline(xintercept = exit_pos,  linetype = "dashed", color = "red3",   linewidth = 0.7) +
        scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(x = "Normalized lap position", y = panel_title, color = NULL) +
        theme_classic(base_family = BF, base_size = BS) +
        theme(legend.position = "none")
    }

    panels <- list()
    if (has_brake) panels[["Brake"]]    <- ggplotly(make_panel_gg("brake", "Brake"),    tooltip = tooltip)
    if (has_gas)   panels[["Throttle"]] <- ggplotly(make_panel_gg("gas",   "Throttle"), tooltip = tooltip)

    lap_labels <- unique(df$lap_label)
    for (i in seq_along(panels)) {
      for (j in seq_along(panels[[i]]$x$data)) {
        nm  <- panels[[i]]$x$data[[j]]$name
        panels[[i]]$x$data[[j]]$legendgroup <- nm
        panels[[i]]$x$data[[j]]$showlegend  <- (i == 1L && nm %in% lap_labels)
      }
    }

    plotly::subplot(panels, nrows = length(panels), shareX = TRUE,
                    titleY = TRUE, margin = 0.10) |>
      plotly::layout(
        showlegend = TRUE,
        title  = list(text = "Throttle & Brake Through Corner", x = 0),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.22),
        margin = list(t = 60, b = 120)
      ) |>
      plotly::toWebGL()
  }

  # ---- summary table ----

  make_summary_table <- function(df) {
    if (!.ca_has_cols(df, c("speedKmh", "lapTime", "carPositionNormalized"))) return(NULL)

    df |>
      group_by(lap_label) |>
      summarise(
        corner_time_s   = (.ca_interp(carPositionNormalized, lapTime, exit_pos) -
                             .ca_interp(carPositionNormalized, lapTime, entry_pos)) / 1000,
        entry_speed_kmh = .ca_interp(carPositionNormalized, speedKmh, entry_pos),
        min_speed_kmh   = min(speedKmh[carPositionNormalized >= min(entry_pos, exit_pos) &
                                        carPositionNormalized <= max(entry_pos, exit_pos)],
                              na.rm = TRUE),
        avg_speed_kmh   = {
                           in_s <- carPositionNormalized >= min(entry_pos, exit_pos) &
                                   carPositionNormalized <= max(entry_pos, exit_pos) &
                                   is.finite(speedKmh) & is.finite(lapTime)
                           t_s <- lapTime[in_s]; v_s <- speedKmh[in_s]
                           ord <- order(t_s); t_s <- t_s[ord]; v_s <- v_s[ord]
                           if (length(t_s) < 2) NA_real_
                           else sum(diff(t_s) * (head(v_s, -1) + tail(v_s, -1)) / 2) / sum(diff(t_s))
                         },
        exit_speed_kmh  = .ca_interp(carPositionNormalized, speedKmh, exit_pos),
        max_lat_g       = if (.ca_has_cols(df, "accGHorizontal"))
                            max(abs(accGHorizontal[carPositionNormalized >= min(entry_pos, exit_pos) &
                                                    carPositionNormalized <= max(entry_pos, exit_pos)]),
                                na.rm = TRUE)
                          else NA_real_,
        max_brake_g     = if (.ca_has_cols(df, "accGFrontal")) {
                            vals <- accGFrontal[carPositionNormalized >= min(entry_pos, exit_pos) &
                                                carPositionNormalized <= max(entry_pos, exit_pos) &
                                                accGFrontal < 0]
                            if (length(vals) == 0) NA_real_ else max(abs(vals), na.rm = TRUE)
                          } else NA_real_,
        max_accel_g     = if (.ca_has_cols(df, "accGFrontal")) {
                            vals <- accGFrontal[carPositionNormalized >= min(entry_pos, exit_pos) &
                                                carPositionNormalized <= max(entry_pos, exit_pos) &
                                                accGFrontal > 0]
                            if (length(vals) == 0) NA_real_ else max(vals, na.rm = TRUE)
                          } else NA_real_,
        .groups = "drop"
      ) |>
      arrange(corner_time_s)
  }

  # ---- speed delta ----

  plot_speed_delta <- function(df, ref_lap) {
    if (is.null(ref_lap)) {
      return(make_error_plot("Choose a reference lap in the sidebar to see the delta."))
    }
    if (!.ca_has_cols(df, c("speedKmh", "carPositionNormalized"))) {
      return(make_error_plot("speedKmh not found in data"))
    }

    ref_label <- paste0("Lap ", ref_lap)
    ref_df    <- df |> filter(lap_label == ref_label)
    if (nrow(ref_df) == 0) {
      return(make_error_plot(paste("Lap", ref_lap, "is not in the selected laps")))
    }

    grid <- seq(
      min(df$carPositionNormalized, na.rm = TRUE),
      max(df$carPositionNormalized, na.rm = TRUE),
      length.out = 500
    )
    ref_speed <- .ca_interp(ref_df$carPositionNormalized, ref_df$speedKmh, grid)

    other_labels <- setdiff(unique(df$lap_label), ref_label)
    if (length(other_labels) == 0) {
      return(make_error_plot("No other laps to compare against the reference lap"))
    }

    delta_df <- purrr::map_dfr(other_labels, function(lbl) {
      other_df    <- df |> filter(lap_label == lbl)
      other_speed <- .ca_interp(other_df$carPositionNormalized, other_df$speedKmh, grid)
      tibble(
        carPositionNormalized = grid,
        delta_kmh             = other_speed - ref_speed,
        lap_label             = lbl
      )
    })

    ggplot(delta_df, aes(x = carPositionNormalized, y = delta_kmh, color = lap_label)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_vline(xintercept = entry_pos, linetype = "dashed", color = "green4", linewidth = 0.7) +
      geom_vline(xintercept = exit_pos,  linetype = "dashed", color = "red3",   linewidth = 0.7) +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title    = paste("Speed Delta vs Lap", ref_lap),
        subtitle = "Positive = faster than reference",
        x        = "Normalized lap position",
        y        = "Speed delta (km/h)",
        color    = NULL
      ) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  }

  # ---- time delta ----

  plot_time_delta <- function(df, ref_lap) {
    if (is.null(ref_lap)) {
      return(make_error_plot("Choose a reference lap in the sidebar to see the delta."))
    }
    if (!.ca_has_cols(df, c("lapTime", "carPositionNormalized"))) {
      return(make_error_plot("lapTime not found in data"))
    }

    ref_label <- paste0("Lap ", ref_lap)
    ref_df    <- df |> filter(lap_label == ref_label)
    if (nrow(ref_df) == 0) {
      return(make_error_plot(paste("Lap", ref_lap, "is not in the selected laps")))
    }

    grid <- seq(
      min(df$carPositionNormalized, na.rm = TRUE),
      max(df$carPositionNormalized, na.rm = TRUE),
      length.out = 500
    )
    ref_time <- .ca_interp(ref_df$carPositionNormalized, ref_df$lapTime, grid)

    other_labels <- setdiff(unique(df$lap_label), ref_label)
    if (length(other_labels) == 0) {
      return(make_error_plot("No other laps to compare against the reference lap"))
    }

    delta_df <- purrr::map_dfr(other_labels, function(lbl) {
      other_df   <- df |> filter(lap_label == lbl)
      other_time <- .ca_interp(other_df$carPositionNormalized, other_df$lapTime, grid)
      raw_delta  <- (ref_time - other_time) / 1000
      tibble(
        carPositionNormalized = grid,
        delta_s               = raw_delta - raw_delta[1],
        lap_label             = lbl
      )
    })

    ggplot(delta_df, aes(x = carPositionNormalized, y = delta_s, color = lap_label)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_vline(xintercept = entry_pos, linetype = "dashed", color = "green4", linewidth = 0.7) +
      geom_vline(xintercept = exit_pos,  linetype = "dashed", color = "red3",   linewidth = 0.7) +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title    = paste("Cumulative Time Delta vs Lap", ref_lap),
        subtitle = "Positive = faster than reference",
        x        = "Normalized lap position",
        y        = "Time delta (s)",
        color    = NULL
      ) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  }

  # ---- consistency ----

  consistency_long <- function(df) {
    tbl <- make_summary_table(df)
    if (is.null(tbl) || nrow(tbl) < 2) return(NULL)
    tbl |>
      mutate(lap_num = as.integer(gsub("Lap ", "", lap_label))) |>
      arrange(lap_num) |>
      select(lap_label, lap_num, corner_time_s, entry_speed_kmh, min_speed_kmh, exit_speed_kmh,
             any_of(c("max_lat_g", "max_brake_g", "max_accel_g"))) |>
      pivot_longer(
        cols      = c(corner_time_s, entry_speed_kmh, min_speed_kmh, exit_speed_kmh,
                      any_of(c("max_lat_g", "max_brake_g", "max_accel_g"))),
        names_to  = "metric",
        values_to = "value"
      ) |>
      mutate(metric = recode(metric,
        corner_time_s   = "Corner Time (s)",
        entry_speed_kmh = "Entry Speed (km/h)",
        min_speed_kmh   = "Min Speed (km/h)",
        exit_speed_kmh  = "Exit Speed (km/h)",
        max_lat_g       = "Max Lateral G",
        max_brake_g     = "Max Braking G",
        max_accel_g     = "Max Accel. G"
      ))
  }

  make_consistency_table <- function(df) {
    long <- consistency_long(df)
    if (is.null(long)) return(NULL)
    long |>
      group_by(metric) |>
      summarise(
        Mean  = mean(value, na.rm = TRUE),
        SD    = sd(value,   na.rm = TRUE),
        `CV%` = SD / Mean * 100,
        Min   = min(value,  na.rm = TRUE),
        Max   = max(value,  na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(cols = c(Mean, SD, `CV%`, Min, Max), names_to = "Stat", values_to = "value") |>
      pivot_wider(names_from = metric, values_from = value) |>
      mutate(Stat = factor(Stat, levels = c("Mean", "SD", "CV%", "Min", "Max"))) |>
      arrange(Stat)
  }

  plot_consistency_trend <- function(df) {
    long <- consistency_long(df)
    if (is.null(long)) return(make_error_plot("Need at least 2 laps for consistency analysis"))
    mean_df <- long |>
      group_by(metric) |>
      summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
    ggplot(long, aes(x = factor(lap_num), y = value, group = 1)) +
      geom_hline(data = mean_df, aes(yintercept = mean_val),
                 linetype = "dashed", color = "grey50", linewidth = 0.7) +
      geom_line(color = "steelblue", linewidth = 0.7, alpha = 0.7) +
      geom_point(color = "steelblue", size = 3) +
      facet_wrap(~metric, scales = "free_y", ncol = 2) +
      labs(
        title    = "Corner Metrics by Lap",
        subtitle = "Dashed line = mean",
        x        = "Lap",
        y        = NULL
      ) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(
        panel.spacing = unit(2, "lines"),
        strip.text    = element_text(size = 11, margin = margin(t = 4, b = 6)),
        plot.title    = element_text(margin = margin(b = 20))
      )
  }

  # ---- corner map ----

  plot_corner_map <- function(df, color_var = "speedKmh") {
    needed <- c("carCoordinatesX", "carCoordinatesZ", "carPositionNormalized", color_var)
    if (!.ca_has_cols(df, needed)) {
      return(make_error_plot(paste("Column not found in data:", color_var)))
    }

    pos_min <- df |> group_by(lap_label) |>
      summarise(mn = min(carPositionNormalized), .groups = "drop") |> pull(mn) |> max()
    pos_max <- df |> group_by(lap_label) |>
      summarise(mx = max(carPositionNormalized), .groups = "drop") |> pull(mx) |> min()
    df <- df |> filter(carPositionNormalized >= pos_min, carPositionNormalized <= pos_max)

    label_map    <- c(speedKmh = "Speed (km/h)", accGHorizontal = "Lateral G",
                      accGFrontal = "Longitudinal G", gas = "Throttle", brake = "Brake")
    legend_label <- label_map[[color_var]]
    palette_hint <- if (color_var == "gas") "(grey = low, green = high)"
                    else if (color_var == "accGFrontal") "(red = braking, white = 0, green = acceleration)"
                    else if (color_var == "brake") "(grey = low, red = high)"
                    else if (color_var == "speedKmh") "(red = low, green = high)"
                    else "(blue = left, white = 0, orange = right)"
    plot_title   <- paste0("Corner Map — ", legend_label, " ", palette_hint)

    color_scale  <- if (color_var == "gas") {
      scale_color_gradient(low = "grey95", high = "limegreen", name = legend_label)
    } else if (color_var == "accGFrontal") {
      scale_color_gradient2(low = "red3", mid = "white", high = "limegreen",
                            midpoint = 0, name = legend_label)
    } else if (color_var == "brake") {
      scale_color_gradient(low = "grey85", high = "red3", name = legend_label)
    } else if (color_var == "speedKmh") {
      scale_color_gradientn(colours = c("red3", "yellow", "limegreen"), name = legend_label)
    } else {
      scale_color_gradient2(low = "royalblue", mid = "white", high = "darkorange2",
                            midpoint = 0, name = legend_label)
    }

    ggplot(df, aes(x = carCoordinatesX, y = carCoordinatesZ, color = .data[[color_var]])) +
      geom_point(size = 1.5, alpha = 0.9) +
      facet_wrap(~lap_label) +
      color_scale +
      coord_fixed() +
      labs(title = plot_title) +
      theme_classic(base_family = BF, base_size = BS) +
      theme(
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.title        = element_blank(),
        legend.position   = "right",
        panel.spacing     = unit(2, "lines"),
        plot.title        = element_text(margin = margin(b = 20)),
        strip.text        = element_text(size = 11, margin = margin(t = 4, b = 6))
      )
  }

  # ---- UI ----

  ui <- fluidPage(
    tags$head(tags$style(HTML("body { font-size: 16px; }"))),
    titlePanel("Corner Analysis — Step 3: Analysis"),
    sidebarLayout(
      sidebarPanel(
        tags$b(sprintf("Corner: pos %.4f → %.4f", entry_pos, exit_pos)),
        tags$hr(),
        sliderInput("pre_buf",  "Seconds before entry",
                    min = 0, max = 10, value = 1, step = 0.5),
        sliderInput("post_buf", "Seconds after exit",
                    min = 0, max = 10, value = 1, step = 0.5),
        tags$hr(),
        tags$label("Select laps"),
        fluidRow(
          column(6, actionButton("select_all",   "Select all",   class = "btn-sm btn-default", width = "100%")),
          column(6, actionButton("deselect_all", "Deselect all", class = "btn-sm btn-default", width = "100%"))
        ),
        tags$br(),
        checkboxGroupInput("selected_laps", label = NULL,
                           choices  = laps_all,
                           selected = laps_all),
        tags$hr(),
        uiOutput("ref_ui"),
        tags$hr(),
        checkboxInput("show_hover", "Show hover data", value = FALSE),
        tags$hr(),
        selectInput("map_color", "Corner map measure",
                    choices  = c("Speed (km/h)"   = "speedKmh",
                                 "Lateral G"       = "accGHorizontal",
                                 "Longitudinal G"  = "accGFrontal",
                                 "Throttle"        = "gas",
                                 "Brake"           = "brake"),
                    selected = "speedKmh"),
        tags$hr(),
        numericInput("grip_limit", "Grip limit (G) — leave blank to hide circle",
                     value = recommended_grip, min = 0.1, step = 0.1),
        if (is.finite(recommended_grip))
          tags$small(style = "color: #666;",
            sprintf("Suggested: %.2f G  (90%% of max braking %.2f G across all laps)",
                    recommended_grip, max_brake_g)),
        tags$hr(),
        sliderInput("lateral_g_cutoff",
                    "Lateral G low-pass cutoff (Hz)  [0 = no filter]",
                    min = 0, max = 20, value = 0, step = 0.5),
        helpText("2–5 Hz captures driver steering inputs. Above 5 Hz likely reflects",
                 "suspension or road surface noise. Set to 0 for the raw signal.",
                 "Affects: Traction Circle, Corner Map (Lateral G), max_lat_g in summary tables.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Traction Circle",  plotlyOutput("plot_tc",    height = "560px")),
          tabPanel("Speed Trace",      plotlyOutput("plot_speed", height = "460px")),
          tabPanel("Throttle & Brake", plotlyOutput("plot_tb",    height = "560px")),
          tabPanel("Corner Summary",
            downloadButton("dl_summary", "Export CSV", class = "btn-sm btn-default",
                           style = "margin: 8px 0;"),
            gt_output("table_summary"),
            tags$hr(),
            uiOutput("consist_trend_ui")
          ),
          tabPanel("Speed Delta",      plotlyOutput("plot_delta",  height = "460px")),
          tabPanel("Time Delta",       plotlyOutput("plot_tdelta", height = "460px")),
          tabPanel("Corner Map",       plotlyOutput("plot_map",    height = "560px")),
          tabPanel("Consistency",
            downloadButton("dl_consistency", "Export CSV", class = "btn-sm btn-default",
                           style = "margin: 8px 0;"),
            gt_output("table_consistency")
          )
        )
      )
    )
  )

  # ---- Server ----

  server <- function(input, output, session) {

    observeEvent(input$select_all, {
      updateCheckboxGroupInput(session, "selected_laps", selected = laps_all)
    })

    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(session, "selected_laps", selected = character(0))
    })

    output$ref_ui <- renderUI({
      req(input$selected_laps)
      laps <- as.integer(input$selected_laps)
      choices <- c("None" = "0", setNames(as.character(laps), paste0("Lap ", laps)))
      current  <- isolate(input$ref_lap)
      selected <- if (!is.null(current) && current %in% as.character(laps)) current else "0"
      selectInput("ref_lap", "Reference lap", choices = choices, selected = selected)
    })

    corner_data_rv <- reactive({
      req(input$selected_laps)
      laps <- as.integer(input$selected_laps)
      if (length(laps) == 0) return(NULL)

      df <- get_corner_data(
        pre_sec       = input$pre_buf,
        post_sec      = input$post_buf,
        selected_laps = laps
      )

      cutoff <- input$lateral_g_cutoff
      if (!is.null(cutoff) && cutoff > 0 && .ca_has_cols(df, c("accGHorizontal", "lapTime"))) {
        df <- df |>
          group_by(lap_label) |>
          group_modify(function(gdf, key) {
            dt_ms <- median(diff(gdf$lapTime), na.rm = TRUE)
            fs    <- if (!is.na(dt_ms) && dt_ms > 0) 1000 / dt_ms else NA_real_
            gdf |> mutate(accGHorizontal = .ca_lowpass(accGHorizontal, cutoff, fs))
          }) |>
          ungroup()
      }

      df
    })

    ref_lap_rv <- reactive({
      req(input$ref_lap)
      v <- as.integer(input$ref_lap)
      if (v == 0L) NULL else v
    })

    output$plot_tc <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_traction_circle(df, input$grip_limit, axis_lim = g_axis_lim), input$show_hover)
    })

    output$plot_speed <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_speed_trace(df), input$show_hover) |>
        plotly::layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.35),
          margin = list(b = 120)
        )
    })

    output$plot_tb <- renderPlotly({
      df <- corner_data_rv(); req(df)
      plot_throttle_brake(df, show_hover = input$show_hover)
    })

    output$table_summary <- render_gt({
      df <- corner_data_rv(); req(df)
      tbl_df <- make_summary_table(df)
      req(tbl_df)

      ref_label <- if (!is.null(ref_lap_rv())) paste0("Lap ", ref_lap_rv()) else NULL

      gt_tbl <- tbl_df |>
        gt() |>
        cols_label(
          lap_label       = "Lap",
          corner_time_s   = "Corner Time (s)",
          entry_speed_kmh = "Entry Speed (km/h)",
          min_speed_kmh   = "Min Speed (km/h)",
          avg_speed_kmh   = "Avg Speed (km/h)",
          exit_speed_kmh  = "Exit Speed (km/h)",
          max_lat_g       = "Max Lateral G",
          max_brake_g     = "Max Braking G",
          max_accel_g     = "Max Accel. G"
        ) |>
        fmt_number(columns = where(is.numeric), decimals = 2) |>
        tab_options(
          table.font.names = "Times New Roman",
          table.font.size  = px(16)
        )

      if (!is.null(ref_label) && ref_label %in% tbl_df$lap_label) {
        gt_tbl <- gt_tbl |>
          tab_style(
            style     = cell_fill(color = "#E8F5E9"),
            locations = cells_body(rows = lap_label == ref_label)
          )
      }

      gt_tbl
    })

    output$plot_delta <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_speed_delta(df, ref_lap_rv()), input$show_hover) |>
        plotly::layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.35),
          margin = list(b = 120)
        )
    })

    output$plot_tdelta <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_time_delta(df, ref_lap_rv()), input$show_hover) |>
        plotly::layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.35),
          margin = list(b = 120)
        )
    })

    output$plot_map <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_corner_map(df, color_var = input$map_color), input$show_hover)
    })

    output$table_consistency <- render_gt({
      df <- corner_data_rv(); req(df)
      tbl <- make_consistency_table(df)
      req(tbl)
      tbl |>
        gt() |>
        cols_label(Stat = "") |>
        fmt_number(columns = where(is.numeric), decimals = 2) |>
        tab_options(
          table.font.names = "Times New Roman",
          table.font.size  = px(16)
        )
    })

    output$dl_summary <- downloadHandler(
      filename = function() paste0("corner_summary_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- corner_data_rv()
        req(df)
        write.csv(make_summary_table(df), file, row.names = FALSE)
      }
    )

    output$dl_consistency <- downloadHandler(
      filename = function() paste0("corner_consistency_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- corner_data_rv()
        req(df)
        write.csv(make_consistency_table(df), file, row.names = FALSE)
      }
    )

    output$consist_trend_ui <- renderUI({
      df <- corner_data_rv()
      long <- if (!is.null(df)) consistency_long(df) else NULL
      n_metrics <- if (!is.null(long)) length(unique(long$metric)) else 4L
      height_px <- ceiling(n_metrics / 2) * 220
      plotlyOutput("plot_consist_trend", height = paste0(height_px, "px"))
    })

    output$plot_consist_trend <- renderPlotly({
      df <- corner_data_rv(); req(df)
      safe_plot(plot_consistency_trend(df), input$show_hover)
    })
  }

  shinyApp(ui, server) |> runApp(launch.browser = TRUE)
}

# ---- Entry point ----

corner_analysis <- function() {
  suppressPackageStartupMessages(library(dplyr))

  sel <- .ca_upload_gadget()

  corner <- .ca_corner_select_gadget(
    telemetry = sel$telemetry,
    lap_id    = sel$lap_id
  )

  .ca_analysis_app(
    telemetry = sel$telemetry,
    entry_pos = corner$entry_pos,
    exit_pos  = corner$exit_pos
  )

  invisible(NULL)
}

## Run
corner_analysis()
