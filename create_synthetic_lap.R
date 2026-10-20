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

# ----------------------------
# Position-time helpers (interpolated boundary crossings)
# ----------------------------

# Order one lap's samples by time and unwrap normalized position across the
# start/finish line, so position rises from ~0 to ~1 over the lap.
# Returns row indices (time order, first sample at each position) and the
# unwrapped positions.
.lap_unwrap <- function(pos, t) {
  idx <- which(is.finite(pos) & is.finite(t))
  idx <- idx[order(t[idx])]
  p <- pos[idx]
  jump <- c(0, diff(p))
  u <- p + cumsum(jump < -0.5) - cumsum(jump > 0.5)
  if (length(u) > 0 && u[1] > 0.5) u <- u - 1  # lap starts just before the line
  u <- cummax(u)
  keep <- !duplicated(u)
  list(idx = idx[keep], u = u[keep])
}

# Linear interpolation of y at xout, with linear extrapolation beyond the
# first/last sample of up to max_extrap (lap fraction); NA further out
.interp_extrap <- function(x, y, xout, max_extrap = 0.02) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, length(xout)))
  out <- stats::approx(x, y, xout = xout, rule = 2)$y
  lo <- xout < x[1]
  hi <- xout > x[n]
  out[lo] <- y[1] + (xout[lo] - x[1]) * (y[2] - y[1]) / (x[2] - x[1])
  out[hi] <- y[n] + (xout[hi] - x[n]) * (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  out[(lo & x[1] - xout > max_extrap) | (hi & xout - x[n] > max_extrap)] <- NA_real_
  out
}

# Time at which a lap passes unwrapped position(s) u (0 = start/finish line at
# the start of the lap, 1 = at its end), interpolated between the samples
# either side of each crossing
lap_time_at <- function(pos, t, u) {
  un <- .lap_unwrap(pos, t)
  .interp_extrap(un$u, t[un$idx], u)
}

# Sector duration from interpolated boundary-crossing times.
# A sector with start > end wraps across the start/finish line.
seg_duration <- function(pos, t, start, end) {
  if (start < end) {
    tt <- lap_time_at(pos, t, c(start, end))
    tt[2] - tt[1]
  } else {
    tt <- lap_time_at(pos, t, c(start, 1, 0, end))
    (tt[2] - tt[1]) + (tt[4] - tt[3])
  }
}

build_synthetic_lap_from_bounds <- function(
    telemetry,
    bounds,
    lap_col = "lapCount",
    pos_col = "carPositionNormalized",
    time_col = "lapTime",
    include_boundary = "left",
    min_samples_per_seg = 10
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })
  
  stopifnot(include_boundary %in% c("left", "right"))
  include_left <- include_boundary == "left"
  
  if (!all(c("seg", "start", "end") %in% names(bounds))) {
    stop("bounds must contain columns: seg, start, end")
  }
  
  in_interval <- function(pos, start, end, include_left = TRUE) {
    if (start < end) {
      if (include_left) pos >= start & pos < end else pos > start & pos <= end
    } else {
      if (include_left) (pos >= start & pos < 1) | (pos >= 0 & pos < end)
      else (pos > start & pos <= 1) | (pos >= 0 & pos <= end)
    }
  }
  
  per_lap <- telemetry %>%
    filter(.data[[lap_col]] > 0) %>%
    arrange(.data[[lap_col]], .data[[pos_col]], .data[[time_col]])

  # Sector time = interpolated time at the end boundary minus interpolated
  # time at the start boundary (not a sum of whole sample intervals)
  seg_times <- purrr::map_dfr(seq_len(nrow(bounds)), function(i) {
    b <- bounds[i, ]
    per_lap %>%
      group_by(.data[[lap_col]]) %>%
      summarise(
        n_seg = sum(in_interval(.data[[pos_col]], b$start, b$end, include_left), na.rm = TRUE),
        seg_time = seg_duration(.data[[pos_col]], .data[[time_col]], b$start, b$end),
        .groups = "drop"
      ) %>%
      mutate(seg = b$seg, start = b$start, end = b$end)
  })

  winners <- seg_times %>%
    filter(n_seg >= min_samples_per_seg, is.finite(seg_time)) %>%
    group_by(seg) %>%
    slice_min(seg_time, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(seg)
  
  missing_segs <- setdiff(bounds$seg, winners$seg)
  if (length(missing_segs) > 0) {
    fallback <- seg_times %>%
      filter(seg %in% missing_segs, is.finite(seg_time)) %>%
      group_by(seg) %>%
      slice_max(n_seg, n = 1, with_ties = FALSE) %>%
      ungroup()
    winners <- bind_rows(winners, fallback) %>% arrange(seg)
  }
  
  stitched <- purrr::map_dfr(seq_len(nrow(bounds)), function(i) {
    b <- bounds[i, ]
    wlap <- winners %>% filter(seg == b$seg) %>% pull(.data[[lap_col]])
    per_lap %>%
      filter(.data[[lap_col]] == wlap) %>%
      arrange(.data[[pos_col]], .data[[time_col]]) %>%
      filter(in_interval(.data[[pos_col]], b$start, b$end, include_left)) %>%
      mutate(seg = b$seg, source_lap = wlap)
  }) %>%
    arrange(seg, .data[[pos_col]], .data[[time_col]])
  
  list(
    synthetic_lap = stitched,
    segments = winners %>% select(seg, start, end, source_lap = !!rlang::sym(lap_col), seg_time)
  )
}

finalize_synthetic_lap <- function(
    stitched,
    telemetry_cols = NULL,
    lap_col = "lapCount",
    time_col = "lapTime",
    pos_col = "carPositionNormalized",
    new_lap = NULL,
    time_absolute_col = "time",
    time_offset = NA_real_,
    keep_provenance = TRUE
) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  if (is.null(telemetry_cols)) telemetry_cols <- names(stitched)
  
  if (!pos_col %in% names(stitched)) stop("stitched is missing ", pos_col)
  if (!time_col %in% names(stitched)) stop("stitched is missing ", time_col)
  
  if (is.null(new_lap)) new_lap <- 0L
  
  s <- stitched %>%
    arrange(.data[[pos_col]]) %>%
    mutate(
      !!lap_col := as.integer(new_lap)
    )
  
  if (time_absolute_col %in% names(s)) {
    if (is.finite(time_offset)) {
      s[[time_absolute_col]] <- time_offset + s[[time_col]]
    } else {
      s[[time_absolute_col]] <- NA_real_
    }
  }
  
  base_keep <- intersect(telemetry_cols, names(s))
  
  if (keep_provenance) {
    prov <- intersect(c("seg", "source_lap", "start", "end", "dt_seg"), names(s))
    keep <- unique(c(base_keep, prov))
  } else {
    keep <- base_keep
  }
  
  s %>%
    select(all_of(keep)) %>%
    mutate(.synthetic = TRUE)
}

unify_from_speed_output <- function(
    speed_output,
    telemetry,
    new_lap = NULL,
    keep_provenance = TRUE,
    time_offset = NA_real_
) {
  if (is.null(speed_output$synthetic_lap)) stop("speed_output must contain $synthetic_lap")
  stitched <- speed_output$synthetic_lap
  
  if (is.null(new_lap)) {
    new_lap <- max(telemetry$lapCount, na.rm = TRUE) + 1L
  }
  
  synth <- finalize_synthetic_lap(
    stitched = stitched,
    telemetry_cols = names(telemetry),
    new_lap = new_lap,
    keep_provenance = keep_provenance,
    time_offset = time_offset
  )
  
  missing_in_synth <- setdiff(names(telemetry), names(synth))
  if (length(missing_in_synth) > 0) {
    synth[missing_in_synth] <- NA
  }
  
  extra_in_synth <- setdiff(names(synth), names(telemetry))
  telemetry2 <- telemetry
  if (length(extra_in_synth) > 0) {
    telemetry2[extra_in_synth] <- NA
  }
  
  telemetry_with_synth <- dplyr::bind_rows(
    telemetry2,
    synth %>% dplyr::select(names(telemetry2))
  )
  
  list(
    synthetic_lap = synth,
    telemetry_with_synth = telemetry_with_synth
  )
}

resample_and_retime_synthetic_lap <- function(
    telemetry,
    segments,
    n_bins,
    discrete_numeric_cols = c("gear", "isAbsInAction", "isTcInAction", "isInPit"),
    seg_col = "seg",
    start_col = "start",
    end_col = "end",
    lap_col = "lapCount",
    pos_col = "carPositionNormalized",
    time_col = "lapTime",
    keep_provenance = TRUE
) {
  # Builds the composite lap on a regular position grid. Each grid point keeps
  # the timing of the lap the sector was taken from: time = (source-lap time at
  # that position) - (source-lap time at the sector start), interpolated
  # between samples, then chained across sectors. Channels are interpolated from
  # the whole source lap, so values at the boundaries use the samples on both
  # sides of the line.
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })

  need_seg <- c(seg_col, start_col, end_col, "source_lap")
  miss_seg <- setdiff(need_seg, names(segments))
  if (length(miss_seg) > 0) stop("segments is missing: ", paste(miss_seg, collapse = ", "))
  need_tel <- c(lap_col, pos_col, time_col)
  miss_tel <- setdiff(need_tel, names(telemetry))
  if (length(miss_tel) > 0) stop("telemetry is missing: ", paste(miss_tel, collapse = ", "))

  seg_tbl <- segments %>%
    transmute(
      seg = .data[[seg_col]],
      start = .data[[start_col]],
      end = .data[[end_col]],
      source_lap = .data[["source_lap"]]
    ) %>%
    arrange(seg) %>%
    mutate(len = ifelse(start < end, end - start, (1 - start) + end))

  if (sum(seg_tbl$len, na.rm = TRUE) <= 0) stop("Bad segment boundaries. Total length is not positive.")

  alloc_n <- pmax(2L, as.integer(round(n_bins * seg_tbl$len / sum(seg_tbl$len))))
  delta <- n_bins - sum(alloc_n)
  if (delta != 0) {
    idx <- which.max(seg_tbl$len)
    alloc_n[idx] <- alloc_n[idx] + delta
    if (alloc_n[idx] < 2) alloc_n[idx] <- 2
  }
  seg_tbl$n <- alloc_n

  # Value of the sample nearest to each xout (x sorted ascending)
  nearest_pick <- function(x, y, xout) {
    i <- findInterval(xout, x, all.inside = TRUE)
    pick <- ifelse(abs(x[i + 1] - xout) < abs(xout - x[i]), i + 1, i)
    y[pick]
  }

  # Provenance columns from an earlier composite lap in the same file are not channels
  value_cols <- setdiff(
    names(telemetry),
    c(lap_col, pos_col, time_col, seg_col, "source_lap", "start", "end", "dt_seg", ".synthetic")
  )

  resampled <- purrr::map_dfr(seq_len(nrow(seg_tbl)), function(i) {
    b <- seg_tbl[i, ]
    wrap <- b$start >= b$end

    src <- telemetry %>% filter(.data[[lap_col]] == b$source_lap)
    un <- .lap_unwrap(src[[pos_col]], src[[time_col]])
    src <- src[un$idx, , drop = FALSE]
    u_src <- un$u
    t_at <- function(u) .interp_extrap(u_src, src[[time_col]], u)

    # Grid over [start, end); a sector across the start/finish line runs from
    # start to end + 1 and always gets a point exactly on the line (u = 1)
    end_u <- if (wrap) b$end + 1 else b$end
    grid_u <- seq(b$start, end_u, length.out = b$n + 1)[seq_len(b$n)]
    if (wrap) grid_u <- sort(c(grid_u[abs(grid_u - 1) > 1e-9], 1))
    src_u <- ifelse(grid_u >= 1, grid_u - 1, grid_u)  # position within the source lap

    if (wrap) {
      to_line <- t_at(1) - t_at(b$start)
      t_seg <- ifelse(grid_u < 1,
                      t_at(grid_u) - t_at(b$start),
                      to_line + t_at(src_u) - t_at(0))
      seg_dur <- to_line + t_at(b$end) - t_at(0)
    } else {
      t_seg <- t_at(grid_u) - t_at(b$start)
      seg_dur <- t_at(b$end) - t_at(b$start)
    }

    out <- tibble::tibble(
      seg = b$seg,
      !!pos_col := grid_u %% 1,
      .t_seg = t_seg,
      .seg_dur = seg_dur,
      .on_line = wrap & grid_u == 1,
      source_lap = b$source_lap,
      start = b$start,
      end = b$end
    )

    for (cn in value_cols) {
      y <- src[[cn]]
      if (cn %in% discrete_numeric_cols || !is.numeric(y)) {
        # Discrete signals (gear, ABS/TC flags) and text: nearest sample
        out[[cn]] <- nearest_pick(u_src, y, src_u)
      } else {
        ok <- is.finite(y)
        out[[cn]] <- if (sum(ok) >= 2) {
          stats::approx(u_src[ok], y[ok], xout = src_u, rule = 2)$y
        } else {
          NA_real_
        }
      }
    }

    out
  })

  if (any(!is.finite(resampled$.t_seg)) || any(!is.finite(resampled$.seg_dur))) {
    stop("Could not time the composite lap: a source lap does not cover its whole sector.")
  }

  # Chain sectors: each sector starts when the previous one ends
  offsets <- resampled %>%
    distinct(seg, .seg_dur) %>%
    arrange(seg) %>%
    mutate(.offset = lag(cumsum(.seg_dur), default = 0))
  total <- sum(offsets$.seg_dur)

  resampled <- resampled %>%
    left_join(offsets %>% select(seg, .offset), by = "seg") %>%
    mutate(.t = .offset + .t_seg)

  # Re-reference time to the start/finish line, like a recorded lap:
  # lapTime = 0 at position 0, plus a closing point at position 1 with the
  # full composite lap time
  t0 <- resampled$.t[resampled$.on_line][1]
  closing <- resampled %>%
    filter(.on_line) %>%
    slice(1) %>%
    mutate(!!pos_col := 1, !!time_col := total)
  resampled <- resampled %>%
    mutate(!!time_col := (.t - t0) %% total)
  # If a sector starts exactly on the line, keep its point and drop the duplicate
  if (sum(resampled[[pos_col]] == 0) > 1) resampled <- resampled %>% filter(!.on_line)

  out <- bind_rows(resampled, closing) %>%
    arrange(.data[[time_col]], .data[[pos_col]]) %>%
    select(-.t_seg, -.seg_dur, -.on_line, -.offset, -.t) %>%
    rename(!!seg_col := seg)

  if (!keep_provenance) {
    out <- out %>% select(-any_of(c("source_lap", "start", "end")))
  }

  out
}

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
