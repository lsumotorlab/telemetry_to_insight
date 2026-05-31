# telemetry_harmonize_and_preprocess_app.R
# =======================================

library(shiny)
library(tidyverse)
library(readr)
filter <- dplyr::filter
options(shiny.maxRequestSize = 500 * 1024^2)

# =============================================================================
# Helpers
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

parse_int_list <- function(x) {
  if (is.null(x)) return(integer(0))
  x <- trimws(x)
  if (!nzchar(x)) return(integer(0))
  parts <- unlist(strsplit(x, "[,\\s]+"))
  parts <- parts[nzchar(parts)]
  out <- suppressWarnings(as.integer(parts))
  out <- out[!is.na(out)]
  unique(out)
}

# =============================================================================
# Harmonizer
# =============================================================================
required_fields <- c(
  "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
  "lapCount", "lapTime", "carPositionNormalized",
  "speedKmh", "gear", "brake", "gas",
  "engineRPM", "steer",
  "isAbsInAction", "isTcInAction", "isInPit",
  "accGHorizontal", "accGFrontal"
)

field_labels <- c(
  carCoordinatesX        = "Car coordinates X - Lateral",
  carCoordinatesY        = "Car coordinates Y - Altitude",
  carCoordinatesZ        = "Car coordinates Z - Longitudinal",
  lapCount               = "Lap count",
  lapTime                = "Lap time",
  carPositionNormalized  = "Track position (normalized)",
  speedKmh               = "Speed",
  gear                   = "Gear",
  brake                  = "Brake",
  gas                    = "Gas / Throttle",
  engineRPM              = "Engine RPM",
  steer                  = "Steering",
  isAbsInAction          = "ABS active",
  isTcInAction           = "Traction control active",
  isInPit                = "In pit",
  accGHorizontal         = "Lateral acceleration",
  accGFrontal            = "Longitudinal acceleration"
)

guess_mapping <- function(names_in) {
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  names_norm <- norm(names_in)
  
  syn <- list(
    carCoordinatesX = c("^x$", "xpos", "carcoordinatesx", "coordx", "xcoordinate", "autocoordinatesx", "autocoordsx"),
    carCoordinatesY = c("^y$", "ypos", "carcoordinatesy", "coordy", "ycoordinate", "elev", "autocoordinatesy", "autocoordsy"),
    carCoordinatesZ = c("^z$", "zpos", "carcoordinatesz", "coordz", "zcoordinate", "long", "lon", "autocoordinatesz", "autocoordsz"),
    
    lapCount = c("lap", "lapcount", "lap_number", "lapno", "lapidx", "lapindex"),
    lapTime = c("laptime", "lap_time", "lapduration", "laptimesec", "laptimes", "timeperlap"),
    carPositionNormalized = c("carpositionnormalized", "positionnormalized", "posnorm", "trackpos", "trackposition", "splinepos", "splineposition"),
    
    speedKmh = c("speed", "speed_kmh", "speedkmh", "kmh", "kph", "speedkph", "speedms", "mps", "velocity"),
    gear = c("^gear$", "gearbox", "g", "currentgear"),
    
    brake = c("brake", "brakepressure", "brake_pct", "brakeperc", "braking", "brakeinput"),
    gas = c("gas", "throttle", "accel", "throttle_pct", "gas_pedal", "accelerator"),
    
    engineRPM = c("enginerpm", "^rpm$", "engine_speed", "enginespeed", "revs", "rev", "motor_rpm", "motorrpm"),
    steer = c("steer", "steering", "steerangle", "steeringangle", "wheelangle", "steeringwheel", "steerinput", "steeringinput"),
    
    isAbsInAction = c("abs", "isabs", "absinaction", "abs_active", "absenabled", "isabsenabled", "absactive"),
    isTcInAction = c("tc", "istc", "tcinaction", "tc_active", "tcenabled", "tractioncontrol", "tcactive"),
    
    isInPit = c("isinpit", "inpit", "pit", "pitlane", "inpitlane", "pitstatus", "pit_flag", "pitflag", "inpits"),

    accGHorizontal = c("accghorizontal", "lateralg", "lateral_g", "acclateral", "glateral", "acchorizontal", "lateralaccel"),
    accGFrontal    = c("accgfrontal", "longitudinalg", "longitudinal_g", "accfrontal", "gfrontal", "acclongitudinal", "longitudinalaccel")
  )
  
  pick_one <- function(patterns, target_norm) {
    idx_exact_target <- which(names_norm == target_norm)
    if (length(idx_exact_target)) return(names_in[idx_exact_target[1]])
    
    patterns_norm <- unique(norm(gsub("^\\^|\\$$", "", patterns)))
    idx_exact_syn <- which(names_norm %in% patterns_norm[nzchar(patterns_norm)])
    if (length(idx_exact_syn)) return(names_in[idx_exact_syn[1]])
    
    for (p in patterns) {
      idx <- which(grepl(p, names_norm, perl = TRUE))
      if (length(idx)) return(names_in[idx[1]])
    }
    
    idx_sub <- which(vapply(
      names_norm,
      function(n) nzchar(n) && (grepl(target_norm, n, fixed = TRUE) || grepl(n, target_norm, fixed = TRUE)),
      logical(1)
    ))
    if (length(idx_sub)) return(names_in[idx_sub[1]])
    
    d <- as.numeric(adist(target_norm, names_norm, partial = TRUE))
    best <- which.min(d)
    thresh <- max(2, ceiling(nchar(target_norm) * 0.25))
    if (length(best) && d[best] <= thresh) return(names_in[best])
    
    NA_character_
  }
  
  res <- setNames(rep(NA_character_, length(required_fields)), required_fields)
  for (t in required_fields) {
    if (t %in% names_in) {
      res[t] <- t
    } else {
      res[t] <- pick_one(syn[[t]], norm(t))
    }
  }
  res
}

harmonize_telemetry <- function(df, mapping, speed_unit = c("auto", "kmh", "ms"), lap_time_unit = c("auto", "milliseconds", "seconds"), acc_unit = c("auto", "g", "ms2")) {
  speed_unit    <- match.arg(speed_unit)
  lap_time_unit <- match.arg(lap_time_unit)
  acc_unit      <- match.arg(acc_unit)
  n <- nrow(df)
  out_list <- setNames(vector("list", length(required_fields)), required_fields)
  
  for (f in required_fields) {
    sel <- mapping[[f]]
    
    if (is.null(sel) || sel == "<none>" || is.na(sel) || !nzchar(sel)) {
      out_list[[f]] <- rep(NA, n)
      next
    }
    if (!sel %in% names(df)) {
      out_list[[f]] <- rep(NA, n)
      next
    }
    
    col <- df[[sel]]
    
    if (f %in% c(
      "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
      "speedKmh", "brake", "gas", "carPositionNormalized",
      "engineRPM", "steer", "lapTime",
      "accGHorizontal", "accGFrontal"
    )) {
      out_list[[f]] <- suppressWarnings(as.numeric(col))
      
    } else if (f %in% c("lapCount", "gear")) {
      out_list[[f]] <- suppressWarnings(as.integer(as.numeric(col)))
      
    } else if (f %in% c("isAbsInAction", "isTcInAction", "isInPit")) {
      
      if (is.logical(col)) {
        out_list[[f]] <- as.integer(col)
      } else {
        numeric_col <- suppressWarnings(as.numeric(col))
        if (sum(!is.na(numeric_col)) >= max(1, floor(0.8 * length(numeric_col)))) {
          out_list[[f]] <- as.integer(ifelse(is.na(numeric_col), NA, numeric_col > 0))
        } else {
          txt <- tolower(trimws(as.character(col)))
          out_list[[f]] <- as.integer(ifelse(
            txt %in% c("true", "t", "yes", "y", "1", "on", "in", "pit", "pitlane"),
            1,
            ifelse(txt %in% c("false", "f", "no", "n", "0", "off", "out"), 0, NA)
          ))
        }
      }
      
    } else {
      out_list[[f]] <- col
    }
    
    if (length(out_list[[f]]) == 1 && n > 1) out_list[[f]] <- rep(out_list[[f]], n)
  }
  
  if (!all(is.na(out_list[["speedKmh"]]))) {
    svals <- out_list[["speedKmh"]]
    do_convert <- if (speed_unit == "ms") {
      TRUE
    } else if (speed_unit == "kmh") {
      FALSE
    } else {
      # auto: use improved heuristics based on physical limits
      # 120 m/s = 432 km/h (impossible for any race car) -> must already be km/h
      # median > 50 m/s = 180 km/h average (unrealistic for m/s dataset) -> km/h
      q99  <- quantile(svals, 0.99, na.rm = TRUE)
      med  <- median(svals, na.rm = TRUE)
      is_kmh <- (is.finite(q99) && q99 > 120) || (is.finite(med) && med > 50)
      !is_kmh
    }
    if (do_convert) out_list[["speedKmh"]] <- out_list[["speedKmh"]] * 3.6
  }
  
  if (!all(is.na(out_list[["lapTime"]]))) {
    lvals <- out_list[["lapTime"]]
    do_convert <- if (lap_time_unit == "seconds") {
      TRUE
    } else if (lap_time_unit == "milliseconds") {
      FALSE
    } else {
      # auto: no real racing lap exceeds 1000 seconds; if median <= 1000 values are in seconds
      med <- median(lvals, na.rm = TRUE)
      is.finite(med) && med <= 1000
    }
    if (do_convert) out_list[["lapTime"]] <- out_list[["lapTime"]] * 1000
  }
  
  if (!all(is.na(out_list[["carPositionNormalized"]]))) {
    mx <- suppressWarnings(max(out_list[["carPositionNormalized"]], na.rm = TRUE))
    if (is.finite(mx) && mx > 1.1) {
      out_list[["carPositionNormalized"]] <- out_list[["carPositionNormalized"]] / mx
    }
  }

  for (acc_col in c("accGHorizontal", "accGFrontal")) {
    if (!all(is.na(out_list[[acc_col]]))) {
      avals <- out_list[[acc_col]]
      do_convert <- if (acc_unit == "ms2") {
        TRUE
      } else if (acc_unit == "g") {
        FALSE
      } else {
        # auto: if 99th percentile of absolute values > 10, values are almost certainly m/s²
        # (10G is physically impossible for a race car; 10 m/s² ≈ 1G is normal)
        q99 <- quantile(abs(avals), 0.99, na.rm = TRUE)
        is.finite(q99) && q99 > 10
      }
      if (do_convert) out_list[[acc_col]] <- out_list[[acc_col]] / 9.81
    }
  }
  
  tibble::as_tibble(out_list)
}

# =============================================================================
# Preprocessing
# =============================================================================
select_analysis_columns <- function(telemetry) {
  telemetry |>
    select(any_of(c(
      "time", "lapCount", "lapTime",
      "carPositionNormalized", "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
      "speedKmh", "speedMs", "engineRPM", "gear",
      "gas", "brake", "steer",
      "isAbsInAction", "isTcInAction", "isInPit",
      "accGHorizontal", "accGFrontal"
    )))
}

lap_overview_tbl <- function(telemetry) {
  if (!("lapCount" %in% names(telemetry))) return(tibble(lapCount = integer(0)))
  
  has_pit <- "isInPit" %in% names(telemetry)
  safe_max <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else max(x)
  }
  
  telemetry |>
    group_by(lapCount) |>
    summarise(
      rows = n(),
      pit_rows = if (has_pit) sum(isInPit == 1, na.rm = TRUE) else NA_integer_,
      lap_time_ms = if ("lapTime" %in% names(telemetry)) safe_max(lapTime) else NA_real_,
      lap_time_sec = if ("lapTime" %in% names(telemetry)) safe_max(lapTime) / 1000 else NA_real_,
      .groups = "drop"
    ) |>
    mutate(
      lap_time_formatted = if_else(
        is.finite(lap_time_sec),
        sprintf("%d:%05.2f", floor(lap_time_sec / 60), lap_time_sec %% 60),
        NA_character_
      )
    ) |>
    arrange(lapCount)
}

apply_preprocess_filters <- function(
    telemetry,
    lap_offset = 1L,
    remove_laps_with_pit = TRUE,
    laps_to_exclude = integer(0),
    max_lap_time_seconds = NULL,
    min_start_speed_kmh = 100,
    min_overall_speed_kmh = 10
) {
  out <- telemetry
  
  if ("lapCount" %in% names(out) && !("originalLapCount" %in% names(out))) {
    out <- out |> mutate(originalLapCount = lapCount)
  }
  
  if (!is.null(laps_to_exclude) && length(laps_to_exclude) > 0 && "originalLapCount" %in% names(out)) {
    out <- out |> filter(!originalLapCount %in% laps_to_exclude)
  }
  
  if ("lapCount" %in% names(out) && !is.null(lap_offset) && is.finite(lap_offset)) {
    out <- out |> mutate(lapCount = as.integer(lapCount) + as.integer(lap_offset))
  }
  
  if (remove_laps_with_pit && "isInPit" %in% names(out) && "lapCount" %in% names(out)) {
    pit_laps <- out |>
      filter(isInPit == 1) |>
      pull(lapCount) |>
      unique()
    
    if (length(pit_laps) > 0) out <- out |> filter(!lapCount %in% pit_laps)
  }
  
  if (!is.null(max_lap_time_seconds) && is.finite(max_lap_time_seconds) &&
      "lapCount" %in% names(out) && "lapTime" %in% names(out)) {
    
    slow_laps <- out |>
      group_by(lapCount) |>
      summarise(
        lap_time_sec = {
          vals <- lapTime[!is.na(lapTime)]
          if (length(vals) == 0) NA_real_ else max(vals) / 1000
        },
        .groups = "drop"
      ) |>
      filter(is.finite(lap_time_sec) & lap_time_sec > max_lap_time_seconds) |>
      pull(lapCount)
    
    if (length(slow_laps) > 0) out <- out |> filter(!lapCount %in% slow_laps)
  }
  
  
  if (!is.null(min_start_speed_kmh) && is.finite(min_start_speed_kmh) &&
      "lapCount" %in% names(out) && "speedKmh" %in% names(out)) {
    
    bad_start_laps <- out |>
      group_by(lapCount) |>
      summarise(
        start_speed_kmh = {
          vals <- speedKmh[!is.na(speedKmh)]
          if (length(vals) == 0) NA_real_ else vals[1]
        },
        .groups = "drop"
      ) |>
      filter(is.finite(start_speed_kmh) & start_speed_kmh < min_start_speed_kmh) |>
      pull(lapCount)
    
    if (length(bad_start_laps) > 0) out <- out |> filter(!lapCount %in% bad_start_laps)
  }
  
  if (!is.null(min_overall_speed_kmh) && is.finite(min_overall_speed_kmh) &&
      "lapCount" %in% names(out) && "speedKmh" %in% names(out)) {
    
    bad_min_speed_laps <- out |>
      group_by(lapCount) |>
      summarise(
        min_speed_kmh = {
          vals <- speedKmh[!is.na(speedKmh)]
          if (length(vals) == 0) NA_real_ else min(vals)
        },
        .groups = "drop"
      ) |>
      filter(is.finite(min_speed_kmh) & min_speed_kmh < min_overall_speed_kmh) |>
      pull(lapCount)
    
    if (length(bad_min_speed_laps) > 0) out <- out |> filter(!lapCount %in% bad_min_speed_laps)
  }
  
  out
}

# default minimal set used as initial checkbox selection
default_minimal_cols <- c(
  "lapCount", "lapTime",
  "carPositionNormalized", "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
  "speedKmh", "engineRPM", "gear",
  "gas", "brake", "steer",
  "isAbsInAction", "isTcInAction"
)

# =============================================================================
# Shiny App
# =============================================================================
run_telemetry_harmonize_and_preprocess_app <- function() {
  
  ui <- fluidPage(
    titlePanel("Telemetry Harmonizer plus Preprocessing"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV file", accept = c(".csv")),
        checkboxInput("has_header", "File has header", value = TRUE),
        numericInput("preview_n", "Preview rows", value = 8, min = 1),
        hr(),
        
        h4("Harmonization"),
        radioButtons(
          "speed_unit",
          "Input speed unit",
          choices = c("Auto-detect" = "auto", "km/h" = "kmh", "m/s" = "ms"),
          selected = "auto",
          inline = TRUE
        ),
        radioButtons(
          "lap_time_unit",
          "Input lap time unit",
          choices = c("Auto-detect" = "auto", "Milliseconds" = "milliseconds", "Seconds" = "seconds"),
          selected = "auto",
          inline = TRUE
        ),
        radioButtons(
          "acc_unit",
          "Input acceleration unit",
          choices = c("Auto-detect" = "auto", "G forces" = "g", "m/s²" = "ms2"),
          selected = "auto",
          inline = TRUE
        ),
        actionButton("apply_harmonize", "Apply harmonization"),
        
        hr(),
        
        h4("Preprocessing"),
        numericInput("lap_offset", "Lap count offset (add to lapCount)", value = 1, step = 1),
        checkboxInput("remove_laps_with_pit", "Remove entire laps that contain any pit data", value = TRUE),
        textInput("laps_to_exclude", "Manual laps to exclude by original lap count (comma separated)", value = ""),
        checkboxInput("enable_max_lap_time", "Enable max lap time filter", value = FALSE),
        numericInput("max_lap_time_seconds", "Max lap time (seconds)", value = 55, min = 1, step = 1),
        checkboxInput("enable_min_start_speed", "Enable minimum speed at lap start filter", value = FALSE),
        numericInput("min_start_speed_kmh", "Minimum speed at lap start (km/h)", value = 100, min = 0, step = 1),
        checkboxInput("enable_min_overall_speed", "Enable minimum overall speed filter", value = FALSE),
        numericInput("min_overall_speed_kmh", "Minimum overall speed (km/h)", value = 10, min = 0, step = 1),
        actionButton("apply_preprocess", "Apply preprocessing"),
        
        hr(),
        
        h4("Output columns"),
        helpText("Choose columns for the filtered and minimal downloads."),
        uiOutput("columns_ui"),
        
        hr(),
        
        h4("Downloads"),
        downloadButton("download_harmonized", "Download harmonized CSV"),
        downloadButton("download_filtered", "Download filtered CSV"),
        downloadButton("download_minimal", "Download minimal CSV"),
        
        width = 3
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "1 Preview",
            verbatimTextOutput("file_info"),
            tableOutput("preview")
          ),
          tabPanel(
            "2 Harmonize Mapping",
            uiOutput("mapping_ui"),
            verbatimTextOutput("mapping_note"),
            verbatimTextOutput("harmonize_status"),
            tableOutput("harm_preview")
          ),
          tabPanel(
            "3 Preprocess",
            verbatimTextOutput("preprocess_status"),
            h4("Lap overview before"),
            tableOutput("lap_overview_before"),
            h4("Lap overview after"),
            tableOutput("lap_overview_after"),
            h4("Filtered data preview"),
            tableOutput("filtered_preview")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    uploaded <- reactive({
      req(input$file)
      df <- read_csv(
        input$file$datapath,
        col_names = input$has_header,
        col_types = cols(.default = col_guess()),
        show_col_types = FALSE,
        name_repair = "unique_quiet"
      )
      names(df) <- make.names(names(df), unique = TRUE)
      df
    })
    
    output$file_info <- renderText({
      if (is.null(input$file)) return("No file uploaded.")
      df <- uploaded()
      paste0(
        "Rows: ", nrow(df), "  Columns: ", ncol(df),
        "\nDetected column names:\n", paste(names(df), collapse = ", ")
      )
    })
    
    output$preview <- renderTable({
      req(uploaded())
      head(uploaded(), n = input$preview_n)
    })
    
    suggested <- reactive({
      req(uploaded())
      guess_mapping(names(uploaded()))
    })
    
    output$mapping_ui <- renderUI({
      req(uploaded())
      choices <- c("<none>", names(uploaded()))
      tagList(
        lapply(required_fields, function(field) {
          selectInput(
            paste0("map__", field),
            label = if (field %in% names(field_labels)) field_labels[[field]] else field,
            choices = choices,
            selected = unname(dplyr::coalesce(suggested()[field], "<none>"))
          )
        })
      )
    })
    
    mapping_values <- reactive({
      req(uploaded())
      sapply(required_fields, function(field) input[[paste0("map__", field)]], USE.NAMES = TRUE, simplify = TRUE)
    })
    
    output$mapping_note <- renderText({
      req(mapping_values())
      miss <- sum(is.na(mapping_values()) | mapping_values() == "<none>")
      paste0(
        "Required fields: ", length(required_fields),
        "  Unmapped: ", miss,
        "\nNote: isInPit is only needed if you want pit lap removal in preprocessing."
      )
    })
    
    harmonized <- eventReactive(input$apply_harmonize, {
      req(uploaded())
      mapv <- mapping_values()
      out <- harmonize_telemetry(
        df = uploaded(),
        mapping = mapv,
        speed_unit = input$speed_unit,
        lap_time_unit = input$lap_time_unit,
        acc_unit = input$acc_unit
      ) |>
        select_analysis_columns()
      save_output_csv(out, "_harmonized")
      out
    }, ignoreNULL = TRUE)
    
    output$harmonize_status <- renderText({
      if (is.null(input$apply_harmonize) || input$apply_harmonize == 0) {
        return("Not harmonized yet. Click Apply harmonization.")
      }
      h <- harmonized()
      missing_all_na <- names(h)[vapply(h, function(col) all(is.na(col)), logical(1))]
      if (length(missing_all_na)) {
        paste0("Harmonized. Warning: these fields are all NA: ", paste(missing_all_na, collapse = ", "))
      } else {
        "Harmonized. All required fields present (not all values necessarily non missing)."
      }
    })
    
    output$harm_preview <- renderTable({
      req(harmonized())
      head(harmonized(), n = input$preview_n)
    })
    
    upload_target_dir <- reactive({
      req(input$file)
      dirname(normalizePath(input$file$datapath, winslash = "/", mustWork = FALSE))
    })
    
    upload_target_stem <- reactive({
      req(input$file)
      tools::file_path_sans_ext(basename(input$file$name))
    })
    
    save_output_csv <- function(df, suffix) {
      req(input$file)
      out_path <- file.path(upload_target_dir(), paste0(upload_target_stem(), suffix, ".csv"))
      write_csv(df, out_path)
      out_path
    }
    
    # Column selection UI, shown only after harmonization
    output$columns_ui <- renderUI({
      if (is.null(input$apply_harmonize) || input$apply_harmonize == 0) {
        return(helpText("Apply harmonization first to choose columns."))
      }
      req(harmonized())
      cols <- names(harmonized())
      
      default_sel <- intersect(default_minimal_cols, cols)
      if (length(default_sel) == 0) default_sel <- cols
      
      tagList(
        checkboxGroupInput(
          "cols_filtered",
          "Columns to keep for filtered download",
          choices = cols,
          selected = cols
        ),
        checkboxGroupInput(
          "cols_minimal",
          "Columns to keep for minimal download",
          choices = cols,
          selected = default_sel
        )
      )
    })
    
    preprocess_inputs <- reactive({
      list(
        lap_offset = as.integer(input$lap_offset),
        remove_laps_with_pit = isTRUE(input$remove_laps_with_pit),
        laps_to_exclude = parse_int_list(input$laps_to_exclude),
        max_lap_time_seconds = if (isTRUE(input$enable_max_lap_time)) as.numeric(input$max_lap_time_seconds) else NULL,
        min_start_speed_kmh = if (isTRUE(input$enable_min_start_speed)) as.numeric(input$min_start_speed_kmh) else NULL,
        min_overall_speed_kmh = if (isTRUE(input$enable_min_overall_speed)) as.numeric(input$min_overall_speed_kmh) else NULL
      )
    })
    
    telemetry_filtered <- eventReactive(input$apply_preprocess, {
      req(harmonized())
      p <- preprocess_inputs()
      out <- apply_preprocess_filters(
        telemetry = harmonized(),
        lap_offset = p$lap_offset,
        remove_laps_with_pit = p$remove_laps_with_pit,
        laps_to_exclude = p$laps_to_exclude,
        max_lap_time_seconds = p$max_lap_time_seconds,
        min_start_speed_kmh = p$min_start_speed_kmh,
        min_overall_speed_kmh = p$min_overall_speed_kmh
      )
      save_output_csv(out, "_filtered")
      out
    }, ignoreNULL = TRUE)
    
    output$preprocess_status <- renderText({
      if (is.null(input$apply_preprocess) || input$apply_preprocess == 0) {
        return("Not preprocessed yet. Click Apply preprocessing.")
      }
      req(harmonized())
      req(telemetry_filtered())
      
      p <- preprocess_inputs()
      
      before_rows <- nrow(harmonized())
      after_rows <- nrow(telemetry_filtered())
      
      before_laps <- if ("lapCount" %in% names(harmonized())) length(unique(harmonized()$lapCount)) else NA_integer_
      after_laps <- if ("lapCount" %in% names(telemetry_filtered())) length(unique(telemetry_filtered()$lapCount)) else NA_integer_
      
      msg <- c(
        paste0("Rows: ", before_rows, " -> ", after_rows),
        paste0("Laps: ", before_laps, " -> ", after_laps),
        paste0("Lap offset: ", p$lap_offset),
        paste0("Remove pit laps: ", p$remove_laps_with_pit),
        paste0("Manual exclusions: ", if (length(p$laps_to_exclude)) paste(p$laps_to_exclude, collapse = ", ") else "none"),
        paste0("Max lap time filter: ", if (is.null(p$max_lap_time_seconds)) "off" else paste0(p$max_lap_time_seconds, " s")),
        paste0("Min lap start speed filter: ", if (is.null(p$min_start_speed_kmh)) "off" else paste0(p$min_start_speed_kmh, " km/h")),
        paste0("Min overall speed filter: ", if (is.null(p$min_overall_speed_kmh)) "off" else paste0(p$min_overall_speed_kmh, " km/h"))
      )
      
      if (p$remove_laps_with_pit && !("isInPit" %in% names(harmonized()))) {
        msg <- c(msg, "Warning: isInPit not available, so pit lap removal did nothing.")
      }
      
      paste(msg, collapse = "\n")
    })
    
    output$lap_overview_before <- renderTable({
      req(harmonized())
      head(lap_overview_tbl(harmonized()), n = 200)
    })
    
    output$lap_overview_after <- renderTable({
      req(telemetry_filtered())
      head(lap_overview_tbl(telemetry_filtered()), n = 200)
    })
    
    output$filtered_preview <- renderTable({
      req(telemetry_filtered())
      head(telemetry_filtered(), n = input$preview_n)
    })
    
    filtered_selected <- reactive({
      req(telemetry_filtered())
      if (is.null(input$cols_filtered) || length(input$cols_filtered) == 0) return(telemetry_filtered())
      telemetry_filtered() |> select(any_of(input$cols_filtered))
    })
    
    minimal_selected <- reactive({
      req(telemetry_filtered())
      if (is.null(input$cols_minimal) || length(input$cols_minimal) == 0) {
        return(telemetry_filtered() |> select(any_of(default_minimal_cols)))
      }
      telemetry_filtered() |> select(any_of(input$cols_minimal))
    })
    
    output$download_harmonized <- downloadHandler(
      filename = function() paste0("telemetry_harmonized_", Sys.Date(), ".csv"),
      content = function(file) {
        req(harmonized())
        write_csv(harmonized(), file)
        save_output_csv(harmonized(), "_harmonized")
      }
    )
    
    output$download_filtered <- downloadHandler(
      filename = function() paste0("telemetry_filtered_", Sys.Date(), ".csv"),
      content = function(file) {
        req(filtered_selected())
        write_csv(filtered_selected(), file)
        save_output_csv(filtered_selected(), "_filtered")
      }
    )
    
    output$download_minimal <- downloadHandler(
      filename = function() paste0("telemetry_minimal_", Sys.Date(), ".csv"),
      content = function(file) {
        req(minimal_selected())
        write_csv(minimal_selected(), file)
        save_output_csv(minimal_selected(), "_minimal")
      }
    )
  }
  
  shinyApp(ui, server)
}

# Run
run_telemetry_harmonize_and_preprocess_app()


