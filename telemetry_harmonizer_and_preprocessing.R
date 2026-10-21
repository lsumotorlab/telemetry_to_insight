# telemetry_harmonize_and_preprocess_app.R
# =======================================

library(shiny)
library(tidyverse)
library(readr)
filter <- dplyr::filter
options(shiny.maxRequestSize = 500 * 1024^2)

# Shared calculation functions (telemetry_core.R, same folder as this script):
# guess_mapping(), harmonize_telemetry(), apply_preprocess_filters(), ...
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

# =============================================================================
# Helpers
# =============================================================================
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
            verbatimTextOutput("lateral_check"),
            verbatimTextOutput("harmonize_status"),
            h4("Units used"),
            helpText("For each variable: the unit setting, the statistic of the input values used by",
                     "auto-detect, the rule, and the result. If a result is wrong, choose the unit",
                     "manually in the sidebar and click Apply harmonization again."),
            tableOutput("units_report"),
            tableOutput("harm_preview")
          ),
          tabPanel(
            "3 Preprocess",
            h4("Data quality per lap (harmonized data, before filters)"),
            helpText(
              "Sampling rate = samples per second of recorded time. Long gaps = intervals more than",
              "5 times the median interval. Missing values = % of samples without a value, per channel.",
              "'Car (nearly) stopped' = minimum speed below 10 km/h; 'moves backwards' = track position",
              "falls by more than 0.5% of a lap (e.g., a spin). Lap comparison cannot represent these",
              "periods, so consider excluding such laps."
            ),
            tableOutput("lap_quality"),
            downloadButton("download_quality", "Download data-quality report"),
            hr(),
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
    
    # Updates as the mapping changes, so a different column can be tried at once
    output$lateral_check <- renderText({
      req(uploaded())
      m <- mapping_values()
      req(is.character(m))
      chk <- check_lateral_channel(uploaded(), as.list(m))
      if (is.null(chk$table) || nrow(chk$table) == 0) return(chk$message)
      paste0(
        chk$message,
        "\nCorrelation with steering x speed^2 per column: ",
        paste0(chk$table$column, " ", sprintf("%.2f", chk$table$r), collapse = ", ")
      )
    })

    units_report <- reactiveVal(NULL)

    harmonized <- eventReactive(input$apply_harmonize, {
      req(uploaded())
      mapv <- mapping_values()
      out <- harmonize_telemetry(
        df = uploaded(),
        mapping = mapv,
        speed_unit = input$speed_unit,
        lap_time_unit = input$lap_time_unit,
        acc_unit = input$acc_unit
      )
      units_report(attr(out, "units_report"))
      out <- select_analysis_columns(out)
      save_output_csv(out, "_harmonized")
      out
    }, ignoreNULL = TRUE)

    output$units_report <- renderTable({
      req(harmonized())
      units_report()
    })
    
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
    
    lap_quality <- reactive({
      req(harmonized())
      lap_quality_tbl(harmonized())
    })

    output$lap_quality <- renderTable({
      head(lap_quality(), n = 200)
    }, digits = 1)

    output$download_quality <- downloadHandler(
      filename = function() paste0(upload_target_stem(), "_data_quality.csv"),
      content = function(file) write_csv(lap_quality(), file)
    )

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


