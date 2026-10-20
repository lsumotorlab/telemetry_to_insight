# Real-data check: toolkit lap times vs the simulator's official lap times (R1.2)
# ===================================================================
# Uses a raw telemetry file that contains the simulator's own lap time for the
# previous lap (Assetto Corsa: column "lastLap", in ms). The data file is NOT
# part of the repository; set its path before running, from the project folder:
#
#   real_file <- "C:/path/to/your_telemetry.csv"
#   source("validation/validate_real_lap_times.R")
#
# For each completed lap N, the official lap time is the "lastLap" value on the
# first sample of lap N+1. The toolkit lap time (lap_time_estimate in
# telemetry_core.R) is compared with it, together with the v1.0.0 method
# (lapTime of the last sample). Only a per-lap summary is written:
# validation/validation_real_lap_times_results.csv (no raw data).
#
# Column names can be changed below for other simulators/loggers.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

if (!exists("real_file") || !file.exists(real_file)) {
  stop("Set real_file to the path of a telemetry CSV with an official lap-time column, e.g.\n",
       '  real_file <- "C:/path/to/telemetry.csv"; source("validation/validate_real_lap_times.R")',
       call. = FALSE)
}

# Column names in the raw file (Assetto Corsa defaults); times in ms
col_lap      <- "lapCount"
col_lap_time <- "lapTime"
col_official <- "lastLap"
col_pos      <- "carPositionNormalized"
col_pit      <- "isInPit"

cols <- c(col_lap, col_lap_time, col_official, col_pos, col_pit)
raw <- read_csv(real_file, col_select = any_of(cols), show_col_types = FALSE, progress = FALSE)
missing_cols <- setdiff(setdiff(cols, col_pit), names(raw))
if (length(missing_cols) > 0) stop("Missing columns in real_file: ", paste(missing_cols, collapse = ", "))

raw <- raw |>
  transmute(
    lap = .data[[col_lap]],
    lapTime = as.numeric(.data[[col_lap_time]]),
    official = as.numeric(.data[[col_official]]),
    pos = as.numeric(.data[[col_pos]]),
    pit = if (col_pit %in% names(raw)) .data[[col_pit]] else NA
  )

laps <- sort(unique(raw$lap))

results <- map_dfr(laps[-length(laps)], function(L) {
  d <- raw |> filter(lap == L)
  nxt <- raw |> filter(lap == laps[match(L, laps) + 1])
  dt <- diff(sort(d$lapTime[is.finite(d$lapTime)]))
  tibble(
    lap = L,
    samples = nrow(d),
    median_interval_ms = median(dt),
    max_interval_ms = max(dt),
    pit_samples = sum(d$pit == 1, na.rm = TRUE),
    official_s = nxt$official[1] / 1000,
    toolkit_s = lap_time_estimate(d$pos, d$lapTime) / 1000,
    v1_last_sample_s = safe_max(d$lapTime) / 1000
  )
}) |>
  mutate(
    toolkit_error_ms = round((toolkit_s - official_s) * 1000, 2),
    v1_error_ms = round((v1_last_sample_s - official_s) * 1000, 2)
  )

options(pillar.sigfig = 7)
cat("\nLap times: toolkit and v1.0.0 method vs the simulator's official lap time\n")
print(results, n = Inf, width = Inf)

flying <- results |> filter(pit_samples == 0)
cat("\nLaps without pit samples:", nrow(flying), "\n")
cat("  toolkit: max |error| =", max(abs(flying$toolkit_error_ms)), "ms, mean error =",
    round(mean(flying$toolkit_error_ms), 2), "ms\n")
cat("  v1.0.0 : max |error| =", max(abs(flying$v1_error_ms)), "ms, mean error =",
    round(mean(flying$v1_error_ms), 2), "ms\n")

write_csv(results, file.path(vdir, "validation_real_lap_times_results.csv"))
cat("Summary saved to validation/validation_real_lap_times_results.csv\n")
