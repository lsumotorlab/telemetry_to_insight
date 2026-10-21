# Real-data check: lap times and track length vs official values (R1.2)
# ===================================================================
# Uses a raw telemetry file that contains the simulator's own lap time for the
# previous lap (Assetto Corsa: column "lastLap", in ms). The data file is NOT
# part of the repository; set its path (and optionally the official track
# length in metres) before running, from the project folder:
#
#   real_file <- r"(yourfile.csv)"
#   official_track_length_m <- 1956          # optional
#   source("validation/validate_real_lap_times.R")
#
# 1) Lap time: for each completed lap N, the official lap time is the
#    "lastLap" value on the first sample of lap N+1. The toolkit lap time
#    (lap_time_estimate in telemetry_core.R) is compared with it, together with
#    the v1.0.0 method (lapTime of the last sample).
# 2) Track length: distance driven per lap = sum of the distances between
#    consecutive car positions (carCoordinates, m), horizontal (X-Z) and 3D.
#    The official length is measured along the track centreline, the car
#    follows its racing line, so differences of about 1% are expected.
# Only a per-lap summary is written:
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
col_x        <- "carCoordinatesX"
col_y        <- "carCoordinatesY"
col_z        <- "carCoordinatesZ"

if (!exists("official_track_length_m")) official_track_length_m <- NA_real_

cols <- c(col_lap, col_lap_time, col_official, col_pos, col_pit, col_x, col_y, col_z)
raw <- read_csv(real_file, col_select = any_of(cols), show_col_types = FALSE, progress = FALSE)
missing_cols <- setdiff(c(col_lap, col_lap_time, col_official, col_pos), names(raw))
if (length(missing_cols) > 0) stop("Missing columns in real_file: ", paste(missing_cols, collapse = ", "))
has_xz <- all(c(col_x, col_z) %in% names(raw))
has_y <- col_y %in% names(raw)

raw <- raw |>
  transmute(
    lap = .data[[col_lap]],
    lapTime = as.numeric(.data[[col_lap_time]]),
    official = as.numeric(.data[[col_official]]),
    pos = as.numeric(.data[[col_pos]]),
    pit = if (col_pit %in% names(raw)) .data[[col_pit]] else NA,
    x = if (has_xz) as.numeric(.data[[col_x]]) else NA_real_,
    y = if (has_y) as.numeric(.data[[col_y]]) else 0,
    z = if (has_xz) as.numeric(.data[[col_z]]) else NA_real_
  )

# Distance driven in one lap (m): sum of steps between consecutive positions,
# in time order; repeated positions (no update between samples) add nothing
lap_distance <- function(d, dims = c("x", "z")) {
  d <- d |> filter(is.finite(lapTime), if_all(all_of(dims), is.finite)) |> arrange(lapTime)
  if (nrow(d) < 2) return(NA_real_)
  steps <- sqrt(Reduce(`+`, lapply(dims, \(k) diff(d[[k]])^2)))
  sum(steps)
}

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
    v1_last_sample_s = safe_max(d$lapTime) / 1000,
    distance_xz_m = if (has_xz) lap_distance(d, c("x", "z")) else NA_real_,
    distance_3d_m = if (has_xz) lap_distance(d, c("x", "y", "z")) else NA_real_
  )
}) |>
  mutate(
    toolkit_error_ms = round((toolkit_s - official_s) * 1000, 2),
    v1_error_ms = round((v1_last_sample_s - official_s) * 1000, 2),
    distance_3d_diff_pct = round((distance_3d_m - official_track_length_m) / official_track_length_m * 100, 2)
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

if (has_xz) {
  cat("\nDistance driven per flying lap (from coordinates):\n")
  cat("  horizontal (X-Z): mean", round(mean(flying$distance_xz_m), 1), "m, range",
      round(min(flying$distance_xz_m), 1), "-", round(max(flying$distance_xz_m), 1), "m\n")
  cat("  3D (with elevation): mean", round(mean(flying$distance_3d_m), 1), "m, range",
      round(min(flying$distance_3d_m), 1), "-", round(max(flying$distance_3d_m), 1), "m\n")
  if (is.finite(official_track_length_m)) {
    cat("  official track length:", official_track_length_m, "m -> 3D difference: mean",
        round(mean(flying$distance_3d_diff_pct), 2), "%, range",
        min(flying$distance_3d_diff_pct), "to", max(flying$distance_3d_diff_pct), "%\n")
  }
}

write_csv(results, file.path(vdir, "validation_real_lap_times_results.csv"))
cat("Summary saved to validation/validation_real_lap_times_results.csv\n")
