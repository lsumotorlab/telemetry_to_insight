# Effect of linear interpolation on peak values (reviewer revision, R1.7)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj), then:
#   source("validation/validate_interpolation_peaks.R")
# Optional real-data part (data file NOT in the repository):
real_file <- r"(C:\Users\galzi\OneDrive - Lietuvos sporto universitetas\Research\Telemetry Methodology\paper\frontiers_sport_active_living\telemetry_example.csv)"
real_lateral_col <- "accGVertical"
source("validation/validate_interpolation_peaks.R")
#
# Two tools resample laps by linear interpolation on a grid of track positions:
#   - lap comparison: both laps on a common grid (default 3000 points since
#     v1.1.0, after this test; 800 in v1.0.0)
#   - composite reference lap: position grid, default = samples in the first lap
# Linear interpolation can only lower a maximum (raise a minimum), never create
# a higher one, because every interpolated value lies between two samples.
# This script measures how much peaks are lowered.
#
# Part A: synthetic laps with exactly known peaks (true values set by formula,
#         independent of the toolkit), sampled at 10, 30, 60, 100 and 240 Hz:
#           speed: a sharp peak at the end of a straight (accelerating
#                  0.2 km/h per m, braking 0.77 km/h per m; true max 250 km/h)
#                  and a smooth corner minimum (true min 80 km/h)
#           five bell-shaped (Gaussian) peaks of height 100 (so errors are in %
#                  of the peak) and width sigma = 2, 5, 10, 20 and 40 m
#         Lap length 2,000 m, lap time 60 s (mean 120 km/h). 20 laps per
#         sampling rate, each with the features shifted by 0.5 m more, so the
#         peaks fall at different places between samples and grid points.
#         Errors: sampled value vs true value (sampling itself) and
#         interpolated value vs sampled value (interpolation only).
# Part B: real session (if real_file is set): for each flying lap, peak speed,
#         minimum speed, peak |lateral G| and peak/minimum longitudinal G in
#         the recorded samples vs after interpolation, unfiltered and with the
#         toolkit's 5 Hz low-pass filter applied to the accelerations first.
#         Only a per-lap summary of peak values is written (no raw data).
#
# Tests the functions in telemetry_core.R, the same code the apps use.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

grid_sizes <- c(800, 1500, 3000)   # lap comparison: v1.0.0 default, 1500, current default
eps <- 1e-9

# Sector boundaries used to build composite laps (all sectors from one lap,
# so the composite lap is that lap resampled and retimed)
composite_of <- function(lap_df, lap_id) {
  segs <- tibble(seg = 1:4, start = c(0, 0.25, 0.5, 0.75), end = c(0.25, 0.5, 0.75, 0),
                 source_lap = lap_id)
  resample_and_retime_synthetic_lap(lap_df, segs, n_bins = nrow(lap_df))
}

# Lap comparison grid (the lap is compared with itself; the _ref columns are
# the interpolated lap)
comparison_of <- function(lap_df, vars, n) {
  prep <- prepare_lap_for_compare(lap_df)
  resample_two_laps(prep, prep, vars = vars, n_points = n) |>
    rename_with(\(x) sub("_ref$", "", x), ends_with("_ref"))
}

# =============================================================================
# Part A: synthetic laps with known peaks
# =============================================================================
L_m <- 2000
T_ms <- 60000
c_amp <- 0.3                       # speed of travel varies +/- 30 % along the lap
u_of_t <- \(t) t / T_ms + c_amp / (2 * pi) * sin(2 * pi * t / T_ms)

rates_hz <- c(10, 30, 60, 100, 240)
n_laps <- 20
shift_m <- (seq_len(n_laps) - 1) * 0.5

sigmas_m <- c(2, 5, 10, 20, 40)
gauss_cols <- paste0("peak_sigma_", sigmas_m, "m")

# Channel values as exact functions of distance x (m), features shifted by d
signals <- function(x, d) {
  xs <- x - d
  cusp <- ifelse(xs <= 600, 250 - 0.2 * (600 - xs), 250 - 0.77 * (xs - 600))
  corner <- 160 - 80 * exp(-(xs - 1400)^2 / (2 * 40^2))
  out <- tibble(speedKmh = pmax(cusp, corner))
  for (k in seq_along(sigmas_m)) {
    out[[gauss_cols[k]]] <- 100 * exp(-(xs - 1100)^2 / (2 * sigmas_m[k]^2))
  }
  out
}

# True extremes (value, position in m before the shift)
features <- tibble(
  feature = c("Speed max (km/h)", "Speed min (km/h)", paste0("Peak, sigma ", sigmas_m, " m (%)")),
  col = c("speedKmh", "speedKmh", gauss_cols),
  type = c("max", "min", rep("max", length(sigmas_m))),
  true_value = c(250, 80, rep(100, length(sigmas_m))),
  true_x = c(600, 1400, rep(1100, length(sigmas_m))),
  # Duration of the feature at the mean speed (33.3 m/s): full width at half
  # height for the Gaussian peaks
  description = c(
    "end of straight: +0.2 / -0.77 km/h per m",
    "corner, sigma 40 m",
    sprintf("half-height width %.1f m = %.2f s at 120 km/h", 2.3548 * sigmas_m, 2.3548 * sigmas_m / (L_m / T_ms * 1000))
  )
)

make_lap <- function(hz, k) {
  t <- seq(0, T_ms - eps, by = 1000 / hz)
  u <- u_of_t(t)
  bind_cols(
    tibble(lapCount = k, lapTime = t, carPositionNormalized = u,
           carCoordinatesX = 0, carCoordinatesZ = 0),
    signals(u * L_m, shift_m[k])
  )
}

extract_peaks <- function(df) {
  pmap_dfr(features, function(feature, col, type, ...) {
    y <- df[[col]]
    i <- if (type == "max") which.max(y) else which.min(y)
    tibble(feature = feature, value = y[i], x_m = df$carPositionNormalized[i] * L_m)
  })
}

vars_a <- c("speedKmh", gauss_cols)
peaks_a <- map_dfr(rates_hz, function(hz) {
  map_dfr(seq_len(n_laps), function(k) {
    lap <- make_lap(hz, k)
    methods <- c(
      list("Recorded samples" = lap),
      set_names(map(grid_sizes, \(n) comparison_of(lap, vars_a, n)),
                paste0("Lap comparison, ", grid_sizes, " points")),
      list("Composite lap" = composite_of(lap, k))
    )
    imap_dfr(methods, \(df, m) extract_peaks(df) |> mutate(method = m)) |>
      mutate(rate_hz = hz, lap = k, samples = nrow(lap), .before = 1)
  })
}) |>
  left_join(features |> select(feature, type, true_value, true_x), by = "feature") |>
  group_by(rate_hz, lap, feature) |>
  mutate(
    recorded = value[method == "Recorded samples"],
    error_vs_true = value - true_value,
    change_vs_recorded = value - recorded,
    x_error_m = x_m - (true_x + shift_m[lap])
  ) |>
  ungroup()

# Check: interpolation never creates a value beyond the recorded extreme
checks_a <- peaks_a |>
  filter(method != "Recorded samples") |>
  mutate(pass = if_else(type == "max", value <= recorded + eps, value >= recorded - eps))

# Worst = largest magnitude (signed)
worst <- \(x) x[which.max(abs(x))]
summary_a <- peaks_a |>
  group_by(rate_hz, method, feature) |>
  summarise(
    true_value = first(true_value),
    mean_error_vs_true = mean(error_vs_true),
    worst_error_vs_true = worst(error_vs_true),
    mean_change_vs_recorded = mean(change_vs_recorded),
    worst_change_vs_recorded = worst(change_vs_recorded),
    worst_position_error_m = worst(x_error_m),
    .groups = "drop"
  ) |>
  left_join(features |> select(feature, description), by = "feature") |>
  mutate(method = factor(method, levels = c("Recorded samples", paste0("Lap comparison, ", grid_sizes, " points"), "Composite lap")),
         feature = factor(feature, levels = features$feature)) |>
  arrange(feature, rate_hz, method)

cat("\n=== Part A: synthetic laps (", n_laps, "laps per sampling rate ) ===\n")
cat("Error vs true value = sampling + interpolation; change vs recorded = interpolation only\n")
cat("(negative = peak lowered, positive = minimum raised)\n\n")
print(summary_a |>
        select(feature, rate_hz, method, worst_error_vs_true, worst_change_vs_recorded, mean_change_vs_recorded) |>
        mutate(across(where(is.numeric) & !rate_hz, \(x) round(x, 3))),
      n = Inf, width = Inf)
cat("\nInterpolated value never beyond the recorded extreme:",
    sum(checks_a$pass), "/", nrow(checks_a), "checks passed\n")

write_csv(summary_a, file.path(vdir, "validation_interpolation_peaks_results.csv"))
cat("Summary saved to validation/validation_interpolation_peaks_results.csv\n")

# =============================================================================
# Part B: real session (optional)
# =============================================================================
if (exists("real_file") && file.exists(real_file)) {
  cutoff_hz <- 5
  if (!exists("real_lateral_col")) real_lateral_col <- "accGHorizontal"
  cols_b <- c("lapCount", "lapTime", "carPositionNormalized", "isInPit",
              "carCoordinatesX", "carCoordinatesZ", "speedKmh", real_lateral_col, "accGFrontal")
  raw <- read_csv(real_file, col_select = any_of(cols_b), show_col_types = FALSE, progress = FALSE)
  miss <- setdiff(cols_b, names(raw))
  if (length(miss) > 0) stop("Missing columns in real_file: ", paste(miss, collapse = ", "))
  # Lateral acceleration is analysed under the toolkit's standard name
  raw <- raw |> rename(accGHorizontal = all_of(real_lateral_col))
  cat("\nPart B: lateral acceleration taken from column", real_lateral_col, "\n")

  # Flying laps: completed (a later lap exists) and no pit samples
  laps <- sort(unique(raw$lapCount))
  flying <- laps[-length(laps)] |>
    keep(\(L) sum(raw$isInPit[raw$lapCount == L] == 1, na.rm = TRUE) == 0)

  features_b <- tibble(
    feature = c("Speed max (km/h)", "Speed min (km/h)", "|Lateral G| max",
                "Longitudinal G max", "Longitudinal G min"),
    col = c("speedKmh", "speedKmh", "accGHorizontal", "accGFrontal", "accGFrontal"),
    type = c("max", "min", "absmax", "max", "min")
  )
  extract_b <- function(df) {
    pmap_dfr(features_b, function(feature, col, type) {
      y <- df[[col]]
      tibble(feature = feature,
             value = switch(type, max = max(y, na.rm = TRUE), min = min(y, na.rm = TRUE),
                            absmax = max(abs(y), na.rm = TRUE)))
    })
  }
  vars_b <- c("speedKmh", "accGHorizontal", "accGFrontal")

  peaks_b <- map_dfr(flying, function(L) {
    d <- raw |> filter(lapCount == L, is.finite(lapTime)) |> arrange(lapTime)
    fs <- sampling_rate_hz(d$lapTime)
    versions <- list(
      "Unfiltered" = d,
      "Low-pass 5 Hz (accelerations)" = d |>
        mutate(across(c(accGHorizontal, accGFrontal), \(y) as.numeric(lowpass_lap(y, lapTime, cutoff_hz))))
    )
    imap_dfr(versions, function(dv, ver) {
      # Lap comparison uses absolute lateral G (as in the app)
      dv_abs <- dv |> mutate(accGHorizontal = abs(accGHorizontal))
      methods <- c(
        list("Recorded samples" = dv),
        set_names(map(grid_sizes, \(n) comparison_of(dv_abs, vars_b, n)),
                  paste0("Lap comparison, ", grid_sizes, " points")),
        list("Composite lap" = composite_of(dv, L))
      )
      imap_dfr(methods, \(df, m) extract_b(df) |> mutate(method = m)) |>
        mutate(version = ver, .before = 1)
    }) |>
      mutate(lap = L, samples = nrow(d), sampling_hz = round(fs, 1), .before = 1)
  }) |>
    left_join(features_b |> select(feature, type), by = "feature") |>
    group_by(version, lap, feature) |>
    mutate(recorded = value[method == "Recorded samples"],
           change = value - recorded,
           change_pct = 100 * change / abs(recorded)) |>
    ungroup()

  checks_b <- peaks_b |>
    filter(method != "Recorded samples") |>
    mutate(pass = if_else(type == "min", value >= recorded - eps, value <= recorded + eps))

  summary_b <- peaks_b |>
    filter(method != "Recorded samples") |>
    group_by(version, feature, method) |>
    summarise(
      mean_recorded = mean(recorded),
      mean_change = mean(change),
      worst_change = worst(change),
      mean_change_pct = mean(change_pct),
      worst_change_pct = worst(change_pct),
      .groups = "drop"
    ) |>
    mutate(method = factor(method, levels = c(paste0("Lap comparison, ", grid_sizes, " points"), "Composite lap")),
           feature = factor(feature, levels = features_b$feature)) |>
    arrange(version, feature, method)

  cat("\n=== Part B: real session,", length(flying), "flying laps, median sampling",
      round(median(peaks_b$sampling_hz), 1), "Hz ===\n")
  cat("Change = interpolated peak - recorded peak (negative = peak lowered, positive = minimum raised)\n\n")
  print(summary_b |> mutate(across(where(is.numeric), \(x) round(x, 3))), n = Inf, width = Inf)
  cat("\nInterpolated value never beyond the recorded extreme:",
      sum(checks_b$pass), "/", nrow(checks_b), "checks passed\n")

  write_csv(peaks_b |> select(version, lap, samples, sampling_hz, feature, method, value, recorded, change, change_pct),
            file.path(vdir, "validation_interpolation_peaks_real_results.csv"))
  cat("Per-lap peak values saved to validation/validation_interpolation_peaks_real_results.csv\n")
} else {
  cat("\nPart B skipped (set real_file to a telemetry CSV to run it).\n")
}
