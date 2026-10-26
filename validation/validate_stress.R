# Stress-test validation (reviewer revision, R2.2 / R1.6)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj):
#   source("validation/validate_stress.R")
#
# Runs the toolkit (telemetry_core.R) on telemetry_validation_stress.csv:
# 9 laps, each with one recording problem (sampling rate 10/60/100 Hz,
# irregular intervals, dropped samples, NA values, missing lapTime, a missing
# channel, and all combined). Compares every result with the true value from
# telemetry_validation_stress_expected.csv (see make_validation_stress.R for
# how the data, true values and tolerances are defined) and reports expected,
# calculated and error. The v1.0.0 calculation (arithmetic mean, row counts,
# last-sample lap time) is shown alongside for the lap summary metrics.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

tel <- read_csv(file.path(vdir, "telemetry_validation_stress.csv"), show_col_types = FALSE)
expected <- read_csv(file.path(vdir, "telemetry_validation_stress_expected.csv"), show_col_types = FALSE)

thr_gas <- 0.99
thr_brake <- 0.01
corner_entry <- 0.30
corner_exit <- 0.45
sector_bounds <- tibble(
  seg   = 1:4,
  start = c(0.15, 0.45, 0.70, 0.90),
  end   = c(0.45, 0.70, 0.90, 0.15)
)

# v1.0.0 lap summary formulas (for comparison only)
legacy_lap_stats <- function(lap_data) {
  lap_data |>
    summarise(
      `Lap Time (s)` = max(lapTime, na.rm = TRUE) / 1000,
      `Full Throttle (%)` = sum(gas > thr_gas, na.rm = TRUE) / n() * 100,
      `Braking (%)` = sum(brake > thr_brake, na.rm = TRUE) / n() * 100,
      `Brake/Throttle Overlap (%)` = sum(gas > thr_gas & brake > thr_brake, na.rm = TRUE) / n() * 100,
      `Avg Speed (km/h)` = mean(speedKmh, na.rm = TRUE),
      `ABS Active (%)` = sum(isAbsInAction == 1, na.rm = TRUE) / n() * 100,
      `TC Active (%)` = sum(isTcInAction == 1, na.rm = TRUE) / n() * 100
    ) |>
    pivot_longer(everything(), names_to = "metric", values_to = "v1_calculated")
}

calculated <- map_dfr(sort(unique(tel$lapCount)), function(lap) {
  d <- tel |> filter(lapCount == lap)

  lap_vals <- calculate_lap_stats(tel, lap, thr_gas, thr_brake) |>
    select(-Lap) |>
    pivot_longer(everything(), names_to = "metric", values_to = "calculated")

  corner <- corner_summary_table(d |> mutate(lap_label = "lap"), corner_entry, corner_exit)
  corner_vals <- tibble(
    metric = c("Corner Time (s)", "Corner Entry Speed (km/h)", "Corner Exit Speed (km/h)",
               "Corner Min Speed (km/h)", "Corner Avg Speed (km/h)"),
    calculated = c(corner$corner_time_s, corner$entry_speed_kmh, corner$exit_speed_kmh,
                   corner$min_speed_kmh, corner$avg_speed_kmh)
  )

  sec <- compute_seg_times_real(d, sector_bounds)
  sector_vals <- tibble(metric = paste0("Sector ", names(sec), " Time (s)"), calculated = sec / 1000)

  bind_rows(lap_vals, corner_vals, sector_vals) |>
    left_join(legacy_lap_stats(d), by = "metric") |>
    mutate(lap = lap, .before = 1)
})

results <- expected |>
  left_join(calculated, by = c("lap", "metric")) |>
  mutate(
    error = calculated - expected,
    pass = if_else(is.na(expected), is.na(calculated),
                   !is.na(calculated) & abs(error) <= tolerance),
    v1_error = v1_calculated - expected
  )

# =============================================================================
# REPORT
# =============================================================================
options(pillar.sigfig = 6)
cat("\nStress test: expected vs calculated, per lap and metric\n")
print(results |>
        select(lap, metric, expected, calculated, error, tolerance, pass) |>
        mutate(across(c(expected, calculated, error, tolerance), \(x) signif(x, 6))),
      n = Inf, width = Inf)

cat("\nLap summary metrics: largest |error| per lap, toolkit vs v1.0.0 method\n")
summary_tbl <- results |>
  filter(!is.na(v1_calculated) | metric %in% c("Lap Time (s)", "Avg Speed (km/h)")) |>
  filter(metric %in% c("Lap Time (s)", "Avg Speed (km/h)", "Full Throttle (%)", "Braking (%)",
                       "Brake/Throttle Overlap (%)", "ABS Active (%)", "TC Active (%)")) |>
  group_by(lap, condition, metric) |>
  summarise(error = abs(error), v1_error = abs(v1_error), .groups = "drop") |>
  mutate(across(c(error, v1_error), \(x) round(x, 4)))
print(summary_tbl |>
        pivot_wider(id_cols = c(lap, condition), names_from = metric,
                    values_from = c(error, v1_error), names_glue = "{metric} [{.value}]"),
      n = Inf, width = Inf)

n_fail <- sum(!results$pass)
cat("\n", nrow(results) - n_fail, " / ", nrow(results), " checks passed\n", sep = "")
if (n_fail > 0) {
  cat("FAILED:\n")
  print(results |> filter(!pass) |> select(lap, condition, metric, expected, calculated, error, tolerance),
        n = Inf, width = Inf)
} else {
  cat("ALL CHECKS PASSED\n")
}

write_csv(results, file.path(vdir, "validation_stress_results.csv"), na = "NA")
cat("Results saved to validation/validation_stress_results.csv\n")
