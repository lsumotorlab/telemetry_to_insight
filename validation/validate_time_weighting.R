# Validation of time-weighted metrics (reviewer revision, R2.2 / R1.10)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj), then:
#   source("validation/validate_time_weighting.R")
#
# Part A: telemetry_validation.csv (4 laps x 20 samples; each lap uniformly
#         sampled at its own interval: 500, 1000, 1500, 2000 ms)
#         -> time-weighted results must equal the arithmetic means of v1.0.0.
# Part B: telemetry_validation_uneven.csv
#         Lap 1 = irregular sampling, Lap 2 = same values at uniform 500 ms.
#         -> results must equal hand-calculated expected values.
#
# Tests the functions in telemetry_core.R, the same code the apps use.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

tol <- 1e-6

# -----------------------------------------------------------------------------
# Result collection
# -----------------------------------------------------------------------------
results <- tibble()
check <- function(part, item, expected, calculated) {
  results <<- bind_rows(
    results,
    tibble(
      part = part,
      check = item,
      expected = expected,
      calculated = calculated,
      error = calculated - expected,
      pass = !is.na(calculated) & abs(calculated - expected) < tol
    )
  )
}

# Old (v1.0.0) per-sample formulas, used as the reference in Part A
legacy_lap_stats <- function(lap_data, thr = 0.99, brk = 0.01) {
  lap_data |>
    summarise(
      `Full Throttle (%)` = sum(gas > thr, na.rm = TRUE) / n() * 100,
      `Braking (%)` = sum(brake > brk, na.rm = TRUE) / n() * 100,
      `Brake/Throttle Overlap (%)` = sum(gas > thr & brake > brk, na.rm = TRUE) / n() * 100,
      `Avg Speed (km/h)` = mean(speedKmh, na.rm = TRUE),
      `ABS Active (%)` = sum(isAbsInAction == 1, na.rm = TRUE) / n() * 100,
      `TC Active (%)` = sum(isTcInAction == 1, na.rm = TRUE) / n() * 100
    )
}

weighted_metrics <- c(
  "Full Throttle (%)", "Braking (%)", "Brake/Throttle Overlap (%)",
  "Avg Speed (km/h)", "ABS Active (%)", "TC Active (%)"
)

# Corner summary table for a corner from entry to exit (one row per lap)
corner_summary <- function(df, entry, exit) {
  corner_summary_table(df |> mutate(lap_label = as.character(lapCount)), entry, exit)
}

# =============================================================================
# PART A: original validation CSV (uniform sampling)
# =============================================================================
val <- read_csv(file.path(vdir, "telemetry_validation.csv"), show_col_types = FALSE)

dt_by_lap <- val |>
  group_by(lapCount) |>
  summarise(dt = list(unique(diff(lapTime))), .groups = "drop")
cat("Part A sampling intervals per lap (ms):\n")
walk2(dt_by_lap$lapCount, dt_by_lap$dt, \(l, d) cat("  Lap", l, ":", d, "\n"))

for (lap in sort(unique(val$lapCount))) {
  lap_data <- val |> filter(lapCount == lap)
  new <- calculate_lap_stats(val, lap)
  old <- legacy_lap_stats(lap_data)
  for (m in weighted_metrics) {
    check("A uniform", paste0("Lap ", lap, " | ", m), old[[m]], new[[m]])
  }
  # single-lap map subtitle: time_pct(cond, lap_time_weights(lap_data))
  check(
    "A uniform", paste0("Lap ", lap, " | Map: full throttle %"),
    old[["Full Throttle (%)"]],
    time_pct(lap_data$gas > 0.99, lap_time_weights(lap_data))
  )
}

# Corner avg speed over a sub-section of each lap: new vs plain mean
entry_a <- 0.2
exit_a <- 0.6
corner_new <- corner_summary(val, entry_a, exit_a)
corner_old <- val |>
  filter(carPositionNormalized >= entry_a, carPositionNormalized <= exit_a) |>
  group_by(lap_label = as.character(lapCount)) |>
  summarise(avg = mean(speedKmh, na.rm = TRUE), .groups = "drop")
corner_cmp <- inner_join(corner_new, corner_old, by = "lap_label")
for (i in seq_len(nrow(corner_cmp))) {
  check(
    "A uniform", paste0("Lap ", corner_cmp$lap_label[i], " | Corner avg speed"),
    corner_cmp$avg[i], corner_cmp$avg_speed_kmh[i]
  )
}

# =============================================================================
# PART B: uneven sampling CSV, hand-calculated expected values
# =============================================================================
# Lap 1 lapTime (ms): 100, 200, 300, 400, 1400, 2400, 3400, 3500
# Weights: 100, 100, 100, 550, 1000, 1000, 550, 100  (sum = 3500)
#   speed  : 200, 200, 200, 150, 100, 100, 150, 200
#   gas = 1 on samples 1-4 and 8, brake > 0.01 on samples 4-6,
#   ABS on sample 5, TC on sample 2
# Avg speed     = (3*200*100 + 2*150*550 + 2*100*1000 + 200*100) / 3500 = 445000 / 3500
# Full throttle = (300 + 550 + 100) / 3500
# Braking       = (550 + 2000) / 3500
# Overlap       = 550 / 3500   (sample 4)
# ABS           = 1000 / 3500, TC = 100 / 3500
# Lap 2: same values, uniform 500 ms -> plain means (8 samples)
uneven <- read_csv(file.path(vdir, "telemetry_validation_uneven.csv"), show_col_types = FALSE)

expected_b <- tribble(
  ~lap, ~metric,                       ~expected,
  1,    "Avg Speed (km/h)",            445000 / 3500,
  1,    "Full Throttle (%)",           950 / 3500 * 100,
  1,    "Braking (%)",                 2550 / 3500 * 100,
  1,    "Brake/Throttle Overlap (%)",  550 / 3500 * 100,
  1,    "ABS Active (%)",              1000 / 3500 * 100,
  1,    "TC Active (%)",               100 / 3500 * 100,
  1,    "Lap Time (s)",                3.5,
  1,    "Max Speed (km/h)",            200,
  1,    "Min Speed (km/h)",            100,
  1,    "Max Lateral G",               1.0,
  1,    "Max Accel G",                 0.5,
  1,    "Max Brake G",                 -1.2,
  2,    "Avg Speed (km/h)",            1300 / 8,
  2,    "Full Throttle (%)",           5 / 8 * 100,
  2,    "Braking (%)",                 3 / 8 * 100,
  2,    "Brake/Throttle Overlap (%)",  1 / 8 * 100,
  2,    "ABS Active (%)",              1 / 8 * 100,
  2,    "TC Active (%)",               1 / 8 * 100,
  2,    "Lap Time (s)",                4.0
)

stats_b <- map(c(1, 2), \(l) calculate_lap_stats(uneven, l))
pwalk(expected_b, \(lap, metric, expected) {
  check("B uneven", paste0("Lap ", lap, " | ", metric), expected, stats_b[[lap]][[metric]])
})

lap1 <- uneven |> filter(lapCount == 1)
check("B uneven", "Lap 1 | Map: full throttle %", 950 / 3500 * 100,
      time_pct(lap1$gas > 0.99, lap_time_weights(lap1)))
check("B uneven", "Lap 1 | Map: overlap %", 550 / 3500 * 100,
      time_pct(lap1$gas > 0.99 & lap1$brake > 0.01, lap_time_weights(lap1)))

# Corner spanning all samples (0.1 to 0.8)
corner_b <- corner_summary(uneven, 0.1, 0.8)
check("B uneven", "Lap 1 | Corner avg speed", 445000 / 3500,
      corner_b$avg_speed_kmh[corner_b$lap_label == "1"])
check("B uneven", "Lap 1 | Corner time (s)", 3.4,
      corner_b$corner_time_s[corner_b$lap_label == "1"])
check("B uneven", "Lap 2 | Corner avg speed", 1300 / 8,
      corner_b$avg_speed_kmh[corner_b$lap_label == "2"])
check("B uneven", "Lap 2 | Corner time (s)", 3.5,
      corner_b$corner_time_s[corner_b$lap_label == "2"])

# For information: what the old arithmetic mean gives on uneven Lap 1
old_b <- legacy_lap_stats(lap1)

# =============================================================================
# REPORT
# =============================================================================
options(pillar.sigfig = 7)
cat("\n")
print(results |> mutate(across(c(expected, calculated, error), \(x) round(x, 6))), n = Inf, width = Inf)

cat("\nOld arithmetic-mean results on uneven Lap 1 (for comparison only):\n")
print(old_b |> mutate(across(everything(), \(x) round(x, 3))), width = Inf)

n_fail <- sum(!results$pass)
cat("\n", nrow(results) - n_fail, " / ", nrow(results), " checks passed\n", sep = "")
if (n_fail > 0) {
  cat("FAILED:\n")
  print(results |> filter(!pass), n = Inf, width = Inf)
} else {
  cat("ALL CHECKS PASSED\n")
}

write_csv(results, file.path(vdir, "validation_time_weighting_results.csv"))
cat("Results saved to validation/validation_time_weighting_results.csv\n")
