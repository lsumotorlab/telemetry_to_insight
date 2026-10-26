# Validation of the track-position check (reviewer revision, R1.8)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj), then:
#   source("validation/validate_position_check.R")
#
# position_check() (telemetry_core.R) compares a lap's normalized track
# position with the distance driven, calculated from the car coordinates, and
# reports the position coverage and the largest deviation (lap fraction).
# lap_quality_tbl() adds both to the data-quality report and flags laps with
# coverage < 95% or deviation > 5% of a lap.
#
# Test lap: circular track (radius 300 m), 60 s, 100 Hz, speed varying +/- 30%
# along the lap, so position is not proportional to time.
#   A exact position (= distance fraction)        -> deviation ~0, coverage ~100%
#   B start offset 0.0009 (timing line as in Assetto Corsa), wraps at the line
#                                                  -> as A
#   C distorted position u + a sin(2 pi u) / (2 pi), a = 0.5
#                                                  -> deviation = a / (2 pi) = 7.96%, flagged
#   D incomplete lap (first 80% of the samples)   -> coverage 80%, flagged
#   E lap with coordinates missing (all NA)       -> not checked (NA), no flag

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

results <- tibble()
check <- function(case, item, expected, calculated, pass) {
  results <<- bind_rows(results, tibble(case = case, check = item, expected = expected,
                                        calculated = calculated, pass = pass))
}

R_m <- 300
T_ms <- 60000
t <- seq(0, T_ms - 10, by = 10)
u <- t / T_ms + 0.3 / (2 * pi) * sin(2 * pi * t / T_ms)   # exact distance fraction
make_lap <- function(lap, pos, keep = rep(TRUE, length(t)), coords = TRUE) {
  tibble(lapCount = lap, lapTime = t, carPositionNormalized = pos,
         carCoordinatesX = if (coords) R_m * cos(2 * pi * u) else NA_real_,
         carCoordinatesY = 0,
         carCoordinatesZ = if (coords) R_m * sin(2 * pi * u) else NA_real_,
         speedKmh = 100)[keep, ]
}

a <- 0.5
laps <- bind_rows(
  make_lap(1, u),
  make_lap(2, (u + 0.0009) %% 1),
  make_lap(3, u + a * sin(2 * pi * u) / (2 * pi)),
  make_lap(4, u, keep = t < 0.8 * T_ms),
  make_lap(5, u, coords = FALSE)
)
q <- lap_quality_tbl(laps)
val <- \(lap, col) q[[col]][q$lapCount == lap]
has_flag <- \(lap, txt) grepl(txt, val(lap, "flags"), fixed = TRUE)

for (lap in 1:2) {
  nm <- c("A exact position", "B start offset 0.0009")[lap]
  check(nm, "deviation (% lap)", 0, val(lap, "position_vs_distance_max_dev_pct"),
        val(lap, "position_vs_distance_max_dev_pct") < 0.01)
  check(nm, "coverage (% lap)", 100, val(lap, "position_coverage_pct"),
        abs(val(lap, "position_coverage_pct") - 100) < 0.1)
  check(nm, "no position flag", 0, has_flag(lap, "position") * 1, !has_flag(lap, "position"))
}

check("C distorted position", "deviation (% lap)", a / (2 * pi) * 100,
      val(3, "position_vs_distance_max_dev_pct"),
      abs(val(3, "position_vs_distance_max_dev_pct") - a / (2 * pi) * 100) < 0.01)
check("C distorted position", "flagged", 1, has_flag(3, "differs from distance") * 1,
      has_flag(3, "differs from distance"))

cov_d <- max(u[t < 0.8 * T_ms]) * 100
check("D incomplete lap", "coverage (% lap)", cov_d, val(4, "position_coverage_pct"),
      abs(val(4, "position_coverage_pct") - cov_d) < 1e-6)
check("D incomplete lap", "flagged", 1, has_flag(4, "covers only") * 1, has_flag(4, "covers only"))

check("E coordinates missing", "deviation not calculated", NA, val(5, "position_vs_distance_max_dev_pct"),
      is.na(val(5, "position_vs_distance_max_dev_pct")))
check("E coordinates missing", "no position flag", 0, has_flag(5, "position") * 1, !has_flag(5, "position"))

options(pillar.sigfig = 5)
print(results, n = Inf, width = Inf)
cat("\n", sum(results$pass), "/", nrow(results), "checks passed\n")
write_csv(results, file.path(vdir, "validation_position_check_results.csv"))
cat("Results saved to validation/validation_position_check_results.csv\n")
