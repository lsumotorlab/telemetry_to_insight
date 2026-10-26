# Validation of sector timing and composite-lap retiming (reviewer revision, R2.3 / R2.2)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj), then:
#   source("validation/validate_composite_lap.R")
#
# Test laps have a known, non-constant speed. Position over time is an exact
# formula, so true boundary-crossing times and sector times are known:
#   u(t) = t/T + c/(2*pi) * (sin(2*pi*t/T + phi) - sin(phi))
# (u = normalized track position, T = lap time, speed varies by +/- c*100 %)
#
# Part C: sector times from interpolated boundary crossings at 10 / 60 / 100 Hz
#         and with irregular sampling, vs. exact values (both scripts that
#         compute sector times). The old v1.0.0 method is shown for comparison.
# Part D: composite reference lap built from the test laps. Checks that the
#         fastest lap is chosen for each sector, that the composite lap time
#         equals the sum of the best sectors, and that the position-time
#         relationship of each source lap is preserved inside every sector.
#
# Tests the functions in telemetry_core.R, the same code the apps use.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

tol_ms <- 1      # timing tolerance (ms)
tol_kmh <- 0.1   # speed tolerance (km/h)

# -----------------------------------------------------------------------------
# Result collection
# -----------------------------------------------------------------------------
results <- tibble()
check <- function(part, item, expected, calculated, tol) {
  results <<- bind_rows(
    results,
    tibble(
      part = part,
      check = item,
      expected = expected,
      calculated = calculated,
      error = calculated - expected,
      tolerance = tol,
      pass = !is.na(calculated) & abs(calculated - expected) <= tol
    )
  )
}

# -----------------------------------------------------------------------------
# Test laps with exact position-time functions
# -----------------------------------------------------------------------------
track_m <- 2000
laps_def <- tribble(
  ~lap, ~T_ms,  ~c,   ~phi,
  1,    60000,  0.30, 0,
  2,    60500,  0.30, pi / 2,
  3,    61000,  0.35, pi
)

u_of_t <- function(t, T, c, phi) t / T + c / (2 * pi) * (sin(2 * pi * t / T + phi) - sin(phi))
speed_kmh_of_t <- function(t, T, c, phi) track_m / T * (1 + c * cos(2 * pi * t / T + phi)) * 3600

# Exact time at which lap L reaches unwrapped position u (0..1)
t_of_u <- function(u, L) {
  p <- laps_def[laps_def$lap == L, ]
  vapply(u, function(uu) {
    if (uu <= 0) return(0)
    if (uu >= 1) return(p$T_ms)
    uniroot(\(t) u_of_t(t, p$T_ms, p$c, p$phi) - uu,
            c(0, p$T_ms), tol = 1e-10)$root
  }, numeric(1))
}

# Exact sector time of lap L
exact_seg_time <- function(L, start, end) {
  if (start < end) {
    diff(t_of_u(c(start, end), L))
  } else {
    T <- laps_def$T_ms[laps_def$lap == L]
    (T - t_of_u(start, L)) + t_of_u(end, L)
  }
}

# Sampled telemetry for all laps, given a function returning sample times
make_laps <- function(sample_times) {
  pmap_dfr(laps_def, function(lap, T_ms, c, phi) {
    t <- sample_times(T_ms)
    t <- t[t < T_ms]
    u <- u_of_t(t, T_ms, c, phi)
    tibble(
      lapCount = lap,
      lapTime = t,
      carPositionNormalized = u %% 1,
      carCoordinatesX = 318.31 * cos(2 * pi * u),
      carCoordinatesZ = 318.31 * sin(2 * pi * u),
      speedKmh = speed_kmh_of_t(t, T_ms, c, phi),
      gear = 3L
    )
  })
}

schemes <- list(
  "10 Hz"     = \(T) seq(0, T, by = 100),
  "60 Hz"     = \(T) seq(0, T, by = 1000 / 60),
  "100 Hz"    = \(T) seq(0, T, by = 10),
  "irregular" = \(T) {
    set.seed(42)
    c(0, cumsum(runif(ceiling(T / 5), min = 5, max = 60)))  # 5-60 ms gaps
  }
)

bounds <- tibble(
  seg   = 1:4,
  start = c(0.15, 0.45, 0.70, 0.90),
  end   = c(0.45, 0.70, 0.90, 0.15)   # sector 4 crosses the start/finish line
)

exact_tbl <- expand_grid(lap = laps_def$lap, seg = bounds$seg) |>
  left_join(bounds, by = "seg") |>
  mutate(exact_ms = pmap_dbl(list(lap, start, end), exact_seg_time))

# v1.0.0 sector time: sum of whole sample intervals of samples inside the sector
old_seg_time <- function(lap_df, start, end) {
  d <- lap_df |>
    arrange(carPositionNormalized, lapTime) |>
    mutate(dt = pmax(0, lapTime - lag(lapTime, default = first(lapTime))))
  inside <- if (start < end) {
    d$carPositionNormalized >= start & d$carPositionNormalized < end
  } else {
    d$carPositionNormalized >= start | d$carPositionNormalized < end
  }
  sum(d$dt[inside])
}

# =============================================================================
# PART C: sector times at different sampling rates
# =============================================================================
sector_summary <- tibble()

for (sch in names(schemes)) {
  tel <- make_laps(schemes[[sch]])

  st <- exact_tbl |>
    mutate(
      builder_ms = pmap_dbl(list(lap, start, end), \(L, s, e) {
        d <- tel |> filter(lapCount == L)
        seg_duration(d$carPositionNormalized, d$lapTime, s, e)
      }),
      compare_ms = map2_dbl(lap, seg, \(L, k) {
        d <- tel |> filter(lapCount == L)
        compute_seg_times_real(d, bounds)[[as.character(k)]]
      }),
      old_ms = pmap_dbl(list(lap, start, end), \(L, s, e) {
        old_seg_time(tel |> filter(lapCount == L), s, e)
      })
    )

  check("C sectors", paste0(sch, " | max |error| sector time, composite builder (ms)"),
        0, max(abs(st$builder_ms - st$exact_ms)), tol_ms)
  check("C sectors", paste0(sch, " | max |error| sector time, lap comparison (ms)"),
        0, max(abs(st$compare_ms - st$exact_ms)), tol_ms)
  check("C sectors", paste0(sch, " | sum of sectors - lap time, lap 1 (ms)"),
        0, sum(st$builder_ms[st$lap == 1]) - laps_def$T_ms[1], tol_ms)

  sector_summary <- bind_rows(sector_summary, tibble(
    sampling = sch,
    new_max_abs_error_ms = max(abs(st$builder_ms - st$exact_ms)),
    old_max_abs_error_ms = max(abs(st$old_ms - st$exact_ms))
  ))
}

# =============================================================================
# PART D: composite reference lap (built from 10 Hz and 100 Hz test laps)
# =============================================================================
# Expected composite from the exact functions: fastest lap per sector,
# sectors chained, time re-referenced to the start/finish line
best <- exact_tbl |>
  group_by(seg) |>
  slice_min(exact_ms, n = 1) |>
  ungroup() |>
  arrange(seg) |>
  mutate(offset_ms = lag(cumsum(exact_ms), default = 0))
total_exact <- sum(best$exact_ms)
wrap <- best |> filter(start > end)
t0_exact <- wrap$offset_ms +
  (laps_def$T_ms[laps_def$lap == wrap$lap] - t_of_u(wrap$start, wrap$lap))

# Unwrapped source-lap position of a composite point (sector-relative)
src_u <- function(pos, start, end) ifelse(start > end & pos < start, pos + 1, pos)

expected_time <- function(pos, seg) {
  map2_dbl(pos, seg, \(p, k) {
    b <- best[best$seg == k, ]
    if (p >= 1) return(total_exact)
    u <- src_u(p, b$start, b$end)
    tau <- if (u < 1) {
      t_of_u(u, b$lap) - t_of_u(b$start, b$lap)
    } else {
      (laps_def$T_ms[laps_def$lap == b$lap] - t_of_u(b$start, b$lap)) + t_of_u(u - 1, b$lap)
    }
    (b$offset_ms + tau - t0_exact) %% total_exact
  })
}

# v1.0.0 retiming for comparison: sector time spread in proportion to distance
# (constant speed within the sector; distance is proportional to position here)
old_expected_time <- function(pos, seg) {
  map2_dbl(pos, seg, \(p, k) {
    b <- best[best$seg == k, ]
    if (p >= 1) return(total_exact)
    len <- if (b$start < b$end) b$end - b$start else 1 - b$start + b$end
    frac <- (src_u(p, b$start, b$end) - b$start) / len
    (b$offset_ms + frac * b$exact_ms - t0_exact) %% total_exact
  })
}

retime_summary <- tibble()

for (sch in c("10 Hz", "100 Hz")) {
  tel <- make_laps(schemes[[sch]])
  built <- build_synthetic_lap_from_bounds(tel, bounds)
  comp <- resample_and_retime_synthetic_lap(
    telemetry = tel, segments = built$segments, n_bins = 2000
  )

  # Fastest lap chosen for each sector
  chosen <- built$segments |> arrange(seg)
  for (k in bounds$seg) {
    check("D composite", paste0(sch, " | sector ", k, " source lap"),
          best$lap[best$seg == k], chosen$source_lap[chosen$seg == k], 0)
  }

  # Composite lap time and start/finish reference
  check("D composite", paste0(sch, " | composite lap time (ms)"),
        total_exact, max(comp$lapTime), tol_ms)
  first_row <- comp |> slice_min(lapTime, n = 1, with_ties = FALSE)
  check("D composite", paste0(sch, " | position at lapTime 0"),
        0, first_row$carPositionNormalized, 1e-9)

  # Position-time relationship inside sectors
  comp <- comp |>
    mutate(
      expected_ms = expected_time(carPositionNormalized, seg),
      old_ms = old_expected_time(carPositionNormalized, seg)
    )
  # error measured around the lap (0 and the full lap time are the same instant)
  circ_err <- \(a, b) ((a - b + total_exact / 2) %% total_exact) - total_exact / 2
  comp <- comp |>
    mutate(err_ms = circ_err(lapTime, expected_ms), old_err_ms = circ_err(old_ms, expected_ms))
  check("D composite", paste0(sch, " | max |error| composite lapTime vs source laps (ms)"),
        0, max(abs(comp$err_ms)), tol_ms)

  # Channel values: speed at each composite point vs exact source-lap speed
  comp <- comp |>
    mutate(expected_kmh = pmap_dbl(list(carPositionNormalized, seg), \(p, k) {
      b <- best[best$seg == k, ]
      u <- src_u(p, b$start, b$end)
      u <- if (u >= 1) u - 1 else u
      pr <- laps_def[laps_def$lap == b$lap, ]
      speed_kmh_of_t(t_of_u(u, b$lap), pr$T_ms, pr$c, pr$phi)
    }))
  check("D composite", paste0(sch, " | max |error| composite speed (km/h)"),
        0, max(abs(comp$speedKmh - comp$expected_kmh)), tol_kmh)

  # Lap comparison script reads the sector times back from the composite lap
  seg_back <- compute_seg_times_synthetic(comp |> mutate(lapCount = 99))
  check("D composite", paste0(sch, " | max |error| sector times read by lap comparison (ms)"),
        0, max(abs(seg_back$times[as.character(best$seg)] - best$exact_ms)), tol_ms)

  retime_summary <- bind_rows(retime_summary, tibble(
    sampling = sch,
    new_max_abs_error_ms = max(abs(comp$err_ms)),
    old_method_max_abs_error_ms = max(abs(comp$old_err_ms))
  ))
}

# =============================================================================
# REPORT
# =============================================================================
options(pillar.sigfig = 7)
cat("\n")
print(results |> mutate(across(c(expected, calculated, error), \(x) round(x, 4))),
      n = Inf, width = Inf)

cat("\nSector time error vs exact (ms): new interpolated crossings vs old v1.0.0 method\n")
print(sector_summary |> mutate(across(where(is.numeric), \(x) round(x, 3))), width = Inf)

cat("\nComposite-lap position-time error vs source laps (ms): new retiming vs old distance-proportional\n")
print(retime_summary |> mutate(across(where(is.numeric), \(x) round(x, 3))), width = Inf)

n_fail <- sum(!results$pass)
cat("\n", nrow(results) - n_fail, " / ", nrow(results), " checks passed\n", sep = "")
if (n_fail > 0) {
  cat("FAILED:\n")
  print(results |> filter(!pass), n = Inf, width = Inf)
} else {
  cat("ALL CHECKS PASSED\n")
}

write_csv(results, file.path(vdir, "validation_composite_lap_results.csv"))
write_csv(bind_rows(
  sector_summary |> mutate(test = "sector times"),
  retime_summary |> rename(old_max_abs_error_ms = old_method_max_abs_error_ms) |>
    mutate(test = "composite position-time")
), file.path(vdir, "validation_composite_lap_old_vs_new.csv"))
cat("Results saved to validation/validation_composite_lap_results.csv and",
    "validation/validation_composite_lap_old_vs_new.csv\n")
