# Generator for the stress-test validation set (reviewer revision, R2.2 / R1.6)
# ===================================================================
# Writes:
#   validation/telemetry_validation_stress.csv           telemetry, 9 laps
#   validation/telemetry_validation_stress_expected.csv  true values + tolerances
#
# Run from the project folder (open telemetry_to_insight.Rproj):
#   source("validation/make_validation_stress.R")
# The CSVs are committed, so this only needs re-running if the design changes.
# Random elements use a fixed seed, so the output is reproducible.
#
# Every lap is built from exact formulas, so the true value of each metric is
# known without using any toolkit code:
#   position  u(t) = t/T + c/(2*pi) * (sin(2*pi*t/T + phi) - sin(phi))
#   speed     v(t) = L/T * (1 + c * cos(2*pi*t/T + phi))      (L = 2000 m)
#   throttle / brake / ABS / TC switch on and off at known fractions of lap time
# The timing line is at normalized position 0.0009 (as in Assetto Corsa), so
# carPositionNormalized = (u + 0.0009) mod 1 and lapTime = 0 at the line.
#
# One lap per recording problem:
#   1 clean 10 Hz | 2 clean 60 Hz | 3 clean 100 Hz | 4 irregular 5-60 ms
#   5 60 Hz with 2 s of dropped samples | 6 60 Hz with 5% NA in speed/gas/brake
#   7 60 Hz with 5% NA lapTime | 8 60 Hz, brake channel missing
#   9 irregular 50-150 ms (~10 Hz) + 2 s dropped + 5% NA in speed/gas/brake/lapTime
#
# True values:
#   Lap time = T; Avg speed = L / T; Max/Min speed = (L/T)(1 +/- c)
#   Full throttle 72 %, Braking 15 %, Overlap 1 %, ABS 2 %, TC 1 % of lap time
#   Corner (positions 0.30-0.45) and sector times from the exact position-time
#   function; corner average speed = corner distance / corner time.
# Tolerances (worst case for the actual sampling of each lap):
#   times (lap, corner, sectors): 1 ms + linear-interpolation error bound
#     c * pi * sum(d^2) / ((1 - c) * T), d = sampling interval at each crossing
#   max/min/entry/exit speed: 0.1 km/h (corner min speed: + |dv/dt| x interval
#     when the minimum lies at the corner entry or exit)
#   % metrics: [sum over on/off switches of half the sampling interval
#     containing the switch + 2 x (first + last interval)] / T
#   corner avg speed: (sampling intervals at entry + exit) x max |v - corner mean|
#     / corner time
#   missing channel: expected NA

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
vdir <- file.path(root, "validation")

set.seed(2026)

track_m  <- 2000
line_pos <- 0.0009
radius   <- track_m / (2 * pi)

# ---- lap definitions ---------------------------------------------------------
laps <- tribble(
  ~lap, ~condition,                                       ~T_ms, ~c,   ~phi,
  1,    "clean 10 Hz",                                    60000, 0.30, 0.0,
  2,    "clean 60 Hz",                                    60300, 0.30, 0.5,
  3,    "clean 100 Hz",                                   60600, 0.30, 1.0,
  4,    "irregular intervals 5-60 ms",                    60900, 0.30, 1.5,
  5,    "60 Hz, 2 s of dropped samples",                  61200, 0.30, 2.0,
  6,    "60 Hz, 5% NA in speed/gas/brake",                61500, 0.30, 2.5,
  7,    "60 Hz, 5% NA lapTime",                           61800, 0.30, 3.0,
  8,    "60 Hz, brake channel missing",                   62100, 0.30, 3.5,
  9,    "irregular 50-150 ms + 2 s dropped + 5% NA",      62400, 0.30, 4.0
)

# Switch points as fractions of lap time [start, end)
full_throttle_zones <- rbind(c(0.00, 0.18), c(0.25, 0.52), c(0.64, 0.83), c(0.92, 1.00))
brake_zones         <- rbind(c(0.20, 0.26), c(0.55, 0.60), c(0.85, 0.89))
overlap_zones       <- rbind(c(0.25, 0.26))
abs_zones           <- rbind(c(0.21, 0.23))
tc_zones            <- rbind(c(0.65, 0.66))
gap_zone            <- c(0.40, 0.40 + 2000 / 60000)   # ~2 s, inside a full-throttle zone

in_zones <- function(s, zones) {
  out <- rep(FALSE, length(s))
  for (i in seq_len(nrow(zones))) out <- out | (s >= zones[i, 1] & s < zones[i, 2])
  out
}
zone_share <- function(zones) sum(zones[, 2] - zones[, 1]) * 100
switch_points <- function(zones) {
  p <- sort(unique(c(zones)))
  p[p > 0 & p < 1]
}

u_of_t <- function(t, T, c, phi) t / T + c / (2 * pi) * (sin(2 * pi * t / T + phi) - sin(phi))
v_kmh  <- function(t, T, c, phi) track_m / T * (1 + c * cos(2 * pi * t / T + phi)) * 3600

t_of_u <- function(u, T, c, phi) {
  vapply(u, function(uu) {
    if (uu <= 0) return(0)
    if (uu >= 1) return(T)
    uniroot(\(t) u_of_t(t, T, c, phi) - uu, c(0, T), tol = 1e-10)$root
  }, numeric(1))
}

# ---- sample times per lap ----------------------------------------------------
regular_times <- function(T, dt) (0.37 * dt) + (0:ceiling(T / dt)) * dt
irregular_times <- function(T, lo, hi) {
  t <- cumsum(c(runif(1, 0, lo), runif(ceiling(T / lo), lo, hi)))
  t
}

sample_times <- function(lap, T) {
  t <- switch(as.character(lap),
    "1" = regular_times(T, 100),
    "2" = regular_times(T, 1000 / 60),
    "3" = regular_times(T, 10),
    "4" = irregular_times(T, 5, 60),
    "9" = irregular_times(T, 50, 150),
    regular_times(T, 1000 / 60)
  )
  t <- t[t < T]
  if (lap %in% c(5, 9)) {
    s <- t / T
    t <- t[!(s >= gap_zone[1] & s < gap_zone[2])]
  }
  t
}

# ---- build telemetry ---------------------------------------------------------
telemetry <- pmap_dfr(laps, function(lap, condition, T_ms, c, phi) {
  t <- sample_times(lap, T_ms)
  s <- t / T_ms
  u <- u_of_t(t, T_ms, c, phi)
  brake <- ifelse(in_zones(s, brake_zones), 0.8, 0)
  gas <- ifelse(in_zones(s, full_throttle_zones), 1, ifelse(brake > 0, 0, 0.4))
  df <- tibble(
    lapCount = lap,
    lapTime = t,
    carPositionNormalized = (u + line_pos) %% 1,
    carCoordinatesX = radius * cos(2 * pi * u),
    carCoordinatesY = 0,
    carCoordinatesZ = radius * sin(2 * pi * u),
    speedKmh = v_kmh(t, T_ms, c, phi),
    engineRPM = 5000,
    gear = 3L,
    gas = gas,
    brake = brake,
    steer = 0,
    isAbsInAction = as.integer(in_zones(s, abs_zones)),
    isTcInAction = as.integer(in_zones(s, tc_zones)),
    isInPit = 0L,
    accGHorizontal = 0,
    accGFrontal = 0
  )
  n <- nrow(df)
  if (lap %in% c(6, 9)) {
    df$speedKmh[sample(n, round(0.05 * n))] <- NA
    df$gas[sample(n, round(0.05 * n))] <- NA
    df$brake[sample(n, round(0.05 * n))] <- NA
  }
  if (lap %in% c(7, 9)) df$lapTime[sample(n, round(0.05 * n))] <- NA
  if (lap == 8) df$brake <- NA_real_
  df
})

# ---- tolerance helpers (from the sampled data) -------------------------------
# Interval between the valid samples either side of each time point (ms)
interval_at <- function(t_valid, tau) {
  vapply(tau, function(x) {
    before <- t_valid[t_valid <= x]
    after <- t_valid[t_valid > x]
    if (length(before) == 0 || length(after) == 0) return(NA_real_)
    min(after) - max(before)
  }, numeric(1))
}

pct_tolerance <- function(t_valid, T, zones) {
  t_valid <- sort(t_valid)
  sw <- switch_points(zones) * T
  d_sw <- interval_at(t_valid, sw)
  d_first <- t_valid[2] - t_valid[1]
  d_last <- t_valid[length(t_valid)] - t_valid[length(t_valid) - 1]
  (sum(d_sw / 2, na.rm = TRUE) + 2 * (d_first + d_last)) / T * 100
}

# ---- expected values ---------------------------------------------------------
corner_entry <- 0.30
corner_exit  <- 0.45
sector_bounds <- tibble(
  seg   = 1:4,
  start = c(0.15, 0.45, 0.70, 0.90),
  end   = c(0.45, 0.70, 0.90, 0.15)   # sector 4 crosses the start/finish line
)

expected <- pmap_dfr(laps, function(lap, condition, T_ms, c, phi) {
  d <- telemetry |> filter(lapCount == lap)
  v_mean <- track_m / T_ms * 3600

  # time tolerance (s): 1 ms + interpolation/extrapolation error bound
  t_pos <- sort(d$lapTime[is.finite(d$lapTime)])
  time_tol <- function(intervals_ms) {
    0.001 + c * pi * sum(intervals_ms^2) / ((1 - c) * T_ms) / 1000
  }
  dv_dt <- function(t) abs(track_m / T_ms * c * sin(2 * pi * t / T_ms + phi) * 2 * pi / T_ms * 3600)

  pct_row <- function(metric, zones, cols, value) {
    t_ok <- d$lapTime[is.finite(d$lapTime) & complete.cases(d[cols])]
    missing <- any(vapply(cols, \(cl) all(is.na(d[[cl]])), logical(1)))
    tibble(metric = metric,
           expected = if (missing) NA_real_ else value,
           tolerance = if (missing) NA_real_ else pct_tolerance(t_ok, T_ms, zones),
           basis = if (missing) "channel missing: expected NA" else "switch intervals")
  }

  # corner: positions -> distance from the timing line -> exact times
  u_e <- corner_entry - line_pos
  u_x <- corner_exit - line_pos
  t_e <- t_of_u(u_e, T_ms, c, phi)
  t_x <- t_of_u(u_x, T_ms, c, phi)
  corner_avg <- track_m * (u_x - u_e) / (t_x - t_e) * 3600
  grid_t <- seq(t_e, t_x, length.out = 20001)
  grid_v <- v_kmh(grid_t, T_ms, c, phi)
  t_sp <- d$lapTime[is.finite(d$lapTime) & is.finite(d$speedKmh)]
  d_e <- interval_at(sort(t_sp), t_e)
  d_x <- interval_at(sort(t_sp), t_x)
  corner_avg_tol <- (d_e + d_x) * max(abs(grid_v - corner_avg)) / (t_x - t_e)
  i_min <- which.min(grid_v)
  corner_min_tol <- 0.1 +
    (if (i_min == 1) d_e * dv_dt(t_e) else 0) +
    (if (i_min == length(grid_v)) d_x * dv_dt(t_x) else 0)
  corner_time_tol <- time_tol(c(interval_at(t_pos, t_e), interval_at(t_pos, t_x)))
  lap_time_tol <- time_tol(c(t_pos[2] - t_pos[1], t_pos[length(t_pos)] - t_pos[length(t_pos) - 1]))

  # sectors
  sec <- sector_bounds |>
    mutate(
      us = start - line_pos,
      ue = end - line_pos,
      expected = map2_dbl(us, ue, \(a, b) {
        if (a < b) diff(t_of_u(c(a, b), T_ms, c, phi))
        else (T_ms - t_of_u(a, T_ms, c, phi)) + t_of_u(b, T_ms, c, phi)
      }),
      t_start = map_dbl(us, \(a) t_of_u(a, T_ms, c, phi)),
      t_end = map_dbl(ue, \(b) t_of_u(b, T_ms, c, phi)),
      tolerance = map2_dbl(t_start, t_end, \(a, b) time_tol(interval_at(t_pos, c(a, b))))
    )

  bind_rows(
    tibble(metric = "Lap Time (s)", expected = T_ms / 1000, tolerance = lap_time_tol,
           basis = "1 ms + interpolation bound"),
    tibble(metric = "Avg Speed (km/h)", expected = v_mean, tolerance = 0.1, basis = "0.1 km/h"),
    tibble(metric = "Max Speed (km/h)", expected = v_mean * (1 + c), tolerance = 0.1, basis = "0.1 km/h"),
    tibble(metric = "Min Speed (km/h)", expected = v_mean * (1 - c), tolerance = 0.1, basis = "0.1 km/h"),
    pct_row("Full Throttle (%)", full_throttle_zones, "gas", zone_share(full_throttle_zones)),
    pct_row("Braking (%)", brake_zones, "brake", zone_share(brake_zones)),
    pct_row("Brake/Throttle Overlap (%)", rbind(full_throttle_zones, brake_zones), c("gas", "brake"),
            zone_share(overlap_zones)),
    pct_row("ABS Active (%)", abs_zones, "isAbsInAction", zone_share(abs_zones)),
    pct_row("TC Active (%)", tc_zones, "isTcInAction", zone_share(tc_zones)),
    tibble(metric = "Corner Time (s)", expected = (t_x - t_e) / 1000, tolerance = corner_time_tol,
           basis = "1 ms + interpolation bound"),
    tibble(metric = "Corner Entry Speed (km/h)", expected = v_kmh(t_e, T_ms, c, phi), tolerance = 0.1, basis = "0.1 km/h"),
    tibble(metric = "Corner Exit Speed (km/h)", expected = v_kmh(t_x, T_ms, c, phi), tolerance = 0.1, basis = "0.1 km/h"),
    tibble(metric = "Corner Min Speed (km/h)", expected = min(grid_v), tolerance = corner_min_tol,
           basis = "0.1 km/h (+ boundary interval if minimum at entry/exit)"),
    tibble(metric = "Corner Avg Speed (km/h)", expected = corner_avg, tolerance = corner_avg_tol,
           basis = "boundary intervals"),
    tibble(metric = paste0("Sector ", sec$seg, " Time (s)"), expected = sec$expected / 1000,
           tolerance = sec$tolerance, basis = "1 ms + interpolation bound")
  ) |>
    mutate(lap = lap, condition = condition, .before = 1)
})

write_csv(telemetry, file.path(vdir, "telemetry_validation_stress.csv"), na = "NA")
write_csv(expected, file.path(vdir, "telemetry_validation_stress_expected.csv"), na = "NA")

cat("Wrote", nrow(telemetry), "telemetry rows (", n_distinct(telemetry$lapCount), "laps ) and",
    nrow(expected), "expected values to validation/\n")
print(telemetry |> group_by(lapCount) |>
        summarise(rows = n(),
                  median_dt_ms = median(diff(lapTime[is.finite(lapTime)])),
                  max_dt_ms = max(diff(lapTime[is.finite(lapTime)])),
                  na_speed = sum(is.na(speedKmh)), na_lapTime = sum(is.na(lapTime)),
                  .groups = "drop"))
