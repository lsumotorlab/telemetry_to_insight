# Corner across the start/finish line (reviewer revision, R1.10)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj):
#   source("validation/validate_corner_wrap.R")
#
# A synthetic lap with a known speed profile, v(u) = 160 - 80 cos(2 pi u) km/h
# (slowest, 80 km/h, at the start/finish line), on a 2,000-m track, sampled at
# 100 Hz. Exact times t(u) are obtained by integrating 1/v over a fine grid.
# corner_summary_table() is checked against the true values for
#   (1) a corner that crosses the start/finish line (entry 0.95, exit 0.05),
#   (2) the same corner after corner_unwrap(), as the corner analysis app uses it,
#   (3) a corner that does not cross the line (entry 0.30, exit 0.45),
# and the corner time is compared with the sector time (seg_duration).

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))

track_m <- 2000
v_kmh   <- \(u) 160 - 80 * cos(2 * pi * u)

# exact time (ms) at unwrapped position u, from a fine integration grid
u_fine <- seq(0, 1, length.out = 200001)
t_fine <- c(0, cumsum(diff(u_fine) * track_m /
                        (v_kmh((head(u_fine, -1) + tail(u_fine, -1)) / 2) / 3.6))) * 1000
t_true <- \(u) approx(u_fine, t_fine, xout = u)$y
lap_ms <- t_true(1)

# 100 Hz samples, first one 4 ms after the line
t_s  <- seq(4, lap_ms, by = 10)
u_s  <- approx(t_fine, u_fine, xout = t_s)$y
lap  <- tibble(lap_label = "lap", lapTime = t_s, carPositionNormalized = u_s %% 1,
               speedKmh = v_kmh(u_s))

# true corner values: time, entry/exit speed, min speed, average speed (distance / time)
truth <- function(entry, exit) {
  span   <- if (entry > exit) (1 - entry) + exit else exit - entry
  time_s <- if (entry > exit) (lap_ms - t_true(entry) + t_true(exit)) / 1000
            else (t_true(exit) - t_true(entry)) / 1000
  u_in   <- seq(entry, entry + span, length.out = 10001)
  c(corner_time_s   = time_s,
    entry_speed_kmh = v_kmh(entry),
    min_speed_kmh   = min(v_kmh(u_in)),
    avg_speed_kmh   = span * track_m / time_s * 3.6,
    exit_speed_kmh  = v_kmh(exit))
}

tol <- c(corner_time_s = 0.001, entry_speed_kmh = 0.1, min_speed_kmh = 0.1,
         avg_speed_kmh = 0.1, exit_speed_kmh = 0.1)

check <- function(case, tbl, entry, exit) {
  tr <- truth(entry, exit)
  tibble(case = case, metric = names(tr), expected = unname(tr),
         calculated = unlist(tbl[1, names(tr)])) |>
    mutate(error = calculated - expected,
           pass  = abs(error) <= tol[metric])
}

res <- bind_rows(
  check("crosses line",
        corner_summary_table(lap, 0.95, 0.05), 0.95, 0.05),
  check("crosses line, app path (corner_unwrap)",
        corner_summary_table(corner_unwrap(lap, 0.95, 0.05), 0.95, 1.05), 0.95, 0.05),
  check("does not cross line",
        corner_summary_table(lap, 0.30, 0.45), 0.30, 0.45)
)

seg <- tibble(
  case     = c("crosses line", "does not cross line"),
  metric   = "corner_time_s vs seg_duration",
  expected = c(seg_duration(lap$carPositionNormalized, lap$lapTime, 0.95, 0.05),
               seg_duration(lap$carPositionNormalized, lap$lapTime, 0.30, 0.45)) / 1000,
  calculated = res |> filter(metric == "corner_time_s", case %in% c("crosses line", "does not cross line")) |>
    pull(calculated)
) |>
  mutate(error = calculated - expected, pass = abs(error) <= 1e-6)

res <- bind_rows(res, seg)
print(res, n = Inf, width = Inf)
cat(sprintf("\n%d / %d checks passed\n", sum(res$pass), nrow(res)))
