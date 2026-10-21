# Check which acceleration column holds lateral acceleration (reviewer revision)
# ===================================================================
# Some loggers label the acceleration axes differently from their physical
# meaning. In a corner, lateral acceleration = speed^2 / radius and the
# steering angle is roughly proportional to 1 / radius, so the lateral column
# should follow steer x speed^2 and mirror the steering trace corner by corner.
# In the Assetto Corsa session used in the paper, accGVertical holds lateral
# acceleration and accGHorizontal does not (r = -0.97 vs -0.03 on the
# race-pace laps). The harmonizer runs the same check (check_lateral_channel()
# in telemetry_core.R).
#
# The data file is NOT part of the repository; set its path (and optionally
# the lap to plot) before running, from the project folder:
#   real_file <- "C:/path/to/telemetry.csv"
#   lap_to_plot <- 13          # optional; default = a typical lap (median number of samples)
#   source("validation/check_acceleration_axes.R")
#
# Output: the correlation of each acceleration column with steer x speed^2
# (all laps) and a plot of steering and the acceleration columns for one lap.
# Column names can be changed below for other loggers.

suppressPackageStartupMessages({
  library(tidyverse)
})

if (!exists("real_file") || !file.exists(real_file)) {
  stop("Set real_file to the path of a raw telemetry CSV, e.g.\n",
       '  real_file <- "C:/path/to/telemetry.csv"; source("validation/check_acceleration_axes.R")',
       call. = FALSE)
}

# Column names in the raw file (Assetto Corsa defaults)
col_lap      <- "lapCount"
col_lap_time <- "lapTime"
col_steer    <- "steer"
col_speed    <- "speedKmh"
acc_cols     <- c("accGVertical", "accGHorizontal", "accGFrontal")

raw <- read_csv(real_file, col_select = any_of(c(col_lap, col_lap_time, col_steer, col_speed, acc_cols)),
                show_col_types = FALSE, progress = FALSE)
missing_cols <- setdiff(c(col_lap, col_lap_time, col_steer, col_speed), names(raw))
if (length(missing_cols) > 0) stop("Missing columns in real_file: ", paste(missing_cols, collapse = ", "))
acc_cols <- intersect(acc_cols, names(raw))

# Correlation of each acceleration column with steer x speed^2 (sign ignored
# when comparing; it depends on the logger's conventions)
proxy <- raw[[col_steer]] * raw[[col_speed]]^2
correlations <- tibble(
  column = acc_cols,
  r = map_dbl(acc_cols, \(cn) cor(raw[[cn]], proxy, use = "complete.obs"))
) |>
  arrange(desc(abs(r)))
cat("\nCorrelation with steering x speed^2 (all laps); the lateral column should be close to +1 or -1:\n")
print(correlations |> mutate(r = round(r, 3)))
cat("Most likely lateral acceleration column:", correlations$column[1], "\n")

# Plot of one lap
# Default: a typical lap, i.e. the one whose number of samples is closest to
# the median (avoids out-laps and laps with stops, which have more samples)
if (!exists("lap_to_plot")) {
  lap_to_plot <- raw |>
    count(.data[[col_lap]]) |>
    slice_min(abs(n - median(n)), n = 1, with_ties = FALSE) |>
    pull(1)
}
steer_scale <- 30   # steering divided by this, to be on a similar scale as G

p <- raw |>
  filter(.data[[col_lap]] == lap_to_plot) |>
  mutate(time_s = .data[[col_lap_time]] / 1000, steer_scaled = .data[[col_steer]] / steer_scale) |>
  pivot_longer(c(steer_scaled, all_of(acc_cols)), names_to = "channel", values_to = "value") |>
  ggplot(aes(time_s, value)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~channel, ncol = 1, scales = "free_y") +
  labs(x = "Lap time (s)", y = NULL,
       title = paste0("Lap ", lap_to_plot, " (steering divided by ", steer_scale, ")")) +
  theme_classic(base_family = "Times New Roman", base_size = 12)
print(p)
