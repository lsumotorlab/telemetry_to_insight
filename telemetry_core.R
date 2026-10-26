# telemetry_core.R - shared calculation functions
# ===================================================================
# Function library for the Telemetry-to-Insight toolkit. It is loaded
# automatically by the analysis scripts and is not run directly.
# All metric calculations live here, so the apps and the validation scripts
# (validation/) use exactly the same code.
#
# Units (after preprocessing): lapTime in ms (elapsed time within the lap),
# speedKmh in km/h, gas/brake in 0-1, accelerations in G,
# carPositionNormalized in 0-1 (fraction of the lap from the start/finish line).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})
filter <- dplyr::filter

# =============================================================================
# General helpers
# =============================================================================

# TRUE if all cols are present in df
has_cols <- function(df, cols) all(cols %in% names(df))

# Normalize a composite reference lap selection into a character vector
as_syn_laps <- function(x) {
  if (is.null(x)) return(character(0))
  if (length(x) == 0) return(character(0))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- x[x != ""]
  unique(x)
}

# =============================================================================
# Time weighting
# =============================================================================
# Each sample i represents the time around it:
#   w_i = (t[i+1] - t[i-1]) / 2   for interior samples
#   w_1 = t[2] - t[1],  w_n = t[n] - t[n-1]   at the endpoints
# Time-weighted mean:        mean_w(x) = sum(x_i * w_i) / sum(w_i)
# Time share of a condition: pct_w(c)  = 100 * sum(w_i[c_i]) / sum(w_i)
#                            (over samples where the condition is not NA)
# Under uniform sampling all w_i are equal, so these reduce exactly to the
# arithmetic mean and the share of samples. They stay correct for irregular
# sampling and for laps resampled in track position (composite reference lap).
# Samples must be in time order. Falls back to equal weights if no usable
# timing is available.
time_weights <- function(t) {
  n <- length(t)
  if (n < 2) return(rep(1, n))
  dt <- diff(as.numeric(t))
  dt[!is.finite(dt) | dt < 0] <- NA
  if (all(is.na(dt))) return(rep(1, n))
  w <- rowMeans(cbind(c(NA, dt), c(dt, NA)), na.rm = TRUE)
  w[!is.finite(w)] <- 0
  w
}

# lapTime column of a lap's rows, or NULL if absent
lap_times <- function(lap_data) {
  if ("lapTime" %in% names(lap_data)) lap_data$lapTime else NULL
}

# Missing data rule (one rule for all metrics): each metric uses only the
# samples where its channel has a value, and the time weights are computed
# from those samples. A missing value is therefore bridged by its valid
# neighbors, each covering half of the gap - exactly like a dropped sample.
# Rows without lapTime are left out; if a lap has no usable lapTime at all,
# its samples are weighted equally. A channel missing for the whole lap gives NA.
.weighting_times <- function(t, n) {
  if (is.null(t) || !any(is.finite(t))) seq_len(n) else as.numeric(t)
}

# Time share (%) where cond is TRUE: pct_w(c) over the samples with a value
lap_pct <- function(cond, t = NULL) {
  tt <- .weighting_times(t, length(cond))
  ok <- !is.na(cond) & is.finite(tt)
  if (!any(ok)) return(NA_real_)
  ord <- order(tt[ok])
  time_pct(cond[ok][ord], time_weights(tt[ok][ord]))
}

# Time-weighted mean: mean_w(x) over the samples with a value
lap_mean <- function(x, t = NULL) {
  tt <- .weighting_times(t, length(x))
  ok <- is.finite(x) & is.finite(tt)
  if (!any(ok)) return(NA_real_)
  ord <- order(tt[ok])
  time_mean(x[ok][ord], time_weights(tt[ok][ord]))
}


# Time share (%) where cond is TRUE for given weights (samples where cond is
# NA are left out); building block for lap_pct()
time_pct <- function(cond, w) {
  valid <- !is.na(cond)
  if (length(w) == 0 || sum(w[valid]) <= 0) return(NA_real_)
  sum(w[valid & cond]) / sum(w[valid]) * 100
}

# Brake/throttle overlap per sample: TRUE/FALSE, NA if either channel is missing
overlap_flag <- function(gas, brake, full_throttle_threshold, braking_threshold) {
  ifelse(is.na(gas) | is.na(brake), NA,
         gas > full_throttle_threshold & brake > braking_threshold)
}

# Max/min of the finite values; NA (not -Inf/Inf) if there are none
safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else max(x)
}
safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else min(x)
}

# Time-weighted mean, ignoring NA values
time_mean <- function(x, w) {
  ok <- is.finite(x)
  if (!any(ok) || sum(w[ok]) <= 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# =============================================================================
# Interpolation and filtering
# =============================================================================

# Linear interpolation of y(x) at xout; non-finite pairs and duplicate x are
# dropped, values outside the data range take the nearest end value
interp_numeric <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2) return(rep(NA_real_, length(xout)))
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  dup <- duplicated(x)
  x <- x[!dup]
  y <- y[!dup]
  if (length(x) < 2) return(rep(NA_real_, length(xout)))
  stats::approx(x = x, y = y, xout = xout, rule = 2)$y
}

# Zero-phase 2nd-order Butterworth low-pass filter (filtfilt).
# fs = sampling frequency (Hz). Returns x unchanged if the cutoff is not
# below the Nyquist frequency or fs is unknown.
lowpass_filter <- function(x, cutoff_hz, fs) {
  if (is.na(fs) || fs <= 0 || cutoff_hz <= 0 || cutoff_hz >= fs / 2) return(x)
  W <- cutoff_hz / (fs / 2)
  bf <- signal::butter(2, W, type = "low")
  as.numeric(signal::filtfilt(bf, x))
}

# Mean sampling rate (Hz) of one lap from its time stamps (ms):
# (distinct time stamps - 1) / (last - first). Unlike 1000 / median interval,
# this is correct when intervals alternate (e.g., 3 and 6 ms -> ~238 Hz, not 333).
sampling_rate_hz <- function(t) {
  t <- sort(unique(t[is.finite(t)]))
  if (length(t) < 2 || t[length(t)] <= t[1]) return(NA_real_)
  (length(t) - 1) / ((t[length(t)] - t[1]) / 1000)
}

# Low-pass filter for one lap's signal x with time stamps t (ms), robust to
# irregular sampling and missing values:
#   1. samples with a value and a time stamp, in time order
#   2. linear interpolation onto an even time grid at the mean sampling rate
#      (missing values and dropped samples bridged by their neighbors)
#   3. lowpass_filter() on the even grid
#   4. filtered signal interpolated back to the original time stamps
# Samples that were NA (or have no time stamp) stay NA. The sampling rate used
# is returned as attribute "fs". x is returned unchanged if cutoff_hz <= 0,
# fewer than 10 valid samples exist, or the cutoff is not below Nyquist.
lowpass_lap <- function(x, t, cutoff_hz) {
  ok <- is.finite(x) & is.finite(t)
  fs <- sampling_rate_hz(t)  # from all time stamps, as in the data-quality report
  out <- x
  attr(out, "fs") <- fs
  if (cutoff_hz <= 0 || sum(ok) < 10 || is.na(fs) || cutoff_hz >= fs / 2) return(out)

  ord <- order(t[ok])
  tv <- t[ok][ord]
  xv <- x[ok][ord]
  keep <- !duplicated(tv)
  tv <- tv[keep]
  xv <- xv[keep]
  grid <- seq(tv[1], tv[length(tv)], by = 1000 / fs)
  y_grid <- lowpass_filter(stats::approx(tv, xv, xout = grid, rule = 2)$y, cutoff_hz, fs)

  out[] <- NA_real_
  out[ok] <- stats::approx(grid, y_grid, xout = t[ok], rule = 2)$y
  attr(out, "fs") <- fs
  out
}

# Plot subtitle stating the filter and the sampling rate it was based on
filter_subtitle <- function(cutoff_hz, fs) {
  rate <- if (is.finite(fs)) sprintf(" (sampling rate %.0f Hz)", fs) else ""
  if (cutoff_hz > 0 && is.finite(fs) && cutoff_hz < fs / 2) {
    paste0("Low-pass: ", cutoff_hz, " Hz", rate)
  } else if (cutoff_hz > 0) {
    paste0("No filter: cutoff not below half the sampling rate", rate)
  } else {
    paste0("No filter", rate)
  }
}

# =============================================================================
# Lap summary metrics
# =============================================================================
# Per lap (time-weighted, missing data bridged by valid neighbors, see above):
#   Lap Time (s)               = interpolated time at which the car returns to the
#                                lap's own timing line (lap_time_estimate) / 1000
#   Full Throttle (%)          = pct_w(gas > full_throttle_threshold)
#   Braking (%)                = pct_w(brake > braking_threshold)
#   Brake/Throttle Overlap (%) = pct_w(gas > full_throttle_threshold & brake > braking_threshold)
#   Avg Speed (km/h)           = mean_w(speedKmh)
#   ABS / TC Active (%)        = pct_w(flag == 1)
#   Max/Min Speed, Max Accel G, Max Brake G   = sample extremes (unfiltered)
#   Max Lateral G (raw)        = max |accGHorizontal| of the recorded samples
#   Max Lateral G (filtered)   = max |lowpass_lap(accGHorizontal, lapTime, lateral_cutoff_hz)|;
#                                equals the raw value when lateral_cutoff_hz = 0
#   Sampling Rate (Hz)         = sampling_rate_hz(lapTime), the rate the filter uses
calculate_lap_stats <- function(
    telemetry,
    lap_num,
    full_throttle_threshold = 0.99,
    braking_threshold = 0.01,
    lateral_cutoff_hz = 0
) {
  lap_data <- telemetry |>
    dplyr::filter(lapCount == lap_num)

  if (nrow(lap_data) == 0) return(NULL)

  has_pos <- "carPositionNormalized" %in% names(lap_data)

  lap_data |>
    dplyr::summarise(
      Lap = lap_num,
      `Lap Time (s)` = (if (has_pos) lap_time_estimate(carPositionNormalized, lapTime)
                        else safe_max(lapTime)) / 1000,
      `Full Throttle (%)` = lap_pct(gas > full_throttle_threshold, lapTime),
      `Braking (%)` = lap_pct(brake > braking_threshold, lapTime),
      `Brake/Throttle Overlap (%)` = lap_pct(
        overlap_flag(gas, brake, full_throttle_threshold, braking_threshold), lapTime
      ),
      `Avg Speed (km/h)` = lap_mean(speedKmh, lapTime),
      `Max Speed (km/h)` = safe_max(speedKmh),
      `Min Speed (km/h)` = safe_min(speedKmh),
      `ABS Active (%)` = lap_pct(isAbsInAction == 1, lapTime),
      `TC Active (%)` = lap_pct(isTcInAction == 1, lapTime),
      `Max Lateral G (raw)` = if ("accGHorizontal" %in% names(lap_data)) safe_max(abs(accGHorizontal)) else NA_real_,
      `Max Lateral G (filtered)` = if ("accGHorizontal" %in% names(lap_data)) {
        safe_max(abs(as.numeric(lowpass_lap(accGHorizontal, lapTime, lateral_cutoff_hz))))
      } else {
        NA_real_
      },
      `Max Accel G` = if ("accGFrontal" %in% names(lap_data)) safe_max(accGFrontal) else NA_real_,
      `Max Brake G` = if ("accGFrontal" %in% names(lap_data)) safe_min(accGFrontal) else NA_real_,
      `Sampling Rate (Hz)` = sampling_rate_hz(lapTime),
      .groups = "drop"
    )
}

calculate_multiple_laps <- function(
    telemetry,
    lap_numbers,
    full_throttle_threshold = 0.99,
    braking_threshold = 0.01,
    lateral_cutoff_hz = 0
) {
  lap_numbers <- as.integer(lap_numbers)
  purrr::map_dfr(
    lap_numbers,
    \(x) calculate_lap_stats(
      telemetry,
      x,
      full_throttle_threshold = full_throttle_threshold,
      braking_threshold = braking_threshold,
      lateral_cutoff_hz = lateral_cutoff_hz
    )
  )
}

# Mean, SD, Min, Max of each per-lap metric across laps (one value per lap,
# so laps count equally). The composite reference lap is excluded.
get_summary_stats <- function(lap_stats_table, synthetic_lap = NA) {
  syn <- as_syn_laps(synthetic_lap)

  df <- lap_stats_table
  if ("Lap" %in% names(df) && length(syn) > 0) {
    df <- df |> dplyr::filter(!(as.character(.data$Lap) %in% syn))
  }

  df |>
    dplyr::select(-dplyr::any_of("Lap")) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(
          Mean = \(x) mean(x, na.rm = TRUE),
          SD = \(x) sd(x, na.rm = TRUE),
          Min = \(x) min(x, na.rm = TRUE),
          Max = \(x) max(x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Metric", values_to = "Value") |>
    tidyr::separate(Metric, into = c("Metric", "Statistic"), sep = "_(?=[^_]+$)") |>
    tidyr::pivot_wider(names_from = Metric, values_from = Value)
}

# =============================================================================
# Position-time: interpolated boundary crossings
# =============================================================================

# Positions of one lap in time order, unwrapped across the start/finish line
# (jumps of more than half a lap removed)
.unwrap_positions <- function(p) {
  jump <- c(0, diff(p))
  u <- p + cumsum(jump < -0.5) - cumsum(jump > 0.5)
  if (length(u) > 0 && u[1] > 0.5) u <- u - 1  # lap starts just before the line
  u
}

# Order one lap's samples by time and unwrap normalized position across the
# start/finish line, so position rises from ~0 to ~1 over the lap.
# Returns row indices (time order, first sample at each position) and the
# unwrapped positions u.
lap_unwrap <- function(pos, t) {
  idx <- which(is.finite(pos) & is.finite(t))
  idx <- idx[order(t[idx])]
  u <- cummax(.unwrap_positions(pos[idx]))
  keep <- !duplicated(u)
  list(idx = idx[keep], u = u[keep])
}

# Linear interpolation of y at xout, with linear extrapolation beyond the
# first/last sample of up to max_extrap (lap fraction); NA further out
interp_extrap <- function(x, y, xout, max_extrap = 0.02) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, length(xout)))
  out <- stats::approx(x, y, xout = xout, rule = 2)$y
  lo <- xout < x[1]
  hi <- xout > x[n]
  out[lo] <- y[1] + (xout[lo] - x[1]) * (y[2] - y[1]) / (x[2] - x[1])
  out[hi] <- y[n] + (xout[hi] - x[n]) * (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  out[(lo & x[1] - xout > max_extrap) | (hi & xout - x[n] > max_extrap)] <- NA_real_
  out
}

# Time at which a lap passes unwrapped position(s) u (0 = start/finish line at
# the start of the lap, 1 = at its end), interpolated linearly between the
# samples either side of each crossing
lap_time_at <- function(pos, t, u) {
  un <- lap_unwrap(pos, t)
  interp_extrap(un$u, t[un$idx], u)
}

# Sector duration = t(end) - t(start), both interpolated crossing times.
# A sector with start > end wraps across the start/finish line:
#   duration = [t(1) - t(start)] + [t(end) - t(0)]
seg_duration <- function(pos, t, start, end) {
  if (start < end) {
    tt <- lap_time_at(pos, t, c(start, end))
    tt[2] - tt[1]
  } else {
    tt <- lap_time_at(pos, t, c(start, 1, 0, end))
    (tt[2] - tt[1]) + (tt[4] - tt[3])
  }
}

# Lap time (ms) from the interpolated crossing of the lap's own timing line.
# The lap timer starts (lapTime = 0) when the car crosses the timing line; that
# position u0 is estimated from the first samples, and the lap time is the
# interpolated time at which the car reaches u0 + 1. The timing line does not
# have to be at normalized position 0 (in Assetto Corsa it is at about 0.0009).
# Falls back to the last sample's lapTime for laps that do not start at the
# timing line or do not reach it again (out-laps, incomplete laps).
lap_time_estimate <- function(pos, t, max_start_ms = 1000, max_extrap = 0.02) {
  fallback <- safe_max(t)
  un <- lap_unwrap(pos, t)
  u <- un$u
  tt <- as.numeric(t[un$idx])
  n <- length(u)
  if (n < 3 || tt[1] > max_start_ms) return(fallback)
  # position at lapTime = 0, extrapolated back from the first two samples
  u0 <- if (tt[1] <= 0 || tt[2] <= tt[1]) u[1] else u[1] - (u[2] - u[1]) / (tt[2] - tt[1]) * tt[1]
  if (u[n] < u0 + 1 - max_extrap) return(fallback)
  est <- interp_extrap(u, tt, u0 + 1, max_extrap = max_extrap)
  if (is.finite(est)) est else fallback
}

# =============================================================================
# Lap comparison (track_mapping_compare_interactive.R)
# =============================================================================

# One row per track position, in position order
prepare_lap_for_compare <- function(df) {
  need <- c("lapCount", "carCoordinatesX", "carCoordinatesZ", "carPositionNormalized")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop(paste("Missing required columns:", paste(miss, collapse = ", ")))
  }

  df |>
    dplyr::arrange(carPositionNormalized) |>
    dplyr::filter(is.finite(carPositionNormalized)) |>
    dplyr::distinct(carPositionNormalized, .keep_all = TRUE)
}

# Both laps linearly interpolated onto a common grid of n_points track
# positions; delta = comparison - reference
resample_two_laps <- function(ref_df, cmp_df, vars, n_points = 3000) {
  grid <- tibble::tibble(carPositionNormalized = seq(0, 1, length.out = n_points))

  out <- grid

  for (v in vars) {
    if (v %in% names(ref_df)) {
      out[[paste0(v, "_ref")]] <- interp_numeric(
        ref_df$carPositionNormalized,
        ref_df[[v]],
        grid$carPositionNormalized
      )
    } else {
      out[[paste0(v, "_ref")]] <- NA_real_
    }

    if (v %in% names(cmp_df)) {
      out[[paste0(v, "_cmp")]] <- interp_numeric(
        cmp_df$carPositionNormalized,
        cmp_df[[v]],
        grid$carPositionNormalized
      )
    } else {
      out[[paste0(v, "_cmp")]] <- NA_real_
    }

    out[[paste0(v, "_delta")]] <- out[[paste0(v, "_cmp")]] - out[[paste0(v, "_ref")]]
  }

  out
}

# Lap time (s), same definition as the lap summary (lap_time_estimate)
compute_lap_time_seconds <- function(df) {
  if (!("lapTime" %in% names(df))) {
    return(NA_real_)
  }

  if ("carPositionNormalized" %in% names(df)) {
    lap_time_estimate(df$carPositionNormalized, df$lapTime) / 1000
  } else {
    safe_max(df$lapTime) / 1000
  }
}

# Sector boundaries and times (ms) of a composite reference lap
compute_seg_times_synthetic <- function(raw_df) {
  if (!has_cols(raw_df, c("seg", "lapTime", "carPositionNormalized"))) {
    return(list(times = numeric(0), bounds = NULL))
  }

  df <- raw_df |>
    dplyr::filter(!is.na(seg), is.finite(lapTime), is.finite(carPositionNormalized))

  if (nrow(df) == 0) {
    return(list(times = numeric(0), bounds = NULL))
  }

  if (has_cols(df, c("start", "end")) && all(is.finite(df$start)) && all(is.finite(df$end))) {
    # Exact sector boundaries stored with the composite lap
    bounds <- df |>
      dplyr::distinct(seg, start, end) |>
      dplyr::arrange(seg)
  } else {
    # Older composite files without boundary columns: first/last sample of each sector
    bounds <- df |>
      dplyr::arrange(lapTime) |>
      dplyr::group_by(seg) |>
      dplyr::summarise(
        start = dplyr::first(carPositionNormalized),
        end   = dplyr::last(carPositionNormalized),
        .groups = "drop"
      ) |>
      dplyr::arrange(seg)
  }

  list(times = compute_seg_times_real(raw_df, bounds), bounds = bounds)
}

# Sector times (ms) of any lap for the given boundaries, from interpolated crossings
compute_seg_times_real <- function(raw_df, bounds) {
  if (is.null(bounds) ||
      !has_cols(raw_df, c("lapTime", "carPositionNormalized")) ||
      !all(c("seg", "start", "end") %in% names(bounds))) {
    return(numeric(0))
  }

  times <- vapply(seq_len(nrow(bounds)), function(i) {
    seg_duration(raw_df$carPositionNormalized, raw_df$lapTime,
                 bounds$start[i], bounds$end[i])
  }, numeric(1))

  stats::setNames(times, bounds$seg)
}

# =============================================================================
# Corner analysis (corner_analysis_interactive.R)
# =============================================================================
# Corner across the start/finish line (entry_pos > exit_pos): the end of the
# lap is joined to the start of the same lap, as for sector times
# (seg_duration). Samples between the exit and the entry are split at the
# midpoint; those before it (the start of the lap) get position u + 1 and time
# t + [t(1) - t(0)], so the corner runs continuously from entry_pos to
# exit_pos + 1 and corner time = [t(1) - t(entry)] + [t(exit) - t(0)].
# Positions are unwrapped as in lap_unwrap(). Other corners are returned unchanged.
corner_unwrap <- function(lap_df, entry_pos, exit_pos) {
  if (entry_pos <= exit_pos) return(lap_df)
  pos <- lap_df$carPositionNormalized
  t   <- lap_df$lapTime
  idx <- which(is.finite(pos) & is.finite(t))
  if (length(idx) < 2) return(lap_df)
  idx <- idx[order(t[idx])]

  u     <- .unwrap_positions(pos[idx])
  tt    <- t[idx]
  shift <- diff(lap_time_at(pos, t, c(0, 1)))
  start_part <- u < (exit_pos + entry_pos) / 2
  u[start_part]  <- u[start_part] + 1
  tt[start_part] <- tt[start_part] + shift

  lap_df$carPositionNormalized[idx] <- u
  lap_df$lapTime[idx] <- tt
  lap_df
}

# Per lap (df grouped by lap_label), corner from entry_pos to exit_pos:
#   Corner Time (s)    = [t(exit_pos) - t(entry_pos)] / 1000, lapTime linearly
#                        interpolated at the entry and exit positions
#   Entry/Exit Speed   = speed linearly interpolated at entry/exit position
#   Min Speed          = minimum sample speed inside the corner
#   Avg Speed          = mean_w(speedKmh) of samples inside the corner (time-weighted)
#   Max Lateral G (raw)      = max |accGHorizontal_raw| inside the corner (if the
#                              app kept the unfiltered values; else not reported)
#   Max Lateral G (filtered) = max |accGHorizontal| inside the corner (filtered
#                              over the whole lap by the app; raw if cutoff = 0)
#   Max Braking G      = max |accGFrontal| of negative values inside the corner
#   Max Accel. G       = max accGFrontal of positive values inside the corner
# A corner with entry_pos > exit_pos crosses the start/finish line; positions
# and times are first made continuous with corner_unwrap().
corner_summary_table <- function(df, entry_pos, exit_pos) {
  if (!has_cols(df, c("speedKmh", "lapTime", "carPositionNormalized"))) return(NULL)

  if (entry_pos > exit_pos) {
    df <- df |>
      dplyr::group_by(lap_label) |>
      dplyr::group_modify(\(d, k) corner_unwrap(d, entry_pos, exit_pos)) |>
      dplyr::ungroup()
    exit_pos <- exit_pos + 1
  }

  lo <- min(entry_pos, exit_pos)
  hi <- max(entry_pos, exit_pos)

  df |>
    dplyr::group_by(lap_label) |>
    dplyr::summarise(
      corner_time_s   = (interp_numeric(carPositionNormalized, lapTime, exit_pos) -
                           interp_numeric(carPositionNormalized, lapTime, entry_pos)) / 1000,
      entry_speed_kmh = interp_numeric(carPositionNormalized, speedKmh, entry_pos),
      min_speed_kmh   = safe_min(speedKmh[carPositionNormalized >= lo & carPositionNormalized <= hi]),
      avg_speed_kmh   = {
        in_s <- carPositionNormalized >= lo & carPositionNormalized <= hi
        lap_mean(speedKmh[in_s], lapTime[in_s])
      },
      exit_speed_kmh  = interp_numeric(carPositionNormalized, speedKmh, exit_pos),
      max_lat_g_raw   = if (has_cols(df, "accGHorizontal_raw"))
                          safe_max(abs(accGHorizontal_raw[carPositionNormalized >= lo & carPositionNormalized <= hi]))
                        else NA_real_,
      max_lat_g       = if (has_cols(df, "accGHorizontal"))
                          safe_max(abs(accGHorizontal[carPositionNormalized >= lo & carPositionNormalized <= hi]))
                        else NA_real_,
      max_brake_g     = if (has_cols(df, "accGFrontal")) {
                          vals <- accGFrontal[carPositionNormalized >= lo & carPositionNormalized <= hi &
                                                accGFrontal < 0]
                          safe_max(abs(vals))
                        } else NA_real_,
      max_accel_g     = if (has_cols(df, "accGFrontal")) {
                          vals <- accGFrontal[carPositionNormalized >= lo & carPositionNormalized <= hi &
                                                accGFrontal > 0]
                          safe_max(vals)
                        } else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(corner_time_s)
}

# =============================================================================
# Composite reference lap (create_synthetic_lap.R)
# =============================================================================
# Sector time  = interpolated crossing-time difference (seg_duration) per lap;
#                the fastest lap is chosen for each sector.
# Composite lap: points on a regular position grid; time at a point =
#   (offset of its sector) + [t_src(u) - t_src(sector start)], where t_src is
#   the source lap time interpolated at that position, i.e. each sector keeps
#   the position-time relationship of its source lap. Time is re-referenced to 0
#   at the start/finish line. Channels are linearly interpolated from the whole
#   source lap (nearest sample for gear/flags).

build_synthetic_lap_from_bounds <- function(
    telemetry,
    bounds,
    lap_col = "lapCount",
    pos_col = "carPositionNormalized",
    time_col = "lapTime",
    include_boundary = "left",
    min_samples_per_seg = 10
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })
  
  stopifnot(include_boundary %in% c("left", "right"))
  include_left <- include_boundary == "left"
  
  if (!all(c("seg", "start", "end") %in% names(bounds))) {
    stop("bounds must contain columns: seg, start, end")
  }
  
  in_interval <- function(pos, start, end, include_left = TRUE) {
    if (start < end) {
      if (include_left) pos >= start & pos < end else pos > start & pos <= end
    } else {
      if (include_left) (pos >= start & pos < 1) | (pos >= 0 & pos < end)
      else (pos > start & pos <= 1) | (pos >= 0 & pos <= end)
    }
  }
  
  per_lap <- telemetry %>%
    filter(.data[[lap_col]] > 0) %>%
    arrange(.data[[lap_col]], .data[[pos_col]], .data[[time_col]])

  # Sector time = interpolated time at the end boundary minus interpolated
  # time at the start boundary (not a sum of whole sample intervals)
  seg_times <- purrr::map_dfr(seq_len(nrow(bounds)), function(i) {
    b <- bounds[i, ]
    per_lap %>%
      group_by(.data[[lap_col]]) %>%
      summarise(
        n_seg = sum(in_interval(.data[[pos_col]], b$start, b$end, include_left), na.rm = TRUE),
        seg_time = seg_duration(.data[[pos_col]], .data[[time_col]], b$start, b$end),
        .groups = "drop"
      ) %>%
      mutate(seg = b$seg, start = b$start, end = b$end)
  })

  winners <- seg_times %>%
    filter(n_seg >= min_samples_per_seg, is.finite(seg_time)) %>%
    group_by(seg) %>%
    slice_min(seg_time, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(seg)
  
  missing_segs <- setdiff(bounds$seg, winners$seg)
  if (length(missing_segs) > 0) {
    fallback <- seg_times %>%
      filter(seg %in% missing_segs, is.finite(seg_time)) %>%
      group_by(seg) %>%
      slice_max(n_seg, n = 1, with_ties = FALSE) %>%
      ungroup()
    winners <- bind_rows(winners, fallback) %>% arrange(seg)
  }
  
  stitched <- purrr::map_dfr(seq_len(nrow(bounds)), function(i) {
    b <- bounds[i, ]
    wlap <- winners %>% filter(seg == b$seg) %>% pull(.data[[lap_col]])
    per_lap %>%
      filter(.data[[lap_col]] == wlap) %>%
      arrange(.data[[pos_col]], .data[[time_col]]) %>%
      filter(in_interval(.data[[pos_col]], b$start, b$end, include_left)) %>%
      mutate(seg = b$seg, source_lap = wlap)
  }) %>%
    arrange(seg, .data[[pos_col]], .data[[time_col]])
  
  list(
    synthetic_lap = stitched,
    segments = winners %>% select(seg, start, end, source_lap = !!rlang::sym(lap_col), seg_time)
  )
}

finalize_synthetic_lap <- function(
    stitched,
    telemetry_cols = NULL,
    lap_col = "lapCount",
    time_col = "lapTime",
    pos_col = "carPositionNormalized",
    new_lap = NULL,
    time_absolute_col = "time",
    time_offset = NA_real_,
    keep_provenance = TRUE
) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  if (is.null(telemetry_cols)) telemetry_cols <- names(stitched)
  
  if (!pos_col %in% names(stitched)) stop("stitched is missing ", pos_col)
  if (!time_col %in% names(stitched)) stop("stitched is missing ", time_col)
  
  if (is.null(new_lap)) new_lap <- 0L
  
  s <- stitched %>%
    arrange(.data[[pos_col]]) %>%
    mutate(
      !!lap_col := as.integer(new_lap)
    )
  
  if (time_absolute_col %in% names(s)) {
    if (is.finite(time_offset)) {
      s[[time_absolute_col]] <- time_offset + s[[time_col]]
    } else {
      s[[time_absolute_col]] <- NA_real_
    }
  }
  
  base_keep <- intersect(telemetry_cols, names(s))
  
  if (keep_provenance) {
    prov <- intersect(c("seg", "source_lap", "start", "end", "dt_seg"), names(s))
    keep <- unique(c(base_keep, prov))
  } else {
    keep <- base_keep
  }
  
  s %>%
    select(all_of(keep)) %>%
    mutate(.synthetic = TRUE)
}

unify_from_speed_output <- function(
    speed_output,
    telemetry,
    new_lap = NULL,
    keep_provenance = TRUE,
    time_offset = NA_real_
) {
  if (is.null(speed_output$synthetic_lap)) stop("speed_output must contain $synthetic_lap")
  stitched <- speed_output$synthetic_lap
  
  if (is.null(new_lap)) {
    new_lap <- max(telemetry$lapCount, na.rm = TRUE) + 1L
  }
  
  synth <- finalize_synthetic_lap(
    stitched = stitched,
    telemetry_cols = names(telemetry),
    new_lap = new_lap,
    keep_provenance = keep_provenance,
    time_offset = time_offset
  )
  
  missing_in_synth <- setdiff(names(telemetry), names(synth))
  if (length(missing_in_synth) > 0) {
    synth[missing_in_synth] <- NA
  }
  
  extra_in_synth <- setdiff(names(synth), names(telemetry))
  telemetry2 <- telemetry
  if (length(extra_in_synth) > 0) {
    telemetry2[extra_in_synth] <- NA
  }
  
  telemetry_with_synth <- dplyr::bind_rows(
    telemetry2,
    synth %>% dplyr::select(names(telemetry2))
  )
  
  list(
    synthetic_lap = synth,
    telemetry_with_synth = telemetry_with_synth
  )
}

resample_and_retime_synthetic_lap <- function(
    telemetry,
    segments,
    n_bins,
    discrete_numeric_cols = c("gear", "isAbsInAction", "isTcInAction", "isInPit"),
    seg_col = "seg",
    start_col = "start",
    end_col = "end",
    lap_col = "lapCount",
    pos_col = "carPositionNormalized",
    time_col = "lapTime",
    keep_provenance = TRUE
) {
  # Builds the composite lap on a regular position grid. Each grid point keeps
  # the timing of the lap the sector was taken from: time = (source-lap time at
  # that position) - (source-lap time at the sector start), interpolated
  # between samples, then chained across sectors. Channels are interpolated from
  # the whole source lap, so values at the boundaries use the samples on both
  # sides of the line.
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })

  need_seg <- c(seg_col, start_col, end_col, "source_lap")
  miss_seg <- setdiff(need_seg, names(segments))
  if (length(miss_seg) > 0) stop("segments is missing: ", paste(miss_seg, collapse = ", "))
  need_tel <- c(lap_col, pos_col, time_col)
  miss_tel <- setdiff(need_tel, names(telemetry))
  if (length(miss_tel) > 0) stop("telemetry is missing: ", paste(miss_tel, collapse = ", "))

  seg_tbl <- segments %>%
    transmute(
      seg = .data[[seg_col]],
      start = .data[[start_col]],
      end = .data[[end_col]],
      source_lap = .data[["source_lap"]]
    ) %>%
    arrange(seg) %>%
    mutate(len = ifelse(start < end, end - start, (1 - start) + end))

  if (sum(seg_tbl$len, na.rm = TRUE) <= 0) stop("Bad segment boundaries. Total length is not positive.")

  alloc_n <- pmax(2L, as.integer(round(n_bins * seg_tbl$len / sum(seg_tbl$len))))
  delta <- n_bins - sum(alloc_n)
  if (delta != 0) {
    idx <- which.max(seg_tbl$len)
    alloc_n[idx] <- alloc_n[idx] + delta
    if (alloc_n[idx] < 2) alloc_n[idx] <- 2
  }
  seg_tbl$n <- alloc_n

  # Value of the sample nearest to each xout (x sorted ascending)
  nearest_pick <- function(x, y, xout) {
    i <- findInterval(xout, x, all.inside = TRUE)
    pick <- ifelse(abs(x[i + 1] - xout) < abs(xout - x[i]), i + 1, i)
    y[pick]
  }

  # Provenance columns from an earlier composite lap in the same file are not channels
  value_cols <- setdiff(
    names(telemetry),
    c(lap_col, pos_col, time_col, seg_col, "source_lap", "start", "end", "dt_seg", ".synthetic")
  )

  resampled <- purrr::map_dfr(seq_len(nrow(seg_tbl)), function(i) {
    b <- seg_tbl[i, ]
    wrap <- b$start >= b$end

    src <- telemetry %>% filter(.data[[lap_col]] == b$source_lap)
    un <- lap_unwrap(src[[pos_col]], src[[time_col]])
    src <- src[un$idx, , drop = FALSE]
    u_src <- un$u
    t_at <- function(u) interp_extrap(u_src, src[[time_col]], u)

    # Grid over [start, end); a sector across the start/finish line runs from
    # start to end + 1 and always gets a point exactly on the line (u = 1)
    end_u <- if (wrap) b$end + 1 else b$end
    grid_u <- seq(b$start, end_u, length.out = b$n + 1)[seq_len(b$n)]
    if (wrap) grid_u <- sort(c(grid_u[abs(grid_u - 1) > 1e-9], 1))
    src_u <- ifelse(grid_u >= 1, grid_u - 1, grid_u)  # position within the source lap

    if (wrap) {
      to_line <- t_at(1) - t_at(b$start)
      t_seg <- ifelse(grid_u < 1,
                      t_at(grid_u) - t_at(b$start),
                      to_line + t_at(src_u) - t_at(0))
      seg_dur <- to_line + t_at(b$end) - t_at(0)
    } else {
      t_seg <- t_at(grid_u) - t_at(b$start)
      seg_dur <- t_at(b$end) - t_at(b$start)
    }

    out <- tibble::tibble(
      seg = b$seg,
      !!pos_col := grid_u %% 1,
      .t_seg = t_seg,
      .seg_dur = seg_dur,
      .on_line = wrap & grid_u == 1,
      source_lap = b$source_lap,
      start = b$start,
      end = b$end
    )

    for (cn in value_cols) {
      y <- src[[cn]]
      if (cn %in% discrete_numeric_cols || !is.numeric(y)) {
        # Discrete signals (gear, ABS/TC flags) and text: nearest sample
        out[[cn]] <- nearest_pick(u_src, y, src_u)
      } else {
        ok <- is.finite(y)
        out[[cn]] <- if (sum(ok) >= 2) {
          stats::approx(u_src[ok], y[ok], xout = src_u, rule = 2)$y
        } else {
          NA_real_
        }
      }
    }

    out
  })

  if (any(!is.finite(resampled$.t_seg)) || any(!is.finite(resampled$.seg_dur))) {
    stop("Could not time the composite lap: a source lap does not cover its whole sector.")
  }

  # Chain sectors: each sector starts when the previous one ends
  offsets <- resampled %>%
    distinct(seg, .seg_dur) %>%
    arrange(seg) %>%
    mutate(.offset = lag(cumsum(.seg_dur), default = 0))
  total <- sum(offsets$.seg_dur)

  resampled <- resampled %>%
    left_join(offsets %>% select(seg, .offset), by = "seg") %>%
    mutate(.t = .offset + .t_seg)

  # Re-reference time to the start/finish line, like a recorded lap:
  # lapTime = 0 at position 0, plus a closing point at position 1 with the
  # full composite lap time
  t0 <- resampled$.t[resampled$.on_line][1]
  closing <- resampled %>%
    filter(.on_line) %>%
    slice(1) %>%
    mutate(!!pos_col := 1, !!time_col := total)
  resampled <- resampled %>%
    mutate(!!time_col := (.t - t0) %% total)
  # If a sector starts exactly on the line, keep its point and drop the duplicate
  if (sum(resampled[[pos_col]] == 0) > 1) resampled <- resampled %>% filter(!.on_line)

  out <- bind_rows(resampled, closing) %>%
    arrange(.data[[time_col]], .data[[pos_col]]) %>%
    select(-.t_seg, -.seg_dur, -.on_line, -.offset, -.t) %>%
    rename(!!seg_col := seg)

  if (!keep_provenance) {
    out <- out %>% select(-any_of(c("source_lap", "start", "end")))
  }

  out
}

# =============================================================================
# Harmonizer: standard variables, column matching, unit detection
# (telemetry_harmonizer_and_preprocessing.R)
# =============================================================================
required_fields <- c(
  "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
  "lapCount", "lapTime", "carPositionNormalized",
  "speedKmh", "gear", "brake", "gas",
  "engineRPM", "steer",
  "isAbsInAction", "isTcInAction", "isInPit",
  "accGHorizontal", "accGFrontal"
)

field_labels <- c(
  carCoordinatesX        = "Car coordinates X - Lateral",
  carCoordinatesY        = "Car coordinates Y - Altitude",
  carCoordinatesZ        = "Car coordinates Z - Longitudinal",
  lapCount               = "Lap count",
  lapTime                = "Lap time",
  carPositionNormalized  = "Track position (normalized)",
  speedKmh               = "Speed",
  gear                   = "Gear",
  brake                  = "Brake",
  gas                    = "Gas / Throttle",
  engineRPM              = "Engine RPM",
  steer                  = "Steering",
  isAbsInAction          = "ABS active",
  isTcInAction           = "Traction control active",
  isInPit                = "In pit",
  accGHorizontal         = "Lateral acceleration",
  accGFrontal            = "Longitudinal acceleration"
)

guess_mapping <- function(names_in) {
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  names_norm <- norm(names_in)
  
  syn <- list(
    carCoordinatesX = c("^x$", "xpos", "carcoordinatesx", "coordx", "xcoordinate", "autocoordinatesx", "autocoordsx"),
    carCoordinatesY = c("^y$", "ypos", "carcoordinatesy", "coordy", "ycoordinate", "elev", "autocoordinatesy", "autocoordsy"),
    carCoordinatesZ = c("^z$", "zpos", "carcoordinatesz", "coordz", "zcoordinate", "long", "lon", "autocoordinatesz", "autocoordsz"),
    
    lapCount = c("lap", "lapcount", "lap_number", "lapno", "lapidx", "lapindex"),
    lapTime = c("laptime", "lap_time", "lapduration", "laptimesec", "laptimes", "timeperlap"),
    carPositionNormalized = c("carpositionnormalized", "positionnormalized", "posnorm", "trackpos", "trackposition", "splinepos", "splineposition"),
    
    speedKmh = c("speed", "speed_kmh", "speedkmh", "kmh", "kph", "speedkph", "speedms", "mps", "velocity"),
    gear = c("^gear$", "gearbox", "g", "currentgear"),
    
    brake = c("brake", "brakepressure", "brake_pct", "brakeperc", "braking", "brakeinput"),
    gas = c("gas", "throttle", "accel", "throttle_pct", "gas_pedal", "accelerator"),
    
    engineRPM = c("enginerpm", "^rpm$", "engine_speed", "enginespeed", "revs", "rev", "motor_rpm", "motorrpm"),
    steer = c("steer", "steering", "steerangle", "steeringangle", "wheelangle", "steeringwheel", "steerinput", "steeringinput"),
    
    isAbsInAction = c("abs", "isabs", "absinaction", "abs_active", "absenabled", "isabsenabled", "absactive"),
    isTcInAction = c("tc", "istc", "tcinaction", "tc_active", "tcenabled", "tractioncontrol", "tcactive"),
    
    isInPit = c("isinpit", "inpit", "pit", "pitlane", "inpitlane", "pitstatus", "pit_flag", "pitflag", "inpits"),

    accGHorizontal = c("accghorizontal", "lateralg", "lateral_g", "acclateral", "glateral", "acchorizontal", "lateralaccel"),
    accGFrontal    = c("accgfrontal", "longitudinalg", "longitudinal_g", "accfrontal", "gfrontal", "acclongitudinal", "longitudinalaccel")
  )
  
  pick_one <- function(patterns, target_norm) {
    idx_exact_target <- which(names_norm == target_norm)
    if (length(idx_exact_target)) return(names_in[idx_exact_target[1]])
    
    patterns_norm <- unique(norm(gsub("^\\^|\\$$", "", patterns)))
    idx_exact_syn <- which(names_norm %in% patterns_norm[nzchar(patterns_norm)])
    if (length(idx_exact_syn)) return(names_in[idx_exact_syn[1]])
    
    for (p in patterns) {
      idx <- which(grepl(p, names_norm, perl = TRUE))
      if (length(idx)) return(names_in[idx[1]])
    }
    
    idx_sub <- which(vapply(
      names_norm,
      function(n) nzchar(n) && (grepl(target_norm, n, fixed = TRUE) || grepl(n, target_norm, fixed = TRUE)),
      logical(1)
    ))
    if (length(idx_sub)) return(names_in[idx_sub[1]])
    
    d <- as.numeric(adist(target_norm, names_norm, partial = TRUE))
    best <- which.min(d)
    thresh <- max(2, ceiling(nchar(target_norm) * 0.25))
    if (length(best) && d[best] <= thresh) return(names_in[best])
    
    NA_character_
  }
  
  res <- setNames(rep(NA_character_, length(required_fields)), required_fields)
  for (t in required_fields) {
    if (t %in% names_in) {
      res[t] <- t
    } else {
      res[t] <- pick_one(syn[[t]], norm(t))
    }
  }
  res
}

# Lateral-acceleration plausibility check (harmonizer).
# In a corner, lateral acceleration = v^2 / R and the steering angle is roughly
# proportional to 1 / R, so lateral acceleration should follow steer x speed^2
# (correlation close to +1 or -1; the sign depends on the logger's convention).
# Every acceleration-like column of the uploaded file is correlated with
# steer x speed^2. Loggers sometimes label the axes differently (e.g., lateral
# acceleration stored as "accGVertical"), which this check reveals.
#   Not checked : steering, speed or lateral acceleration not mapped, or constant
#   OK          : |r| of the mapped column >= min_r and no other column fits
#                 better by more than margin
#   Warning     : otherwise; a better-fitting column is suggested if one exists
check_lateral_channel <- function(df, mapping, min_r = 0.5, margin = 0.2) {
  col_of <- function(field) {
    v <- mapping[[field]]
    if (is.null(v) || is.na(v) || v == "<none>" || !(v %in% names(df))) NA_character_ else v
  }
  lat_col <- col_of("accGHorizontal")
  steer_col <- col_of("steer")
  speed_col <- col_of("speedKmh")
  if (anyNA(c(lat_col, steer_col, speed_col))) {
    return(list(status = "not checked", message = "Lateral-acceleration check: not done (needs steering, speed and lateral acceleration mapped).", table = NULL))
  }

  proxy <- suppressWarnings(as.numeric(df[[steer_col]]) * as.numeric(df[[speed_col]])^2)
  candidates <- names(df)[grepl("acc|lateral", tolower(names(df))) &
                            vapply(df, is.numeric, logical(1))]
  candidates <- union(lat_col, setdiff(candidates, c(steer_col, speed_col)))
  r_of <- function(cn) {
    x <- suppressWarnings(as.numeric(df[[cn]]))
    ok <- is.finite(x) & is.finite(proxy)
    if (sum(ok) < 100 || sd(x[ok]) == 0 || sd(proxy[ok]) == 0) return(NA_real_)
    cor(x[ok], proxy[ok])
  }
  tbl <- tibble(column = candidates, r = vapply(candidates, r_of, numeric(1))) |>
    arrange(desc(abs(r)))

  r_lat <- tbl$r[tbl$column == lat_col]
  if (!is.finite(r_lat)) {
    return(list(status = "not checked", message = paste0("Lateral-acceleration check: not done (", lat_col, " or steering has no variation, e.g., synthetic data)."), table = tbl))
  }
  others <- tbl |> filter(column != lat_col, is.finite(r))
  best <- if (nrow(others) > 0) others[1, ] else NULL
  better <- !is.null(best) && abs(best$r) >= min_r && abs(best$r) > abs(r_lat) + margin

  if (abs(r_lat) >= min_r && !better) {
    list(status = "ok", table = tbl,
         message = sprintf("Lateral-acceleration check: OK. %s follows steering x speed^2 (r = %.2f).", lat_col, r_lat))
  } else if (better) {
    list(status = "warning", table = tbl,
         message = sprintf(paste0("Lateral-acceleration check: WARNING. %s does not follow steering x speed^2 (r = %.2f), but %s does (r = %.2f).\n",
                                  "The logger may label the acceleration axes differently. Consider mapping %s to Lateral acceleration."),
                           lat_col, r_lat, best$column, best$r, best$column))
  } else {
    list(status = "warning", table = tbl,
         message = sprintf("Lateral-acceleration check: WARNING. %s does not follow steering x speed^2 (r = %.2f). Check the mapping.", lat_col, r_lat))
  }
}

harmonize_telemetry <- function(df, mapping, speed_unit = c("auto", "kmh", "ms"), lap_time_unit = c("auto", "milliseconds", "seconds"), acc_unit = c("auto", "g", "ms2"), pedal_unit = c("auto", "fraction", "percent")) {
  speed_unit    <- match.arg(speed_unit)
  lap_time_unit <- match.arg(lap_time_unit)
  acc_unit      <- match.arg(acc_unit)
  pedal_unit    <- match.arg(pedal_unit)
  n <- nrow(df)
  out_list <- setNames(vector("list", length(required_fields)), required_fields)
  
  for (f in required_fields) {
    sel <- mapping[[f]]
    
    if (is.null(sel) || sel == "<none>" || is.na(sel) || !nzchar(sel)) {
      out_list[[f]] <- rep(NA, n)
      next
    }
    if (!sel %in% names(df)) {
      out_list[[f]] <- rep(NA, n)
      next
    }
    
    col <- df[[sel]]
    
    if (f %in% c(
      "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
      "speedKmh", "brake", "gas", "carPositionNormalized",
      "engineRPM", "steer", "lapTime",
      "accGHorizontal", "accGFrontal"
    )) {
      out_list[[f]] <- suppressWarnings(as.numeric(col))
      
    } else if (f %in% c("lapCount", "gear")) {
      out_list[[f]] <- suppressWarnings(as.integer(as.numeric(col)))
      
    } else if (f %in% c("isAbsInAction", "isTcInAction", "isInPit")) {
      
      if (is.logical(col)) {
        out_list[[f]] <- as.integer(col)
      } else {
        numeric_col <- suppressWarnings(as.numeric(col))
        if (sum(!is.na(numeric_col)) >= max(1, floor(0.8 * length(numeric_col)))) {
          out_list[[f]] <- as.integer(ifelse(is.na(numeric_col), NA, numeric_col > 0))
        } else {
          txt <- tolower(trimws(as.character(col)))
          out_list[[f]] <- as.integer(ifelse(
            txt %in% c("true", "t", "yes", "y", "1", "on", "in", "pit", "pitlane"),
            1,
            ifelse(txt %in% c("false", "f", "no", "n", "0", "off", "out"), 0, NA)
          ))
        }
      }
      
    } else {
      out_list[[f]] <- col
    }
    
    if (length(out_list[[f]]) == 1 && n > 1) out_list[[f]] <- rep(out_list[[f]], n)
  }
  
  # Unit decisions are recorded in a report (attribute "units_report"): the
  # setting, the statistic of the input values the auto rule uses, the rule,
  # and the result. The statistic is shown for manual settings too.
  report <- list()
  add_report <- function(variable, column, setting, statistic, rule, decision) {
    report[[length(report) + 1]] <<- tibble::tibble(
      Variable = variable, `Input column` = column, Setting = setting,
      `Input statistic` = statistic, `Auto-detect rule` = rule, Result = decision
    )
  }
  col_name <- \(f) if (is.null(mapping[[f]]) || is.na(mapping[[f]])) NA_character_ else mapping[[f]]

  if (!all(is.na(out_list[["speedKmh"]]))) {
    svals <- out_list[["speedKmh"]]
    # auto: use improved heuristics based on physical limits
    # 120 m/s = 432 km/h (impossible for any race car) -> must already be km/h
    # median > 50 m/s = 180 km/h average (unrealistic for m/s dataset) -> km/h
    q99  <- quantile(svals, 0.99, na.rm = TRUE, names = FALSE)
    med  <- median(svals, na.rm = TRUE)
    do_convert <- if (speed_unit == "ms") {
      TRUE
    } else if (speed_unit == "kmh") {
      FALSE
    } else {
      is_kmh <- (is.finite(q99) && q99 > 120) || (is.finite(med) && med > 50)
      !is_kmh
    }
    if (do_convert) out_list[["speedKmh"]] <- out_list[["speedKmh"]] * 3.6
    add_report("Speed", col_name("speedKmh"),
               c(auto = "Auto-detect", kmh = "km/h", ms = "m/s")[[speed_unit]],
               sprintf("99th percentile = %.1f; median = %.1f", q99, med),
               "km/h if 99th percentile > 120 or median > 50, else m/s",
               if (do_convert) "m/s -> multiplied by 3.6 (km/h)" else "km/h (unchanged)")
  }

  if (!all(is.na(out_list[["lapTime"]]))) {
    lvals <- out_list[["lapTime"]]
    # auto: no real racing lap exceeds 1000 seconds; if median <= 1000 values are in seconds
    med <- median(lvals, na.rm = TRUE)
    do_convert <- if (lap_time_unit == "seconds") {
      TRUE
    } else if (lap_time_unit == "milliseconds") {
      FALSE
    } else {
      is.finite(med) && med <= 1000
    }
    if (do_convert) out_list[["lapTime"]] <- out_list[["lapTime"]] * 1000
    add_report("Lap time", col_name("lapTime"),
               c(auto = "Auto-detect", milliseconds = "Milliseconds", seconds = "Seconds")[[lap_time_unit]],
               sprintf("median = %.1f", med),
               "seconds if median <= 1000, else milliseconds",
               if (do_convert) "seconds -> multiplied by 1000 (ms)" else "milliseconds (unchanged)")
  }

  if (!all(is.na(out_list[["carPositionNormalized"]]))) {
    mx <- suppressWarnings(max(out_list[["carPositionNormalized"]], na.rm = TRUE))
    rescale <- is.finite(mx) && mx > 1.1
    if (rescale) {
      out_list[["carPositionNormalized"]] <- out_list[["carPositionNormalized"]] / mx
    }
    add_report("Track position", col_name("carPositionNormalized"), "Automatic",
               sprintf("maximum = %.3f", mx),
               "divided by its maximum if maximum > 1.1 (e.g., meters or %)",
               if (rescale) sprintf("divided by %.3f (0-1)", mx) else "0-1 (unchanged)")
  }

  for (acc_col in c("accGHorizontal", "accGFrontal")) {
    if (!all(is.na(out_list[[acc_col]]))) {
      avals <- out_list[[acc_col]]
      # auto: if 99th percentile of absolute values > 10, values are almost certainly m/s²
      # (10G is physically impossible for a race car; 10 m/s² ≈ 1G is normal)
      q99 <- quantile(abs(avals), 0.99, na.rm = TRUE, names = FALSE)
      do_convert <- if (acc_unit == "ms2") {
        TRUE
      } else if (acc_unit == "g") {
        FALSE
      } else {
        is.finite(q99) && q99 > 10
      }
      if (do_convert) out_list[[acc_col]] <- out_list[[acc_col]] / 9.81
      add_report(field_labels[[acc_col]], col_name(acc_col),
                 c(auto = "Auto-detect", g = "G", ms2 = "m/s²")[[acc_unit]],
                 sprintf("99th percentile of |value| = %.2f", q99),
                 "m/s² if 99th percentile of |value| > 10, else G",
                 if (do_convert) "m/s² -> divided by 9.81 (G)" else "G (unchanged)")
    }
  }

  for (pedal_col in c("gas", "brake")) {
    if (!all(is.na(out_list[[pedal_col]]))) {
      pvals <- out_list[[pedal_col]]
      # auto: pedal inputs start at 0 (released); the maximum over the whole file
      # shows the scale. A 0-1 input cannot exceed 1, while a 0-100 input exceeds
      # 1.5 in any real session (full throttle, firm braking). Above 105 the
      # scale is unknown (e.g., raw sensor counts, brake pressure): not changed,
      # flagged in the report for the user to convert.
      mx <- suppressWarnings(max(pvals, na.rm = TRUE))
      unknown <- is.finite(mx) && mx > 105
      do_convert <- !unknown && (if (pedal_unit == "percent") {
        TRUE
      } else if (pedal_unit == "fraction") {
        FALSE
      } else {
        is.finite(mx) && mx > 1.5
      })
      if (do_convert) out_list[[pedal_col]] <- out_list[[pedal_col]] / 100
      add_report(field_labels[[pedal_col]], col_name(pedal_col),
                 c(auto = "Auto-detect", fraction = "0-1", percent = "0-100 (%)")[[pedal_unit]],
                 sprintf("maximum = %.2f", mx),
                 "0-100 if maximum > 1.5, else 0-1; unknown scale if maximum > 105",
                 if (unknown) "WARNING: unknown scale (maximum > 105), not changed. Convert to 0-1 before loading."
                 else if (do_convert) "0-100 -> divided by 100 (0-1)" else "0-1 (unchanged)")
    }
  }

  out <- tibble::as_tibble(out_list)
  attr(out, "units_report") <- dplyr::bind_rows(report)
  out
}

# =============================================================================
# Preprocessing: lap overview and lap filters
# (telemetry_harmonizer_and_preprocessing.R)
# =============================================================================
select_analysis_columns <- function(telemetry) {
  telemetry |>
    select(any_of(c(
      "time", "lapCount", "lapTime",
      "carPositionNormalized", "carCoordinatesX", "carCoordinatesY", "carCoordinatesZ",
      "speedKmh", "speedMs", "engineRPM", "gear",
      "gas", "brake", "steer",
      "isAbsInAction", "isTcInAction", "isInPit",
      "accGHorizontal", "accGFrontal"
    )))
}

lap_overview_tbl <- function(telemetry) {
  if (!("lapCount" %in% names(telemetry))) return(tibble(lapCount = integer(0)))
  
  has_pit <- "isInPit" %in% names(telemetry)
  safe_max <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else max(x)
  }
  
  telemetry |>
    group_by(lapCount) |>
    summarise(
      rows = n(),
      pit_rows = if (has_pit) sum(isInPit == 1, na.rm = TRUE) else NA_integer_,
      lap_time_ms = if ("lapTime" %in% names(telemetry)) safe_max(lapTime) else NA_real_,
      lap_time_sec = if ("lapTime" %in% names(telemetry)) safe_max(lapTime) / 1000 else NA_real_,
      .groups = "drop"
    ) |>
    mutate(
      lap_time_formatted = if_else(
        is.finite(lap_time_sec),
        sprintf("%d:%05.2f", floor(lap_time_sec / 60), lap_time_sec %% 60),
        NA_character_
      )
    ) |>
    arrange(lapCount)
}

# Check of one lap's normalized track position against the distance driven,
# calculated independently from the car coordinates (time order, samples with
# position and coordinates):
#   u            = position unwrapped across the start/finish line
#   Coverage     = (max u - min u), lap fraction; ~1 for a complete lap
#   Deviation    = max |(u - u_first) / (u_last - u_first) - D / D_total|,
#                  D = cumulative distance between consecutive car positions
#                  (X, Z, and Y if available); lap fraction
# Position is measured along the track's reference line and distance along the
# driven line, so small deviations are expected; large ones indicate a wrong
# or distorted position channel (or spins).
position_check <- function(pos, t, x, z, y = NULL) {
  if (is.null(y) || all(!is.finite(y))) y <- rep(0, length(pos))
  ok <- is.finite(pos) & is.finite(t) & is.finite(x) & is.finite(z) & is.finite(y)
  idx <- which(ok)
  idx <- idx[order(t[idx])]
  out <- list(coverage = NA_real_, deviation = NA_real_)
  if (length(idx) < 3) return(out)

  p <- pos[idx]
  jump <- c(0, diff(p))
  u <- p + cumsum(jump < -0.5) - cumsum(jump > 0.5)
  out$coverage <- max(u) - min(u)

  d <- c(0, cumsum(sqrt(diff(x[idx])^2 + diff(y[idx])^2 + diff(z[idx])^2)))
  span_u <- u[length(u)] - u[1]
  if (d[length(d)] > 0 && span_u > 0) {
    out$deviation <- max(abs((u - u[1]) / span_u - d / d[length(d)]))
  }
  out
}

# Per-lap data-quality report, shown before the lap filters are applied.
#   Sampling rate (Hz)     = (samples with a time stamp - 1) / (last - first time stamp)
#   Median / max interval  = between consecutive distinct time stamps (ms)
#   Long gaps              = intervals > gap_factor x median interval
#   Missing values         = % NA per channel (only channels with any NA listed)
#   Car (nearly) stopped   = minimum speed < stop_speed_kmh
#   Moves backwards        = largest drop in track position, in time order
#                            (start/finish crossing removed), > back_tol lap
#                            fraction; lap comparison cannot represent this
#   Position coverage      = position_check() coverage (% of lap); flagged if
#                            < pos_cov_min
#   Position vs distance   = position_check() deviation (% of lap); flagged if
#                            > pos_dev_max
lap_quality_tbl <- function(telemetry, stop_speed_kmh = 10, back_tol = 0.005, gap_factor = 5,
                            pos_cov_min = 0.95, pos_dev_max = 0.05) {
  if (!has_cols(telemetry, c("lapCount", "lapTime"))) return(tibble(lapCount = integer(0)))
  channels <- intersect(setdiff(required_fields, "lapCount"), names(telemetry))

  one_lap <- function(d) {
    t <- sort(unique(d$lapTime[is.finite(d$lapTime)]))
    dt <- diff(t)
    med_dt <- if (length(dt) > 0) median(dt) else NA_real_
    span_s <- if (length(t) > 1) (t[length(t)] - t[1]) / 1000 else NA_real_

    na_pct <- vapply(channels, \(cn) mean(is.na(d[[cn]])) * 100, numeric(1))
    miss <- na_pct[na_pct > 0]
    missing_txt <- if (length(miss) > 0) {
      paste0(names(miss), " ", ifelse(miss >= 100, "all", sprintf("%.1f%%", miss)), collapse = "; ")
    } else {
      "none"
    }

    back <- NA_real_
    if ("carPositionNormalized" %in% names(d)) {
      idx <- which(is.finite(d$carPositionNormalized) & is.finite(d$lapTime))
      idx <- idx[order(d$lapTime[idx])]
      p <- d$carPositionNormalized[idx]
      if (length(p) > 1) {
        jump <- c(0, diff(p))
        u <- p + cumsum(jump < -0.5) - cumsum(jump > 0.5)
        back <- max(cummax(u) - u)
      }
    }

    pc <- list(coverage = NA_real_, deviation = NA_real_)
    if (has_cols(d, c("carPositionNormalized", "carCoordinatesX", "carCoordinatesZ"))) {
      pc <- position_check(d$carPositionNormalized, d$lapTime, d$carCoordinatesX, d$carCoordinatesZ,
                           if ("carCoordinatesY" %in% names(d)) d$carCoordinatesY else NULL)
    }

    lap_s <- if ("carPositionNormalized" %in% names(d)) {
      lap_time_estimate(d$carPositionNormalized, d$lapTime) / 1000
    } else {
      safe_max(d$lapTime) / 1000
    }
    pit_n <- if ("isInPit" %in% names(d)) sum(d$isInPit == 1, na.rm = TRUE) else 0L
    n_gaps <- if (is.finite(med_dt)) sum(dt > gap_factor * med_dt) else 0L
    min_speed <- if ("speedKmh" %in% names(d)) safe_min(d$speedKmh) else NA_real_
    na_time <- sum(is.na(d$lapTime))

    flags <- c(
      if (pit_n > 0) "pit samples",
      if (n_gaps > 0) sprintf("%d long gap(s), longest %.0f ms", n_gaps, max(dt)),
      if (na_time > 0) "missing time stamps",
      if (any(miss >= 100)) paste0("channel missing: ", paste(names(miss)[miss >= 100], collapse = ", ")),
      if (is.finite(min_speed) && min_speed < stop_speed_kmh) "car (nearly) stopped",
      if (is.finite(back) && back > back_tol) "moves backwards",
      if (is.finite(pc$coverage) && pc$coverage < pos_cov_min)
        sprintf("position covers only %.0f%% of a lap", pc$coverage * 100),
      if (is.finite(pc$deviation) && pc$deviation > pos_dev_max)
        sprintf("position differs from distance driven by up to %.1f%% of a lap", pc$deviation * 100)
    )

    tibble(
      samples = nrow(d),
      lap_time_s = lap_s,
      sampling_hz = if (is.finite(span_s) && span_s > 0) (length(t) - 1) / span_s else NA_real_,
      median_interval_ms = med_dt,
      max_interval_ms = if (length(dt) > 0) max(dt) else NA_real_,
      long_gaps = n_gaps,
      missing_time_stamps = na_time,
      min_speed_kmh = min_speed,
      max_backward_pct_lap = back * 100,
      position_coverage_pct = pc$coverage * 100,
      position_vs_distance_max_dev_pct = pc$deviation * 100,
      missing_values = missing_txt,
      flags = if (length(flags) > 0) paste(flags, collapse = "; ") else "OK"
    )
  }

  telemetry |>
    group_by(lapCount) |>
    group_modify(\(d, key) one_lap(d)) |>
    ungroup() |>
    arrange(lapCount)
}

apply_preprocess_filters <- function(
    telemetry,
    lap_offset = 1L,
    remove_laps_with_pit = TRUE,
    laps_to_exclude = integer(0),
    max_lap_time_seconds = NULL,
    min_start_speed_kmh = 100,
    min_overall_speed_kmh = 10
) {
  out <- telemetry
  
  if ("lapCount" %in% names(out) && !("originalLapCount" %in% names(out))) {
    out <- out |> mutate(originalLapCount = lapCount)
  }
  
  if (!is.null(laps_to_exclude) && length(laps_to_exclude) > 0 && "originalLapCount" %in% names(out)) {
    out <- out |> filter(!originalLapCount %in% laps_to_exclude)
  }
  
  if ("lapCount" %in% names(out) && !is.null(lap_offset) && is.finite(lap_offset)) {
    out <- out |> mutate(lapCount = as.integer(lapCount) + as.integer(lap_offset))
  }
  
  if (remove_laps_with_pit && "isInPit" %in% names(out) && "lapCount" %in% names(out)) {
    pit_laps <- out |>
      filter(isInPit == 1) |>
      pull(lapCount) |>
      unique()
    
    if (length(pit_laps) > 0) out <- out |> filter(!lapCount %in% pit_laps)
  }
  
  if (!is.null(max_lap_time_seconds) && is.finite(max_lap_time_seconds) &&
      "lapCount" %in% names(out) && "lapTime" %in% names(out)) {
    
    slow_laps <- out |>
      group_by(lapCount) |>
      summarise(
        lap_time_sec = {
          vals <- lapTime[!is.na(lapTime)]
          if (length(vals) == 0) NA_real_ else max(vals) / 1000
        },
        .groups = "drop"
      ) |>
      filter(is.finite(lap_time_sec) & lap_time_sec > max_lap_time_seconds) |>
      pull(lapCount)
    
    if (length(slow_laps) > 0) out <- out |> filter(!lapCount %in% slow_laps)
  }
  
  
  if (!is.null(min_start_speed_kmh) && is.finite(min_start_speed_kmh) &&
      "lapCount" %in% names(out) && "speedKmh" %in% names(out)) {
    
    bad_start_laps <- out |>
      group_by(lapCount) |>
      summarise(
        start_speed_kmh = {
          vals <- speedKmh[!is.na(speedKmh)]
          if (length(vals) == 0) NA_real_ else vals[1]
        },
        .groups = "drop"
      ) |>
      filter(is.finite(start_speed_kmh) & start_speed_kmh < min_start_speed_kmh) |>
      pull(lapCount)
    
    if (length(bad_start_laps) > 0) out <- out |> filter(!lapCount %in% bad_start_laps)
  }
  
  if (!is.null(min_overall_speed_kmh) && is.finite(min_overall_speed_kmh) &&
      "lapCount" %in% names(out) && "speedKmh" %in% names(out)) {
    
    bad_min_speed_laps <- out |>
      group_by(lapCount) |>
      summarise(
        min_speed_kmh = {
          vals <- speedKmh[!is.na(speedKmh)]
          if (length(vals) == 0) NA_real_ else min(vals)
        },
        .groups = "drop"
      ) |>
      filter(is.finite(min_speed_kmh) & min_speed_kmh < min_overall_speed_kmh) |>
      pull(lapCount)
    
    if (length(bad_min_speed_laps) > 0) out <- out |> filter(!lapCount %in% bad_min_speed_laps)
  }
  
  out
}
