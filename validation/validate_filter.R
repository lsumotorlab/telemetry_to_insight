# Validation of the low-pass filter (reviewer revision, R1.9 / R2.5)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj), then:
#   source("validation/validate_filter.R")
#
# lowpass_lap() (telemetry_core.R) filters one lap: it resamples the signal to
# an even time grid at the mean sampling rate, bridges missing values and
# dropped samples, applies the zero-phase 2nd-order Butterworth filter and
# returns values at the original time stamps (NA stays NA).
#
# Test signal (60 s): 1 Hz "driving" component (amplitude 1) + 20 Hz "noise"
# component (amplitude 0.5). Cutoff 5 Hz.
#   A uniform 100 Hz          -> identical to lowpass_filter() on the raw vector
#   B intervals 3/6 ms alternating -> sampling rate = 1000 / 4.5 = 222.2 Hz
#                                    (1000 / median interval would give 333 Hz)
#   C, D, E: uniform 100 Hz with 5% NA / irregular 5-15 ms / 2 s dropped block
#   For B-E: NA positions preserved, no other NA, 1 Hz component kept
#   (amplitude ratio 0.95-1.05) and 20 Hz component removed (< 5% left),
#   both estimated by least squares on the filtered output.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))
vdir <- file.path(root, "validation")

set.seed(7)
cutoff <- 5
signal_at <- \(t) sin(2 * pi * 1 * t / 1000) + 0.5 * sin(2 * pi * 20 * t / 1000)

results <- tibble()
check <- function(case, item, expected, calculated, pass) {
  results <<- bind_rows(results, tibble(case = case, check = item, expected = expected,
                                        calculated = calculated, pass = pass))
}

# Amplitude of the f Hz component in y(t) by least squares (sin + cos terms),
# excluding the first and last second (filter start-up at the edges)
amp_of <- function(y, t, f) {
  ok <- is.finite(y) & t > 1000 & t < max(t) - 1000
  X <- cbind(sin(2 * pi * f * t[ok] / 1000), cos(2 * pi * f * t[ok] / 1000),
             sin(2 * pi * 1 * t[ok] / 1000), cos(2 * pi * 1 * t[ok] / 1000), 1)
  b <- qr.solve(X, y[ok])
  sqrt(b[1]^2 + b[2]^2)
}

# A: uniform sampling -> same result as the plain filter
t <- seq(0, 60000, by = 10)
x <- signal_at(t)
y <- lowpass_lap(x, t, cutoff)
check("A uniform 100 Hz", "max |lowpass_lap - lowpass_filter|", 0,
      max(abs(as.numeric(y) - lowpass_filter(x, cutoff, 100))), max(abs(as.numeric(y) - lowpass_filter(x, cutoff, 100))) < 1e-9)
check("A uniform 100 Hz", "sampling rate (Hz)", 100, attr(y, "fs"), abs(attr(y, "fs") - 100) < 1e-6)

cases <- list(
  "B alternating 3/6 ms" = function() {
    t <- cumsum(c(0, rep(c(3, 6), length.out = 13332)))
    list(t = t, x = signal_at(t), fs = 1000 / 4.5)
  },
  "C 100 Hz, 5% NA" = function() {
    t <- seq(0, 60000, by = 10)
    x <- signal_at(t)
    x[sample(length(x), round(0.05 * length(x)))] <- NA
    list(t = t, x = x, fs = 100)
  },
  "D irregular 5-15 ms" = function() {
    t <- cumsum(c(0, runif(5999, 5, 15)))
    list(t = t, x = signal_at(t), fs = 5999 / (t[length(t)] / 1000))
  },
  "E 100 Hz, 2 s dropped" = function() {
    t <- seq(0, 60000, by = 10)
    t <- t[t < 30000 | t >= 32000]
    list(t = t, x = signal_at(t), fs = (length(t) - 1) / 60)
  }
)

for (nm in names(cases)) {
  d <- cases[[nm]]()
  y <- lowpass_lap(d$x, d$t, cutoff)
  yn <- as.numeric(y)
  check(nm, "sampling rate (Hz)", d$fs, attr(y, "fs"), abs(attr(y, "fs") - d$fs) < 0.01)
  check(nm, "NA positions preserved", sum(is.na(d$x)), sum(is.na(yn)), identical(is.na(yn), is.na(d$x)))
  ok <- is.finite(yn) & d$t > 1000 & d$t < max(d$t) - 1000
  X <- cbind(sin(2 * pi * d$t[ok] / 1000), cos(2 * pi * d$t[ok] / 1000), 1)
  b <- qr.solve(X, yn[ok])
  a1 <- sqrt(b[1]^2 + b[2]^2)
  a20 <- amp_of(yn, d$t, 20)
  check(nm, "1 Hz amplitude kept (ratio)", 1, a1, a1 > 0.95 & a1 < 1.05)
  check(nm, "20 Hz amplitude left (fraction of 0.5)", 0, a20 / 0.5, a20 / 0.5 < 0.05)
}

options(pillar.sigfig = 5)
print(results, n = Inf, width = Inf)
cat("\n", sum(results$pass), "/", nrow(results), "checks passed\n")
write_csv(results, file.path(vdir, "validation_filter_results.csv"))
cat("Results saved to validation/validation_filter_results.csv\n")
