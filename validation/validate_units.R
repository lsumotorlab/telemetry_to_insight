# Unit detection in the harmonizer (reviewer revision, R2 minor 2)
# ===================================================================
# Run from the project folder (open telemetry_to_insight.Rproj):
#   source("validation/validate_units.R")
#
# harmonize_telemetry() is run on small synthetic files with known units, and
# the harmonized values are compared with the true values (km/h, ms, G, 0-1):
#   Speed          km/h vs m/s
#   Lap time       ms vs s
#   Accelerations  G vs m/s²
#   Track position 0-1 vs meters
#   Gas/brake      0-1 vs 0-100, and an unknown scale (0-255) that must be
#                  left unchanged and flagged
# Each case is run with Auto-detect; the gas/brake cases also with the manual
# settings.

suppressPackageStartupMessages({
  library(tidyverse)
})

root <- if (file.exists("telemetry_core.R")) "." else if (file.exists("../telemetry_core.R")) ".." else
  stop("Run this from the project folder (open telemetry_to_insight.Rproj).", call. = FALSE)
source(file.path(root, "telemetry_core.R"))

# one lap, 50 Hz, true values in the toolkit's units
n <- 3000
true <- tibble(
  lapCount              = 1L,
  lapTime               = (seq_len(n) - 1) * 20,                            # ms
  carPositionNormalized = (seq_len(n) - 1) / n,                             # 0-1
  speedKmh              = 160 - 80 * cos(2 * pi * (seq_len(n) - 1) / n),    # km/h
  accGHorizontal        = 1.8 * sin(6 * pi * (seq_len(n) - 1) / n),         # G
  accGFrontal           = 1.2 * cos(4 * pi * (seq_len(n) - 1) / n),         # G
  gas                   = pmin(1, pmax(0, 1.2 * sin(2 * pi * (seq_len(n) - 1) / n * 5))),
  brake                 = pmin(1, pmax(0, -1.2 * sin(2 * pi * (seq_len(n) - 1) / n * 5)))
)
mapping <- setNames(as.list(names(true)), names(true))

run <- function(input, ...) harmonize_telemetry(input, mapping, ...)

# max |harmonized - true| for one variable
err <- function(h, v, expected = true[[v]]) max(abs(h[[v]] - expected))

cases <- list(
  list("Speed in km/h, auto",          true,                                          list(), "speedKmh"),
  list("Speed in m/s, auto",           mutate(true, speedKmh = speedKmh / 3.6),       list(), "speedKmh"),
  list("Lap time in ms, auto",         true,                                          list(), "lapTime"),
  list("Lap time in s, auto",          mutate(true, lapTime = lapTime / 1000),        list(), "lapTime"),
  list("Lateral acc. in G, auto",      true,                                          list(), "accGHorizontal"),
  list("Lateral acc. in m/s², auto",   mutate(true, accGHorizontal = accGHorizontal * 9.81), list(), "accGHorizontal"),
  list("Longitudinal acc. in m/s², auto", mutate(true, accGFrontal = accGFrontal * 9.81), list(), "accGFrontal"),
  list("Position in meters, auto",     mutate(true, carPositionNormalized = carPositionNormalized * 1956), list(), "carPositionNormalized"),
  list("Gas 0-1, auto",                true,                                          list(), "gas"),
  list("Gas 0-100, auto",              mutate(true, gas = gas * 100),                 list(), "gas"),
  list("Brake 0-1, auto",              true,                                          list(), "brake"),
  list("Brake 0-100, auto",            mutate(true, brake = brake * 100),             list(), "brake"),
  list("Gas 0-100, manual 0-100",      mutate(true, gas = gas * 100),                 list(pedal_unit = "percent"),  "gas"),
  list("Gas 0-1, manual 0-1",          true,                                          list(pedal_unit = "fraction"), "gas")
)

res <- map_dfr(cases, \(cs) {
  h <- do.call(run, c(list(cs[[2]]), cs[[3]]))
  # position in meters is divided by its maximum, so the last sample becomes 1
  expected <- if (cs[[4]] == "carPositionNormalized") cs[[2]][[cs[[4]]]] / max(cs[[2]][[cs[[4]]]]) else true[[cs[[4]]]]
  tibble(case = cs[[1]], max_abs_error = err(h, cs[[4]], expected)) |>
    mutate(pass = max_abs_error < 1e-9)
})

# unknown pedal scale (0-255): values unchanged and a warning in the report
raw255 <- mutate(true, brake = brake * 255)
h255   <- run(raw255)
rep255 <- attr(h255, "units_report")
res <- bind_rows(res, tibble(
  case = "Brake 0-255, auto: unchanged and flagged",
  max_abs_error = err(h255, "brake", raw255$brake),
  pass = max_abs_error < 1e-9 &&
    startsWith(rep255$Result[rep255$Variable == field_labels[["brake"]]], "WARNING")
))

print(res, n = Inf, width = Inf)
cat("\nUnits report for the 0-255 brake case:\n")
print(rep255, width = Inf)
cat(sprintf("\n%d / %d checks passed\n", sum(res$pass), nrow(res)))
