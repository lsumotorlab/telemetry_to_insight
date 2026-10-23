# Telemetry Analysis Suite — Usage Guide

A collection of interactive R/Shiny tools for racing telemetry analysis. The toolkit consists of six Shiny apps and a shared function library:

| File | Role |
|------|------|
| `telemetry_harmonizer_and_preprocessing.R` | Step 1: map raw columns to standard names, check units and data quality, filter laps |
| `create_synthetic_lap.R` | Step 2: build a composite reference lap from the fastest sectors |
| `telemetry_lap_level_summary_interactive.R` | Step 3: per-lap statistics, summary statistics, lap trends |
| `track_mapping_interactive.R` | Step 4: single-lap track maps |
| `track_mapping_compare_interactive.R` | Step 5: lap-to-lap comparison |
| `corner_analysis_interactive.R` | Corner-by-corner analysis (any time after Step 1) |
| `telemetry_core.R` | Shared function library: every calculation used by the apps (lap time, time-weighted metrics, sector and corner times, filtering, composite lap, harmonization), with the formulas documented in comments. Each app loads it automatically. |
| `validation/` | Validation datasets, generator scripts, check scripts and their results (see [Validation](#validation)) |

---

## Requirements

- **R ≥ 4.1** (the code uses the native pipe `|>` and the `\(x)` function shorthand). RStudio is recommended but not required.
- **R packages:** shiny, tidyverse (dplyr, tidyr, readr, purrr, ggplot2, tibble), plotly, gt, scales, viridis, lubridate, signal.

The toolkit was tested with the following versions:

| Software | Version |
|----------|---------|
| R | 4.4.1 |
| shiny | 1.13.0 |
| dplyr | 1.1.4 |
| tidyr | 1.3.1 |
| readr | 2.1.5 |
| purrr | 1.1.0 |
| ggplot2 | 3.5.2 |
| tibble | 3.2.1 |
| plotly | 4.11.0 |
| gt | 1.3.0 |
| scales | 1.3.0 |
| viridis | 0.6.5 |
| lubridate | 1.9.3 |
| signal | 1.8.1 |

## Installation

1. Download the repository (GitHub: *Code → Download ZIP*, or the archived release on Zenodo, DOI in the paper) and unzip it, or clone it with git.
2. Open `telemetry_to_insight.Rproj` in RStudio (or set the working directory to the repository folder).
3. Install the packages once:

```r
install.packages(c(
  "shiny", "tidyverse", "plotly", "gt", "scales",
  "viridis", "lubridate", "signal"
))
```

4. Start an app by sourcing its script, e.g. `source("telemetry_harmonizer_and_preprocessing.R")`. Each script loads `telemetry_core.R` and launches its app; the app opens in the RStudio viewer or your browser. Stop the app (Esc in the console, or the stop button) before starting another one, and restart it after updating the code.

---

## Supported input

- **Format:** a CSV file with a header row and **one row per telemetry sample** (files up to 500 MB). Column names and order do not matter: in Step 1 you map your columns to the standard names below (auto-suggested from common names).
- **Tested with:** Assetto Corsa telemetry (~240 Hz). Other simulators and data loggers can be used if they provide the required channels; check the mapping, the *Units used* table and the lateral-acceleration check in Step 1.
- **Conventions expected by the toolkit:**
  - `lapCount` identifies the lap of each sample.
  - `lapTime` is the time elapsed since the start of the current lap (resets to 0 at each start/finish crossing); milliseconds or seconds (auto-detected).
  - `carPositionNormalized` is the position along the lap, from 0 at the start/finish line to 1 at the end of the lap (other ranges, e.g. meters, are rescaled automatically).
  - Speed in km/h or m/s, accelerations in G or m/s² (auto-detected, or set manually).
  - Sampling may be irregular and may contain missing values: time-weighted metrics, interpolated timing and the filter handle both (see `telemetry_core.R` and the validation scripts).
- **Axis labels:** some loggers label the acceleration axes differently from their physical meaning (e.g., lateral acceleration stored as `accGVertical`). Step 1 checks which column follows steering × speed² and warns if the mapped lateral column does not.

### Required columns per app

The harmonized file from Step 1 always contains all standard columns (unmapped ones are empty). The table lists the columns that must contain values; other columns are optional and only the plots or metrics that use them are affected.

| App | Must contain values | Used if present |
|-----|---------------------|-----------------|
| Step 1 Harmonize & Preprocess | `lapCount`, `lapTime` | `carPositionNormalized` (lap time, backward-movement check), `speedKmh`, `steer`, `isInPit`, accelerations (checks and filters) |
| Step 2 Composite reference lap | `lapCount`, `lapTime`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ` | all other channels (carried into the composite lap) |
| Step 3 Lap Summary | `lapCount`, `lapTime`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ` | `speedKmh`, `gas`, `brake`, `isAbsInAction`, `isTcInAction`, `accGHorizontal`, `accGFrontal` (the corresponding metrics) |
| Step 4 Track Mapping | `lapCount`, `carCoordinatesX`, `carCoordinatesZ` | each plot needs its channel(s), e.g. `speedKmh`, `gear`, `brake`, `gas`, `carCoordinatesY` (elevation), `accGHorizontal` and `lapTime` (lateral G over time) |
| Step 5 Lap Comparison | `lapCount`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ` | `lapTime` (lap and sector times), the compared channel |
| Corner Analysis | `lapCount`, `lapTime`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ` | `speedKmh`, `gas`, `brake`, `accGHorizontal`, `accGFrontal` |

---

## Recommended Workflow

```
Raw CSV
   ↓
[1] telemetry_harmonizer_and_preprocessing.R        → harmonized & filtered CSV
   ↓
[2] create_synthetic_lap.R                          → CSV with composite reference lap appended (optional)
   ↓
[3] telemetry_lap_level_summary_interactive.R       → lap statistics & trends
   ↓
[4] track_mapping_interactive.R                     → single-lap track maps
   ↓
[5] track_mapping_compare_interactive.R             → lap-to-lap comparison

Any harmonized CSV can also be passed directly to:
[+] corner_analysis_interactive.R                  → focused corner-by-corner analysis
```

---

## Step 1 — Harmonize & Preprocess

**File:** `telemetry_harmonizer_and_preprocessing.R`

**Run:**
```r
source("telemetry_harmonizer_and_preprocessing.R")   # launches the app
```

**Purpose:** Maps your raw CSV columns to the standard column names expected by all downstream scripts, then applies quality filters to select clean racing laps.

### Tabs

| Tab | What it does |
|-----|-------------|
| **1 Preview** | Shows raw file contents and detected column names |
| **2 Harmonize Mapping** | Maps raw columns to standard names (auto-suggested, manually adjustable). Click **Apply harmonization**. |
| **3 Preprocess** | Shows the data-quality report per lap, then the lap overview before and after filtering. Click **Apply preprocessing**. |

### Standard column names (output)

| Column | Description |
|--------|-------------|
| `lapCount` | Lap number |
| `lapTime` | Lap time in milliseconds (always stored as ms after harmonization; input unit auto-detected or manually specified) |
| `carPositionNormalized` | Track position 0–1 |
| `carCoordinatesX/Y/Z` | 3D car position |

> **Note:** The meaning of X, Y, and Z axes is not consistent across telemetry sources.
            In Assetto Corsa, X and Z represent the 2D track plane (horizontal position) and Y represents elevation.
            Other simulators or telemetry systems may assign these axes differently.
            Verify the axis convention for your specific data source before interpreting coordinate-based visualizations.

| `speedKmh` | Speed (km/h) |
| `engineRPM` | Engine RPM |
| `gear` | Current gear |
| `gas` | Throttle input 0–1 |
| `brake` | Brake input 0–1 |
| `steer` | Steering input |
| `isAbsInAction` | ABS active (0/1) |
| `isTcInAction` | TC active (0/1) |
| `isInPit` | In pit lane (0/1) |
| `accGHorizontal` | Lateral acceleration in G (left/right cornering force) |
| `accGFrontal` | Longitudinal acceleration in G (positive = accelerating, negative = braking). Not filtered; peak values (e.g., Max Accel G) can reflect brief spikes such as gear shifts. |

### Harmonization options

- **Input speed unit** — choose km/h, m/s, or Auto-detect. Auto-detect assumes km/h when the 99th percentile exceeds 120 or the median exceeds 50 (implausible in m/s); otherwise m/s, multiplied by 3.6.
- **Input lap time unit** — choose Milliseconds, Seconds, or Auto-detect. Auto-detect assumes seconds when the median lap time is ≤ 1000 (since no real racing lap takes more than 1000 seconds) and converts by multiplying by 1000. Use the manual options if your data is ambiguous.
- **Input acceleration unit** — choose how `accGHorizontal` and `accGFrontal` are stored in your source file. Options: Auto-detect, G forces, m/s². Auto-detect assumes m/s² when the 99th percentile of absolute values exceeds 10 (since 10 G is physically impossible, but 10 m/s² ≈ 1 G is normal). m/s² values are divided by 9.81 to convert to G.
- **Track position** — rescaled to 0–1 by dividing by its maximum when the maximum exceeds 1.1 (e.g., meters or %).
- **Units used** (tab 2, after *Apply harmonization*) — a table showing, for each variable, the unit setting, the input statistic that auto-detect used, the rule, and the result. If a result is wrong, select the unit manually and apply harmonization again.
- **Lateral-acceleration check** (tab 2) — correlates each acceleration column with steering × speed²; lateral acceleration should follow it closely. A warning names a better-fitting column if the logger labels the axes differently.
- **Data quality per lap** (tab 3) — sampling rate, median/maximum interval, long gaps, missing time stamps, % missing values per channel, minimum speed, backward movement, a check of the track-position channel, and flags (e.g., pit samples, car stopped, moves backwards). Downloadable as CSV.
- **Track-position check** (part of the data-quality report) — the toolkit does not calculate normalized track position; it uses the position channel of the data source. Each lap's position is checked for **coverage** (range of position within the lap; flagged below 95%) and against the **distance driven**, calculated independently from the car coordinates (largest difference between position and distance fraction, as % of a lap; flagged above 5%). Small differences are expected, because position is measured along the track's reference line and distance along the driven line.

### Preprocessing filters

| Filter | Description |
|--------|-------------|
| Lap count offset | Shifts all lap numbers by a constant (default +1) |
| Remove pit laps | Removes entire laps that contain any pit lane rows |
| Manual exclusion | Exclude specific laps by original lap number (comma-separated) |
| Max lap time | Removes laps slower than a threshold (e.g. out-laps, cool-down laps) |
| Min speed at lap start | Removes laps where the car was not already up to speed at lap start |
| Min overall speed | Removes laps containing any datapoint below a minimum speed |

### Downloads

| Button | Contents |
|--------|----------|
| **Download harmonized CSV** | All harmonized data, before lap filtering |
| **Download filtered CSV** | Filtered data, with your chosen column selection |
| **Download minimal CSV** | Filtered data, core columns only |

> **Use the minimal or filtered CSV as input for all subsequent steps.**

---

## Step 2 — Create a Composite Reference Lap

**File:** `create_synthetic_lap.R` (file and function names keep the earlier term "synthetic")

**Run:**
```r
source("create_synthetic_lap.R")   # launches the app
```

**Purpose:** Constructs a composite reference lap by dividing the track into sectors and taking each sector from the lap that was fastest in it. Each sector keeps the timing and channel values of its source lap (sector times from interpolated boundary crossings); the composite lap time is the sum of the best sector times. The result is appended to your dataset as an additional lap and marked in a `.synthetic` column. It is a reconstruction from separate laps, not a lap that was driven, and transitions at sector boundaries need not be physically continuous.

### Workflow (three sequential windows)

**Window 1 — Upload & configure**

1. Upload the filtered/minimal CSV from Step 1.
2. Optionally flip the track horizontally or vertically so it displays correctly.
3. Select the **reference lap** — the lap whose track outline will be used to draw sector boundaries.
4. Set **Resample bins** (number of data points in the composite lap; defaults to the number of samples in the first lap).
5. Set **Min samples per segment per lap** — minimum data points required for a sector to be eligible.
6. Click **Continue to segment selection**.

**Window 2 — Define sector boundaries**

- An interactive track map of the reference lap is shown.
- **Click on the track** to place sector boundary points. Each click snaps to the nearest track position.
- Use **Undo last** or **Clear** to remove points.
- You need at least 2 boundary points (which create 2 or more sectors wrapping around the full lap).
- Click **Done** when finished.

**Window 3 — Preview & download**

- A track map shows which source lap was used for each sector.
- Click **Download updated CSV** to save the dataset with the composite reference lap appended.
- The composite lap receives the next available lap number (max lap + 1).

> The downloaded CSV from this step is the recommended input for Steps 3–5 and Corner Analysis when you want to include the composite reference lap. It is labeled in all apps and excluded from summary and consistency statistics.

---

## Step 3 — Lap Statistics Summary

**File:** `telemetry_lap_level_summary_interactive.R`

**Run:**
```r
source("telemetry_lap_level_summary_interactive.R")   # launches the app
```

**Purpose:** Displays per-lap performance statistics and summary statistics across all selected laps. Useful for identifying outliers, tracking consistency, and comparing effort-related metrics.

### Sidebar controls

| Control | Description |
|---------|-------------|
| Upload CSV | Load the telemetry file (harmonized, or with the composite reference lap) |
| Flip horizontal / vertical | Mirror track coordinates if needed |
| Full throttle threshold | % gas above which a sample counts as "full throttle" |
| Braking threshold | % brake above which a sample counts as "braking" |
| Lateral G low-pass cutoff | Cutoff (Hz, default 5; 0 = no filter) used for "Max Lateral G (filtered)". "Max Lateral G (raw)" is always unfiltered. There is no single correct cutoff: report the value used (shown in the table subtitle). |
| Choose laps | Checkboxes to include/exclude individual laps |
| Designate the composite reference lap | Mark the composite reference lap (pre-selected if the file marks it) — it is excluded from summary statistics and lap trends but shown, labeled, in the lap table |

### Tabs

| Tab | Contents |
|-----|---------|
| **Lap Statistics** | Per-lap table: lap time (interpolated return to the lap's own timing line), time-weighted full throttle %, braking %, brake/throttle overlap %, avg/max/min speed, ABS %, TC %, max lateral G (raw and filtered), max accel G, max brake G (unfiltered), and the lap's sampling rate (Hz). **Export CSV** button saves the displayed table. |
| **Summary Statistics** | Mean, SD, Min, Max across all selected driven laps (composite reference lap excluded) (statistics as rows, variables as columns). **Export CSV** button saves the displayed table. |
| **Lap Trend** | Line chart of any metric across laps — useful for spotting fatigue or warm-up effects |

---

## Step 4 — Track Mapping (Single Lap)

**File:** `track_mapping_interactive.R`

**Run:**
```r
source("track_mapping_interactive.R")   # launches the app
```

**Purpose:** Visualizes telemetry channels overlaid on the track map for a single lap. Up to 4 plots can be displayed simultaneously.

### Sidebar controls

| Control | Description |
|---------|-------------|
| Upload CSV | Load telemetry file |
| Select lap | Choose which lap to display |
| Flip horizontal / vertical | Mirror coordinates |
| Select up to 4 plots | Choose plot types from the list below |
| Gear encoding | *(visible when Gear map is selected)* Remap raw gear values where 0=R, 1=N, 2–7 = gears 1–6 (Assetto Corsa encoding) |
| Brake threshold | *(visible when Braking zones is selected)* Minimum brake input to count as braking |
| Throttle threshold | *(visible when Full throttle zones is selected)* Minimum gas input to count as full throttle |
| Lateral G low-pass cutoff (Hz) | *(visible when Lateral G map or Lateral G over time is selected)* Butterworth 2nd-order zero-phase filter applied to `accGHorizontal`. Range 0–20 Hz; set to 0 for the raw signal. Lower cutoffs remove more rapid fluctuations (e.g., kerbs, bumps, vibration) but also smooth genuine rapid changes and lower peak values. There is no single correct cutoff: choose one for your data and report the value used. The filter resamples each lap to an even time grid at its mean sampling rate, bridges missing values, and keeps missing samples missing; the plot subtitle shows the cutoff and the sampling rate. |
| Overlap thresholds | *(visible when Brake/Throttle Overlap is selected)* Separate throttle and brake thresholds defining what counts as simultaneous overlap |
| Show hover data | Enable interactive tooltips on the map |

### Available plot types

| Plot | What is shown |
|------|--------------|
| Track outline | Plain track layout |
| Speed map | Track colored by speed (red–yellow–green gradient) |
| Gear map | Track colored by gear engaged |
| Braking zones | Points where brake input exceeds threshold, colored by brake pressure |
| Full throttle zones | Points at full throttle, colored by speed |
| Brake/Throttle Overlap | Points where brake and throttle are simultaneously active, colored by flag |
| ABS activation points | Locations where ABS was triggered |
| TC activation points | Locations where traction control was triggered |
| Elevation profile | Elevation vs. distance along the lap |
| Track with elevation color | Track map colored by elevation (cividis palette) |
| Lateral G map | Track colored by lateral acceleration (`accGHorizontal`). Blue = left cornering, red = right cornering. Supports low-pass filtering. |
| Longitudinal G map | Track colored by longitudinal acceleration (`accGFrontal`). Green = accelerating, red = braking. |
| Lateral G over time | Lateral acceleration (`accGHorizontal`) plotted as a time series over lap time. Blue = left, red = right. Supports low-pass filtering. |
| Steer over time | Steering input (`steer`) plotted as a time series over lap time. Blue = left, red = right. |

---

## Step 5 — Lap Comparison

**File:** `track_mapping_compare_interactive.R`

**Run:**
```r
source("track_mapping_compare_interactive.R")   # launches the app
```

**Purpose:** Side-by-side comparison of two laps. Shows how a chosen telemetry attribute differs between the reference and comparison lap, both as a track map and as a trace over normalized lap position.

### Sidebar controls

| Control | Description |
|---------|-------------|
| Upload CSV | Load telemetry file |
| Select Lap A / Lap B | Choose the two laps to compare |
| Reference lap | Which of A or B is the reference (delta = comparison − reference) |
| Reference lap is the composite reference lap | Labels the reference lap as the composite reference lap in the plot titles |
| Flip horizontal / vertical | Mirror coordinates |
| Comparison attribute | The telemetry channel to compare (speed, RPM, gear, gas, brake, steer, ABS, TC, lateral G, longitudinal G). When **lateral G** or **steer** is selected, `accGHorizontal` and `steer` are compared as their **absolute value** — left/right sign is dropped so both laps reflect cornering force magnitude and steering magnitude only. |
| Gear encoding | *(visible when Gear is selected)* Remap raw gear values where 0=R, 1=N, 2–7 = gears 1–6 |
| Interpolation points | Resolution of the common lap axis used for delta calculation (default 3000; fewer points can lower short peaks, see validation/validate_interpolation_peaks.R) |
| Show hover data | Enable interactive tooltips |

### Four fixed panels

| Panel | Description |
|-------|-------------|
| **Reference map** | Track map colored by the selected attribute for the reference lap |
| **Comparison map** | Track map colored by the selected attribute for the comparison lap |
| **Delta map** | Track map colored by the difference (comparison − reference). Green/red = faster/slower for speed; blue/red for other attributes |
| **Attribute trace** | Line chart of both laps over normalized lap position (0–100%) |

Lap times for both selected laps are displayed above the plots in `M:SS.ss` format.

---

## Corner Analysis

**File:** `corner_analysis_interactive.R`

**Run:**
```r
source("corner_analysis_interactive.R")   # launches the app
```

**Purpose:** Deep-dive analysis of a single corner across multiple laps. You interactively define the corner boundaries on the track map and then explore traction, speed, throttle/brake, time deltas, and consistency metrics for every selected lap. Can be used at any point after Step 1 — no composite reference lap required.

### Workflow (three sequential windows)

**Window 1 — Upload & Setup**

1. Upload the harmonized/filtered CSV from Step 1 (or Step 2 if you want the composite reference lap included; it is labeled and excluded from consistency statistics).
2. Optionally flip coordinates horizontally (X) or vertically (Z) to orient the track correctly.
3. Select a **reference lap** — its track outline is used for corner selection in Window 2.
4. Click **Continue to corner selection**.

**Window 2 — Define Corner**

- An interactive track map of the reference lap is displayed.
- **Click once** to place the corner **entry** point (snapped to the nearest `carPositionNormalized` value; shown in green).
- **Click again** to place the corner **exit** point (shown in red). The selected section is highlighted in orange.
- A corner may cross the start/finish line (exit before entry in lap position). The end of each lap is then joined to the start of the same lap, as for sector times in Step 2, and the corner time is [t(lap end) − t(entry)] + [t(exit) − t(lap start)].
- Use **Reset** to start over, then **Confirm corner** to proceed.

**Window 3 — Analysis**

The main analysis app. The sidebar lets you adjust the pre/post buffer, select laps, choose a reference lap for deltas, and draw an optional reference circle.

#### Sidebar controls

| Control | Description |
|---------|-------------|
| Seconds before entry (plots only) | Extra data shown before the corner entry point in traces and plots (0–10 s). Does not affect Corner Summary or Consistency metrics. |
| Seconds after exit (plots only) | Extra data shown after the corner exit point in traces and plots (0–10 s). Does not affect Corner Summary or Consistency metrics. |
| Select laps | Toggle individual laps on/off; Select all / Deselect all buttons |
| Reference lap | Lap used as the baseline for Speed Delta and Time Delta plots |
| Show hover data | Enable interactive tooltips on all plots |
| Corner map measure | Channel used to color the Corner Map (Speed, Lateral G, Longitudinal G, Throttle, Brake) |
| Reference circle radius (G) | Optional dashed circle on the Traction Circle, for visual reference only (blank = hidden, the default). It is not the car's grip limit, which depends on speed, load, tires and direction and cannot be derived from the telemetry. |
| Lateral G low-pass cutoff (Hz) | Optional filter for lateral acceleration (0 = no filter), applied to the whole lap before the corner is cut out. Affects the Traction Circle, Corner Map (Lateral G) and "Max Lateral G (filtered)"; "Max Lateral G (raw)" is always unfiltered. Longitudinal acceleration is not filtered. |

#### Analysis tabs

| Tab | What it shows |
|-----|--------------|
| **Traction Circle** | Lateral G vs longitudinal G scatter for all laps (a G-G diagram), with an optional illustrative reference circle. Axis range = 99.9th percentile of \|G\|, so single spikes (e.g., impacts) do not shrink the plot. |
| **Speed Trace** | Speed (km/h) over normalized lap position; green/red dashed lines mark entry/exit |
| **Throttle & Brake** | Gas and brake inputs faceted into two panels over normalized lap position |
| **Corner Summary** | `gt` table — corner time (s), entry speed, min speed, average speed, exit speed, max lateral G (raw and filtered), max braking and acceleration G per lap; a note states the filter used; sorted by fastest corner time; reference lap highlighted. **Export CSV** button saves the displayed table. |
| **Speed Delta** | Speed difference (km/h), lap minus reference lap, over normalized position; positive = higher speed |
| **Time Delta** | Cumulative time difference (s), lap minus reference lap, starting from 0 at the left edge of the plot; positive = slower (time lost), as in the Segment Time Delta Map |
| **Corner Map** | XZ coordinate scatter colored by the selected measure, one panel per lap |
| **Consistency** | CV% statistics table (mean, SD, CV%, min, max for each metric) plus a lap-by-lap trend chart with mean reference lines. **Export CSV** button saves the statistics table. |

### Required columns

| Column | Required? |
|--------|-----------|
| `lapCount`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ`, `lapTime` | Always |
| `speedKmh` | Speed Trace, Corner Summary, Speed Delta, Time Delta, Corner Map |
| `accGHorizontal`, `accGFrontal` | Traction Circle |
| `gas`, `brake` | Throttle & Brake plot |

> **Note:** The app works with whichever columns are present. Tabs or map measures that rely on a missing column will display an informative error message rather than crashing.

---

## Input/Output Summary

| Step | Input | Output |
|------|-------|--------|
| 1 Harmonize & Preprocess | Any raw telemetry CSV | `*_harmonized.csv`, `*_filtered.csv`, `*_minimal.csv` |
| 2 Composite reference lap | Filtered/minimal CSV | CSV with the composite reference lap appended |
| 3 Summary | Any harmonized CSV | On-screen tables and trend plots; CSV export for Lap Statistics and Summary Statistics |
| 4 Track Mapping | Any harmonized CSV | On-screen interactive track maps |
| 5 Lap Comparison | Any harmonized CSV | On-screen comparison maps and traces |
| + Corner Analysis | Any harmonized CSV | On-screen corner metrics, deltas, traction, and consistency plots; CSV export for Corner Summary and Consistency |

---

## Notes

- All scripts accept file uploads of up to **500 MB**.
- Track orientation (flip horizontal/vertical) must be set consistently across all steps for the coordinate systems to match.
- The composite reference lap is assigned the next available lap number automatically and marked in the `.synthetic` column, so the Lap Summary pre-selects it. Note its number in case you need to designate it manually.
- Most plots use **Times New Roman, size 18** and `theme_classic()`.

---

## Validation

All validation material is in the `validation/` folder. Run each check from the project folder (open `telemetry_to_insight.Rproj`); each script prints its checks and writes a results CSV to `validation/`. The checks use the same functions in `telemetry_core.R` as the apps.

| Script | What it checks | Data | Result |
|--------|----------------|------|--------|
| `validate_time_weighting.R` | Time-weighted metrics: equal to arithmetic means under uniform sampling, and to hand-calculated values under irregular sampling | `telemetry_validation.csv` (4 laps × 20 samples, used for manual validation), `telemetry_validation_uneven.csv` (2 laps × 8 samples) | 57/57 checks |
| `make_validation_stress.R` → `validate_stress.R` | Every lap-summary, corner and sector metric against exactly known true values, for nine recording problems (10/60/100 Hz, irregular sampling, 2-s dropout, 5% missing values, missing time stamps, missing channel, all combined at ~10 Hz) | `telemetry_validation_stress.csv` (generated) and `telemetry_validation_stress_expected.csv` (true values) | 162/162 checks |
| `validate_composite_lap.R` | Sector times from interpolated boundary crossings (10, 60, 100 Hz, irregular) and construction of the composite reference lap | generated in the script | 30/30 checks |
| `validate_interpolation_peaks.R` | How linear interpolation (lap comparison grid, composite lap) affects peak and minimum values | synthetic laps with known peaks; optional real session | interpolation never exceeds recorded extremes; attenuation reported |
| `validate_filter.R` | Low-pass filter with uniform, alternating, irregular and missing-value sampling | synthetic signals | 18/18 checks |
| `validate_position_check.R` | Track-position check: exact position, start offset at the timing line, distorted position, incomplete lap, missing coordinates | synthetic circular laps | all checks pass |
| `validate_real_lap_times.R` | Lap times against the simulator's official lap times, and distance per lap against the official track length | a real session file (not included; set `real_file`) | per-lap summary |
| `check_acceleration_axes.R` | Which acceleration column holds lateral acceleration (correlation with steering × speed², and a plot of steering and the acceleration columns for one lap) | a real session file (not included; set `real_file`) | printed correlations and plot |

```r
source("validation/validate_time_weighting.R")
source("validation/make_validation_stress.R")   # only needed to regenerate the stress dataset
source("validation/validate_stress.R")
source("validation/validate_composite_lap.R")
source("validation/validate_filter.R")
source("validation/validate_position_check.R")
source("validation/validate_interpolation_peaks.R")

# Real-data checks need a raw session file with an official lap-time column (not included):
real_file <- "C:/path/to/telemetry.csv"
official_track_length_m <- 1956                 # optional
source("validation/validate_real_lap_times.R")
source("validation/check_acceleration_axes.R")   # which column is lateral acceleration?
real_lateral_col <- "accGVertical"               # if the logger labels lateral acceleration differently
source("validation/validate_interpolation_peaks.R")
```
