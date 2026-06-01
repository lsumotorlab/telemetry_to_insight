# Telemetry Analysis Suite — Usage Guide

A collection of interactive R/Shiny tools for racing telemetry analysis. The workflow proceeds in five sequential steps, each producing output that feeds the next.

---

## Prerequisites

Install the required packages before running any script:

```r
install.packages(c(
  "shiny", "tidyverse", "readr", "plotly",
  "ggplot2", "viridis", "gt", "scales",
  "lubridate", "purrr", "tidyr", "dplyr",
  "signal"
))
```

---

## Recommended Workflow

```
Raw CSV
   ↓
[1] telemetry_harmonizer_and_preprocessing.R        → harmonized & filtered CSV
   ↓
[2] create_synthetic_lap.R                          → CSV with synthetic lap appended
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
source("telemetry_harmonizer_and_preprocessing.R")
run_telemetry_harmonize_and_preprocess_app()
```

**Purpose:** Maps your raw CSV columns to the standard column names expected by all downstream scripts, then applies quality filters to select clean racing laps.

### Tabs

| Tab | What it does |
|-----|-------------|
| **1 Preview** | Shows raw file contents and detected column names |
| **2 Harmonize Mapping** | Maps raw columns to standard names (auto-suggested, manually adjustable). Click **Apply harmonization**. |
| **3 Preprocess** | Shows lap overview before and after filtering. Click **Apply preprocessing**. |

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
            Verify the axis convention for your specific data source before interpreting coordinate-based visualisations.

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
| `accGFrontal` | Longitudinal acceleration in G (positive = accelerating, negative = braking) |

### Harmonization options

- **Auto convert speed** — detects m/s and converts to km/h automatically
- **Input lap time unit** — choose Milliseconds, Seconds, or Auto-detect. Auto-detect assumes seconds when the median lap time is ≤ 1000 (since no real racing lap takes more than 1000 seconds) and converts by multiplying by 1000. Use the manual options if your data is ambiguous.
- **Input acceleration unit** — choose how `accGHorizontal` and `accGFrontal` are stored in your source file. Options: Auto-detect, G forces, m/s². Auto-detect assumes m/s² when the 99th percentile of absolute values exceeds 10 (since 10 G is physically impossible, but 10 m/s² ≈ 1 G is normal). m/s² values are divided by 9.81 to convert to G.

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

## Step 2 — Create Synthetic Lap

**File:** `create_synthetic_lap.R`

**Run:**
```r
source("create_synthetic_lap.R")
synthetic_lap_interactive()
```

**Purpose:** Constructs an optimal "synthetic" lap by dividing the track into sectors and using the best recorded sector time from any lap for each sector. The result is appended to your dataset as an additional lap.

### Workflow (three sequential windows)

**Window 1 — Upload & configure**

1. Upload the filtered/minimal CSV from Step 1.
2. Optionally flip the track horizontally or vertically so it displays correctly.
3. Select the **reference lap** — the lap whose track outline will be used to draw sector boundaries.
4. Set **Resample bins** (number of data points in the synthetic lap; defaults to the length of the first lap).
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
- Click **Download updated CSV** to save the dataset with the synthetic lap appended.
- The synthetic lap receives the next available lap number (max lap + 1).

> The downloaded CSV from this step is the recommended input for Steps 3–5 when you want to include the synthetic lap in your analysis.

---

## Step 3 — Lap Statistics Summary

**File:** `telemetry_lap_level_summary_interactive.R`

**Run:**
```r
source("telemetry_lap_level_summary_interactive.R")
run_telemetry_app()
```

**Purpose:** Displays per-lap performance statistics and summary statistics across all selected laps. Useful for identifying outliers, tracking consistency, and comparing effort-related metrics.

### Sidebar controls

| Control | Description |
|---------|-------------|
| Upload CSV | Load the telemetry file (harmonized or with synthetic lap) |
| Flip horizontal / vertical | Mirror track coordinates if needed |
| Full throttle threshold | % gas above which a sample counts as "full throttle" |
| Braking threshold | % brake above which a sample counts as "braking" |
| Choose laps | Checkboxes to include/exclude individual laps |
| Designate synthetic lap | Mark one lap as synthetic — it is excluded from summary statistics but shown in the lap table |

### Tabs

| Tab | Contents |
|-----|---------|
| **Lap Statistics** | Per-lap table: lap time, full throttle %, braking %, brake/throttle overlap %, avg/max/min speed, ABS %, TC %, max lateral G, max accel G, max brake G. **Export CSV** button saves the displayed table. |
| **Summary Statistics** | Mean, SD, Min, Max across all selected non-synthetic laps (statistics as rows, variables as columns). **Export CSV** button saves the displayed table. |
| **Lap Trend** | Line chart of any metric across laps — useful for spotting fatigue or warm-up effects |

---

## Step 4 — Track Mapping (Single Lap)

**File:** `track_mapping_interactive.R`

**Run:**
```r
source("track_mapping_interactive.R")
track_mapping_interactive()
```

**Purpose:** Visualises telemetry channels overlaid on the track map for a single lap. Up to 4 plots can be displayed simultaneously.

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
| Lateral G low-pass cutoff (Hz) | *(visible when Lateral G map or Lateral G over time is selected)* Butterworth 2nd-order zero-phase filter applied to `accGHorizontal`. Range 0–20 Hz; set to 0 for the raw signal. A cutoff of 2–5 Hz captures intentional steering inputs; values above 5 Hz likely reflect suspension or road noise. |
| Overlap thresholds | *(visible when Brake/Throttle Overlap is selected)* Separate throttle and brake thresholds defining what counts as simultaneous overlap |
| Show hover data | Enable interactive tooltips on the map |

### Available plot types

| Plot | What is shown |
|------|--------------|
| Track outline | Plain track layout |
| Speed map | Track coloured by speed (red–yellow–green gradient) |
| Gear map | Track coloured by gear engaged |
| Braking zones | Points where brake input exceeds threshold, coloured by brake pressure |
| Full throttle zones | Points at full throttle, coloured by speed |
| Brake/Throttle Overlap | Points where brake and throttle are simultaneously active, coloured by flag |
| ABS activation points | Locations where ABS was triggered |
| TC activation points | Locations where traction control was triggered |
| Elevation profile | Elevation vs. distance along the lap |
| Track with elevation color | Track map coloured by elevation (cividis palette) |
| Lateral G map | Track coloured by lateral acceleration (`accGHorizontal`). Blue = left cornering, red = right cornering. Supports low-pass filtering. |
| Longitudinal G map | Track coloured by longitudinal acceleration (`accGFrontal`). Green = accelerating, red = braking. |
| Lateral G over time | Lateral acceleration (`accGHorizontal`) plotted as a time series over lap time. Blue = left, red = right. Supports low-pass filtering. |
| Steer over time | Steering input (`steer`) plotted as a time series over lap time. Blue = left, red = right. |

---

## Step 5 — Lap Comparison

**File:** `track_mapping_compare_interactive.R`

**Run:**
```r
source("track_mapping_compare_interactive.R")
track_mapping_compare()
```

**Purpose:** Side-by-side comparison of two laps. Shows how a chosen telemetry attribute differs between the reference and comparison lap, both as a track map and as a trace over normalized lap position.

### Sidebar controls

| Control | Description |
|---------|-------------|
| Upload CSV | Load telemetry file |
| Select Lap A / Lap B | Choose the two laps to compare |
| Reference lap | Which of A or B is the reference (delta = comparison − reference) |
| Reference lap is synthetic | Labels the reference lap as synthetic in the plot titles |
| Flip horizontal / vertical | Mirror coordinates |
| Comparison attribute | The telemetry channel to compare (speed, RPM, gear, gas, brake, steer, ABS, TC, lateral G, longitudinal G). When **lateral G** or **steer** is selected, `accGHorizontal` and `steer` are compared as their **absolute value** — left/right sign is dropped so both laps reflect cornering force magnitude and steering magnitude only. |
| Gear encoding | *(visible when Gear is selected)* Remap raw gear values where 0=R, 1=N, 2–7 = gears 1–6 |
| Interpolation points | Resolution of the common lap axis used for delta calculation (default 800) |
| Show hover data | Enable interactive tooltips |

### Four fixed panels

| Panel | Description |
|-------|-------------|
| **Reference map** | Track map coloured by the selected attribute for the reference lap |
| **Comparison map** | Track map coloured by the selected attribute for the comparison lap |
| **Delta map** | Track map coloured by the difference (comparison − reference). Green/red = faster/slower for speed; blue/red for other attributes |
| **Attribute trace** | Line chart of both laps over normalized lap position (0–100%) |

Lap times for both selected laps are displayed above the plots in `M:SS.ss` format.

---

## Corner Analysis

**File:** `corner_analysis_interactive.R`

**Run:**
```r
source("corner_analysis_interactive.R")
corner_analysis()
```

**Purpose:** Deep-dive analysis of a single corner across multiple laps. You interactively define the corner boundaries on the track map and then explore traction, speed, throttle/brake, time deltas, and consistency metrics for every selected lap. Can be used at any point after Step 1 — no synthetic lap required.

### Workflow (three sequential windows)

**Window 1 — Upload & Setup**

1. Upload the harmonized/filtered CSV from Step 1 (or Step 2 if you want the synthetic lap included).
2. Optionally flip coordinates horizontally (X) or vertically (Z) to orient the track correctly.
3. Select a **reference lap** — its track outline is used for corner selection in Window 2.
4. Click **Continue to corner selection**.

**Window 2 — Define Corner**

- An interactive track map of the reference lap is displayed.
- **Click once** to place the corner **entry** point (snapped to the nearest `carPositionNormalized` value; shown in green).
- **Click again** to place the corner **exit** point (shown in red). The selected section is highlighted in orange.
- Use **Reset** to start over, then **Confirm corner** to proceed.

**Window 3 — Analysis**

The main analysis app. The sidebar lets you adjust the pre/post buffer, select laps, choose a reference lap for deltas, and configure the grip limit.

#### Sidebar controls

| Control | Description |
|---------|-------------|
| Seconds before entry | Extra data shown before the corner entry point (0–10 s) |
| Seconds after exit | Extra data shown after the corner exit point (0–10 s) |
| Select laps | Toggle individual laps on/off; Select all / Deselect all buttons |
| Reference lap | Lap used as the baseline for Speed Delta and Time Delta plots |
| Show hover data | Enable interactive tooltips on all plots |
| Corner map measure | Channel used to colour the Corner Map (Speed, Lateral G, Longitudinal G, Throttle, Brake) |
| Grip limit (G) | Radius of the dashed circle on the Traction Circle. Auto-suggested as 90% of the maximum recorded braking G across all laps |

#### Analysis tabs

| Tab | What it shows |
|-----|--------------|
| **Traction Circle** | Lateral G vs longitudinal G scatter for all laps, with a user-defined grip limit circle |
| **Speed Trace** | Speed (km/h) over normalized lap position; green/red dashed lines mark entry/exit |
| **Throttle & Brake** | Gas and brake inputs faceted into two panels over normalized lap position |
| **Corner Summary** | `gt` table — corner time (s), entry speed, min speed, average speed, exit speed per lap; sorted by fastest corner time; reference lap highlighted. **Export CSV** button saves the displayed table. |
| **Speed Delta** | Speed difference (km/h) vs the reference lap over normalized position; positive = faster |
| **Time Delta** | Cumulative time gap (s) vs the reference lap; positive = faster |
| **Corner Map** | XZ coordinate scatter coloured by the selected measure, one panel per lap |
| **Consistency** | CV% statistics table (mean, SD, CV%, min, max for each metric) plus a lap-by-lap trend chart with mean reference lines. **Export CSV** button saves the statistics table. |

### Required columns

| Column | Required? |
|--------|-----------|
| `lapCount`, `carPositionNormalized`, `carCoordinatesX`, `carCoordinatesZ`, `lapTime` | Always |
| `speedKmh` | Speed Trace, Corner Summary, Speed Delta, Time Delta, Corner Map |
| `accGHorizontal`, `accGFrontal` | Traction Circle, grip limit suggestion |
| `gas`, `brake` | Throttle & Brake plot |

> **Note:** The app works with whichever columns are present. Tabs or map measures that rely on a missing column will display an informative error message rather than crashing.

---

## Input/Output Summary

| Step | Input | Output |
|------|-------|--------|
| 1 Harmonize & Preprocess | Any raw telemetry CSV | `*_harmonized.csv`, `*_filtered.csv`, `*_minimal.csv` |
| 2 Synthetic Lap | Filtered/minimal CSV | CSV with synthetic lap row appended |
| 3 Summary | Any harmonized CSV | On-screen tables and trend plots; CSV export for Lap Statistics and Summary Statistics |
| 4 Track Mapping | Any harmonized CSV | On-screen interactive track maps |
| 5 Lap Comparison | Any harmonized CSV | On-screen comparison maps and traces |
| + Corner Analysis | Any harmonized CSV | On-screen corner metrics, deltas, traction, and consistency plots; CSV export for Corner Summary and Consistency |

---

## Notes

- All scripts accept file uploads of up to **500 MB**.
- Track orientation (flip horizontal/vertical) must be set consistently across all steps for the coordinate systems to match.
- The synthetic lap is assigned the next available lap number automatically. Note this number when moving the file into Steps 3–5 so you can designate it correctly.
- All plots use **Times New Roman, size 18** and `theme_classic()` for publication-ready output.
