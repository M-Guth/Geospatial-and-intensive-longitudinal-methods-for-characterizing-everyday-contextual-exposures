# Geospatial and Intensive Longitudinal Methods for Characterizing Everyday Contextual Exposures

This repository contains a standalone Shiny demo app that visualizes a simulated geospatial ambulatory assessment workflow in Karlsruhe city center.

The app follows an APAC-like structure (variable naming and feature logic) while keeping the full demo self-contained and reproducible with simulated data.

## Features

- Simulates 5,000 minute-level GPS observations across multiple user IDs
- Derives APAC-like contextual and behavioral features, including:
- Distance and mobility metrics
- Home proximity
- Crowded-area indicators from OSM-analog layers
- 100m buffer summaries (example raster, imperviousness, population)
- Aggregates minute-level records into day-level indicators
- Provides three interactive analysis tabs:
- Minute-Level Map
- Day-Level Aggregation (including calendar heatmap)
- Methodology and Input Data
- Includes a methodology map with:
- GPS points and sampled 100m buffers
- OSM-analog source layers (shop, pedestrian, railway, department)
- Buffer layers by geometry type (50m/10m/50m/0m logic)
- Intersection layers (GPS x example raster, GPS x each OSM layer)
- Collapsible left control/story panels that expand map space when hidden

## Project Structure

- `app.R`: Shiny UI/server app
- `R/simulate_data.R`: Data simulation, methodology layers, and feature engineering
- `data/`: data assets (includes `Karlsruhe_NDVI.tif` in this repository)

## Run the App

1. Open the project in R or RStudio
2. Install required packages
3. Launch the app

```r
install.packages(c(
  "shiny", "dplyr", "ggplot2", "leaflet", "DT", "sf", "scales",
  "lubridate", "tidyr", "purrr"
))

shiny::runApp()
```

## App Overview

### 1) Minute-Level Map

- Filter by user ID
- Color points by selectable variable (for example `speed_kmh`, `ndvi_mean_100m`)
- Inspect point details and optionally display a 100m clicked-point buffer
- Explore the corresponding minute-level table

### 2) Day-Level Aggregation

- Select user and day-level metric
- View trajectory line plot
- View calendar heatmap
- Explore day-level map and table

### 3) Methodology and Input Data

- Review the methodological story and assumptions
- Inspect source layers and derived intersections
- Toggle map layers (collapsed layer control)
- Use the tab-specific user filter to update methodology outputs

## Notes

- All records used by the app are simulated and intended for methodological demonstration.
- Naming conventions are kept close to APAC-style pipelines to support easier conceptual transfer.
