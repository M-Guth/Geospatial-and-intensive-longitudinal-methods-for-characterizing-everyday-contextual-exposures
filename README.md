# Geospatial and intensive longitudinal methods for characterizing everyday contextual exposures

Dieses Repository ist eine eigenstaendige Demo-App in Shiny. Die App imitiert den Analyseansatz aus dem APAC-Repository mit simulierten GPS-Daten in der Karlsruher Innenstadt.

## Inhalt

- Simulation von 5000 GPS-Punkten auf Minute-Level
- Mehrere userId-Werte und APAC-aehnliche Variablenstruktur
- Exemplarische Ableitung von:
  - Distanz- und Bewegungsmetriken
  - Home-Proximity (200m)
  - OSM-analogen Crowded-Area-Indikatoren
  - 100m Buffer-Statistiken fuer NDVI, Imperviousness und Population
- Day-Level Aggregation wie im APAC-Workflow
- Interaktive Kartenansichten und Tabellen

## Projektstruktur

- app.R: Shiny-App (UI + Server)
- R/simulate_data.R: Datensimulation, Methodik-Layer, Feature Engineering
- data/: optionaler Speicherort fuer exportierte CSVs

## Start

1. R oder RStudio im Projektordner oeffnen
2. Benoetigte Pakete installieren
3. App starten

```r
install.packages(c(
  "shiny", "dplyr", "ggplot2", "leaflet", "DT", "sf", "scales",
  "lubridate", "tidyr", "purrr"
))

shiny::runApp()
```

## Methodik in der App

Tab 1: Minute-Level Karte
- Zeigt simulierte GPS-Punkte je userId
- Farbgebung ueber waehlbare Variablen (z. B. speed_kmh, ndvi_mean_100m)
- Klick auf Punkt + optionaler 100m Buffer

Tab 2: Tagesaggregation
- Zeigt day-level Kennzahlen pro userId und Datum
- Zeitreihe, Tageskarte und Tabelle

Tab 3: Methodik und Eingangsdaten
- Zeigt simuliertes NDVI-Raster und OSM-analoge Layer
- Dokumentiert die Buffer-Logik (100m bei Umweltmetriken, 50m/10m/50m/0m bei OSM-Layern)

## Hinweise

- Die Daten sind komplett simuliert und dienen nur zur methodischen Demonstration.
- Die Variablennamen orientieren sich an der APAC-Pipeline, damit ein direkter Vergleich moeglich ist.
