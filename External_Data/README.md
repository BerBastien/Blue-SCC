# External Data Directory

This directory contains external datasets required for the Blue SCC analysis. The repository now uses local data files instead of external Google Drive paths.

## Directory Structure

```
External_Data/
├── 01_Data/               # Coral reef shapefiles (WCMC)
├── Pop/                   # SSP gridded population data (nested structure)
├── SPP1/                  # SSP1 gridded population TIF files
├── SPP2/                  # SSP2 gridded population TIF files
├── SPP3/                  # SSP3 gridded population TIF files
├── SPP4/                  # SSP4 gridded population TIF files
├── SPP5/                  # SSP5 gridded population TIF files
└── SSPs/                  # SSP socioeconomic CSV files (TO BE ADDED)
    ├── SspDb_country_data_2013-06-12.csv
    ├── ssp_gdp.csv
    ├── ssp_pop.csv
    └── CO2Pulse/          # SSP scenario temperature CSV files
        ├── SSP126_magicc_*.csv
        ├── SSP245_magicc_*.csv
        ├── SSP370_magicc_*.csv
        ├── SSP460_magicc_*.csv
        └── SSP585_magicc_*.csv
```

## Current Status

### ✓ Available Data

1. **Coral Reef Shapefiles** (`01_Data/`)
   - WCMC008_CoralReef2021_Pt_v4_1 (point data)
   - WCMC008_CoralReef2021_Py_v4_1 (polygon data)
   - Source: WCMC Coral Reefs 2021 v4.1

2. **Gridded Population Data** (`SPP1/` - `SPP5/`, `Pop/`)
   - TIF files for years 2020-2100 (5-year intervals)
   - Multiple SSP scenarios (SSP1-SSP5)

### ⚠ Missing Data (Required)

The following CSV files are **required** but not yet included (place in `External_Data/SSPs/`):

1. **Country-level SSP Database**
   - `SspDb_country_data_2013-06-12.csv` - SSP country-level projections
   - Used by: Ports, Fisheries modules

2. **SSP GDP Projections**
   - `ssp_gdp.csv` - GDP projections by country, scenario, and year
   - Used by: Mangroves, cross-cutting analysis

3. **SSP Population Projections**
   - `ssp_pop.csv` - Population projections by country, scenario, and year
   - Used by: Cross-cutting analysis

4. **Temperature Scenarios** (`SSPs/CO2Pulse/`)
   - `SSP245_magicc_202303021423.csv` (or similar timestamp)
   - `SSP370_magicc_202311031621.csv` (or similar timestamp)
   - `SSP585_magicc_202310021547.csv` (or similar timestamp)
   - `SSP126_magicc_202308040902.csv` (or similar timestamp)
   - `SSP460_magicc_202402051249.csv` (or similar timestamp)
   - Used by: Temperature damage functions, cross-cutting analysis

## Data Sources

- **Coral Reefs**: UN Environment Programme World Conservation Monitoring Centre (UNEP-WCMC)
- **Gridded Population**: SSP population gridded datasets
- **SSP Socioeconomic Data**: SSP Database (IIASA) - to be obtained
- **Temperature Scenarios**: MAGICC climate model outputs - to be obtained

## Notes

- The code will run with warnings if CSV files are missing
- Some modules may fail without the required CSV data
- File paths are now relative to the repository root using `here::here()`
- The original code referenced Google Drive paths which have been replaced with local paths
