# Permits Data Pipeline

## Overview
The Boston Permits Data Pipeline processes building permits data to create comprehensive datasets for spatial and temporal analysis. It consists of two components: the Records Pipeline and the Ecometrics Pipeline. The Records Pipeline focuses on cleaning, geocoding, and integrating raw permits data, while the Ecometrics Pipeline aggregates this data to generate metrics at various geographic levels, including land parcels, block groups, and census tracts. These outputs support the analysis of construction trends and patterns over time and space.

## Pipeline Components

### Records Pipeline
This component prepares raw permits data for analysis by performing data cleaning, geocoding, and integration with property and land parcel information. Duplicate records are removed, missing geographic information is geocoded, and the data is enriched with identifiers for spatial analysis. The processed permits data is saved as a CSV file named `Permits.Records.Geocoded.(MM-YYYY).csv`.

### Ecometrics Pipeline
Building on the processed permits data, the Ecometrics Pipeline calculates metrics at multiple geographic levels for annual and longitudinal analysis. Outputs include aggregated metrics for land parcels, block groups, and census tracts, provided in both tabular (CSV) and geospatial (shapefile/GPKG) formats. These metrics include permit counts by type, adjusted valuations, and normalized values per parcel.

## Usage Instructions
1. Place input files (e.g., permits, land parcels, property data) in the `Inputs` directory.
2. Run the Records Pipeline to clean and geocode permits data.
3. Execute the Ecometrics Pipeline to calculate metrics and generate outputs.
4. Processed datasets will be saved in the `Outputs` directory, organized by geographic level and file format.

## Author
This guide was prepared by Yunus Emre Tapan in November 2024.
