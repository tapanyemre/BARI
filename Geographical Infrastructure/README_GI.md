# Geographical Infrastructure Data Pipeline

## Overview
The Geographical Infrastructure for Boston (GI) is a comprehensive database that organizes and links data for Boston, Massachusetts, across 17 different geographic levels. These levels include land parcels, streets, census geographies, and other administrative regions, which are structured hierarchically, with smaller units (e.g., land parcels) nested within larger ones (e.g., census tracts). Each level is coordinated using unique identifiers, facilitating the aggregation of data across levels and enabling the integration and analysis of datasets from various sources. The GI database is designed to link data produced by the City of Boston with census geographies, making it a powerful tool for geographic and demographic analysis.

## Outputs
The pipeline produces outputs in both `.csv` and `.shp` (GIS shapefile) formats. Below is a description of the main outputs:

1. **Properties**  
   The Properties dataset is derived from the City of Boston Property Assessment Database for the year 2023. This dataset includes detailed information about each property, such as its address, ownership, and land use. BARI has modified and cleaned the original data to ensure its accuracy and usability, and some new measures have been derived from the original dataset. While the database includes detailed information about properties, no shapefile is provided for this dataset because properties are mapped to Land Parcels. Multiple properties may exist on the same land parcel, and in some cases, they may overlap, making it difficult to define separate polygons.

2. **Land Parcels**  
   The Land Parcels dataset contains unique land parcels, which have been created by merging properties that share the same `GIS_ID` or street address. This process ensures that each land parcel is unique, even though the City of Boston considers every `GIS_ID` as its own parcel. By combining parcels with the same address, the number of land parcels has been reduced compared to the City’s original dataset. The output includes both a CSV file and a shapefile that corresponds to the City's Property Assessment Database.

3. **Street Segments**  
   The Street Segments dataset provides a complete list of all street segments in Boston, Massachusetts, as defined by the Census TIGER Line data from 2013. This dataset allows for the integration of street-level data with other geographic layers.

## Pipeline Scripts
The pipeline consists of multiple scripts that process and transform the data into the final outputs:

- **A. PropertiesToParcel**  
  This script processes parcels from the current year's property assessment dataset and creates an output that is used in the merge step.

- **B. PropToTiger**  
  This script assigns street information from TIGER Line data to parcels. During this step, the property assessment data and TIGER Line data are updated using the latest road information. The output is prepared for the merge step.

- **C. MergeAndOutput**  
  This script merges the outputs from Scripts A and B with property and parcel records to generate the final BARI Parcels dataset and properties with geographic information.

- **D1. ParcelToStreets**  
  This script maps parcel data to street-level information, enabling the analysis of parcel-related attributes at the street level.

- **D2. StreetKMeans**  
  This script applies K-means clustering to land use data from parcels and uses the output of Script D1 to analyze patterns at the street level.

- **E. SanityChecks**  
  This script performs validation checks on the final outputs to ensure they are consistent and comparable with previous years.

## Author
This guide is prepared by Yunus Emre Tapan in November 2024.
