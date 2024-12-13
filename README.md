# BARI Data Portal Workflow

## Introduction

BARI projects are deeply interconnected, relying on three main types of datasets:

1. **Static Datasets**: Updated infrequently, these datasets provide foundational context for long-term analyses.
2. **New Releases**: Generated through interdependent workflows, these datasets are published as standalone releases each year.
3. **Continuous Releases**: Updated annually, these datasets are merged into a unified dataset for consistency rather than creating separate releases.

### Static Datasets

- **Mass Census Indicators (MCI)** and **Census Geographies (CG)**: Updated every five to ten years, these datasets offer a long-term reference for analyses. They are publicly accessible through the **Harvard Dataverse**.

### New Releases

- **Geographical Infrastructure (GI)** and **Property Assessment (PA)**: These datasets are created through a closely linked workflow:
  - **GI** provides foundational data such as **Land Parcels**, **Properties**, and **Roads**, which support all other projects.
  - **PA** relies on GI outputs, and its results, in turn, feed back into GI processes, ensuring accuracy and consistency.

  The execution of GI and PA follows a structured sequence:
  1. Both scripts must be run simultaneously.
  2. Outputs from [the first PA script](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_A_UnitImputation.R) serve as inputs for [the second GI script](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_B_PropToTiger.R).
  3. [Final GI outputs](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_E_SanityChecks.Rmd) are required as inputs for [the second PA script](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_B_Cross.R).

### Continuous Releases

- **Permits** and **911**: These datasets evolve through annual updates. New data is merged into the existing dataset to maintain a unified and consistent record, differing from the standalone release approach of GI and PA.

### Documentation and Overview

Each script is thoroughly annotated with detailed explanations of its dependencies and functionality. The diagram below provides a high-level overview of how these datasets and scripts interact, enabling a seamless and cohesive workflow. Refer to the diagram below for a high-level overview of how these scripts communicate and work harmoniously to deliver results.

![Big Picture](https://github.com/tapanyemre/BARI/blob/main/bigpicture.png)

## Geographical Infrastructure Data Pipeline

### Overview
The Geographical Infrastructure for Boston (GI) is a comprehensive database that organizes and links data for Boston, Massachusetts, across 17 different geographic levels. These levels include land parcels, streets, census geographies, and other administrative regions, which are structured hierarchically, with smaller units (e.g., land parcels) nested within larger ones (e.g., census tracts). Each level is coordinated using unique identifiers, facilitating the aggregation of data across levels and enabling the integration and analysis of datasets from various sources. The GI database is designed to link data produced by the City of Boston with census geographies, making it a powerful tool for geographic and demographic analysis.

### Outputs
The pipeline produces outputs in both `.csv` and `.shp` (GIS shapefile) formats. Below is a description of the main outputs:

1. **Properties**  
   The Properties dataset is derived from the City of Boston Property Assessment Database for the year 2023. This dataset includes detailed information about each property, such as its address, ownership, and land use. BARI has modified and cleaned the original data to ensure its accuracy and usability, and some new measures have been derived from the original dataset. While the database includes detailed information about properties, no shapefile is provided for this dataset because properties are mapped to Land Parcels. Multiple properties may exist on the same land parcel, and in some cases, they may overlap, making it difficult to define separate polygons.

2. **Land Parcels**  
   The Land Parcels dataset contains unique land parcels, which have been created by merging properties that share the same `GIS_ID` or street address. This process ensures that each land parcel is unique, even though the City of Boston considers every `GIS_ID` as its own parcel. By combining parcels with the same address, the number of land parcels has been reduced compared to the City’s original dataset. The output includes both a CSV file and a shapefile that corresponds to the City's Property Assessment Database.

3. **Street Segments**  
   The Street Segments dataset provides a complete list of all street segments in Boston, Massachusetts, as defined by the Census TIGER Line data from 2013. This dataset allows for the integration of street-level data with other geographic layers.

### Workflow
The pipeline consists of multiple scripts that process and transform the data into the final outputs:

1. **[A.PropertiesToParcel](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_A_PropertiesToParcel.Rmd)**  
  This script processes parcels from the current year's property assessment dataset and creates an output that is used in the merge step.

2. **[B.PropToTiger](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_B_PropToTiger.R)**  
  This script assigns street information from TIGER Line data to parcels. During this step, the property assessment data and TIGER Line data are updated using the latest road information. The output is prepared for the merge step.

3. **[C.MergeAndOutput](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_C_MergeAndOutput.R)**  
  This script merges the outputs from Scripts A and B with property and parcel records to generate the final BARI Parcels dataset and properties with geographic information.

4. **[D1.ParcelToStreets](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_D1_ParcelToStreets.R)**  
  This script maps parcel data to street-level information, enabling the analysis of parcel-related attributes at the street level.

5. **[D2.StreetKMeans](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_D2_StreetKMeans.R)**  
  This script applies K-means clustering to land use data from parcels and uses the output of Script D1 to analyze patterns at the street level.

6. **[E.SanityChecks](https://github.com/tapanyemre/BARI/blob/main/Geographical%20Infrastructure/Codes_Final/GI_E_SanityChecks.Rmd)**  
  This script performs validation checks on the final outputs to ensure they are consistent and comparable with previous years.

## Property Assesments Data Pipeline

### Overview

The Property Assesments Data Pipeline processes raw data to produce cleaned datasets, aggregated metrics, and longitudinal files for spatial and temporal analysis. This pipeline integrates 2020 Census geographies while maintaining compatibility with older data to support comprehensive research on property and neighborhood-level dynamics. It is designed to handle raw data inconsistencies, impute missing values, and track property trends over time.

### Outputs

The outputs of the pipeline include cleaned and standardized property data, aggregated metrics for Census tracts and block groups, and longitudinal files that capture changes in property values over time. These outputs enable detailed spatial and temporal analysis of property dynamics and trends.

### Workflow

1. **[PAD_A_UnitImputation](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_A_UnitImputation.R)**  
   Cleans raw property data, imputes missing values, and handles outliers, generating the foundational dataset (`PAD.Record.wUnit.csv`).

2. **[PAD_B_Cross](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_B_Cross.R)**  
   Aggregates property metrics at Census tract and block group levels for spatial analysis and geographic files.

3. **[PAD_C_NghbEffects](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_C_NghbEffects.R)**  
   Conducts multilevel modeling to analyze neighborhood-level effects and identify key trends.

4. **[PAD_D_Long](https://github.com/tapanyemre/BARI/blob/main/Property%20Assesments/Codes_Final/PAD_D_Long.R)**  
   Generates longitudinal files to track changes in property values and produce ecometrics for temporal trends.

## Permits Data Pipeline

### Overview
The Boston Permits Data Pipeline processes building permits data to create comprehensive datasets for spatial and temporal analysis. It consists of two components: the Records Pipeline and the Ecometrics Pipeline. The Records Pipeline focuses on cleaning, geocoding, and integrating raw permits data, while the Ecometrics Pipeline aggregates this data to generate metrics at various geographic levels.

### Outputs
The pipeline produces outputs that facilitate the longitudinal analysis of Permits Records. These outputs include cleaned and processed datasets, as well as ecometrics files, which are structured to analyze trends over time.

1. **Processed Permits Record**  
   This dataset includes cleaned and formatted Permits records with updated call types. The data is prepared to ensure consistency and accuracy across all records.

2. **Permit Ecometrics Files**  
   The ecometrics files are derived from the processed Permits Record data and are designed for longitudinal analysis. These files provide metrics to evaluate trends and patterns in Permits data over time.

### Workflow

1. **[Permits.Syntax.Records.R](https://github.com/tapanyemre/BARI/blob/main/Permits/Codes_Final/Permits.Syntax.Records.R)**

    This component prepares raw permits data for analysis by performing data cleaning, geocoding, and integration with property and land parcel information. Duplicate records are removed, missing geographic              information is geocoded, and the data is enriched with identifiers for spatial analysis. The processed permits data is saved as a CSV file named `Permits.Records.Geocoded.(MM-YYYY).csv`.

2. **[Permits.Syntax.Ecometrics.R](https://github.com/tapanyemre/BARI/blob/main/Permits/Codes_Final/Permits.Syntax.Ecometrics.R)**
   
    Building on the processed permits data, the Ecometrics Pipeline calculates metrics at multiple geographic levels for annual and longitudinal analysis. Outputs include aggregated metrics for land parcels, block       groups, and census tracts, provided in both tabular (CSV) and geospatial (shapefile/GPKG) formats. These metrics include permit counts by type, adjusted valuations, and normalized values per parcel.

### Suggestions
1. Place input files (e.g., permits, land parcels, property data) in the `Inputs` directory.
2. Run the Records Script to clean and geocode permits data.
3. Execute the Ecometrics Script to calculate metrics and generate outputs.
4. Processed datasets will be saved in the `Outputs` directory, organized by geographic level and file format.

## 911 Data Pipeline

### Overview
The 911 Data Pipeline is designed to process 911 call data and generate longitudinal ecometrics files for analysis. This pipeline ensures the preparation and transformation of raw 911 call data into meaningful metrics for research and reporting purposes. Starting in 2024, the pipeline transitioned to using 2020 Census identifiers, and the merging of older census identifiers has been discontinued. However, starting in 2025, the merging of data from previous years will resume to maintain and update the longitudinal ecometrics datasets.

### Outputs
The pipeline produces outputs that facilitate the longitudinal analysis of 911 call data. These outputs include cleaned and processed datasets, as well as ecometrics files, which are structured to analyze trends over time.

1. **Processed 911 Call Data**
   
   This dataset includes cleaned and formatted 911 call records with updated call types. The data is prepared to ensure consistency and accuracy across all records.

2. **Ecometrics Files**
   
   The ecometrics files are derived from the processed 911 call data and are designed for longitudinal analysis. These files provide metrics to evaluate trends and patterns in 911 call data over time.

### Workflow
The pipeline is executed in sequential order through the following scripts:

1. **[911_A_DataProcessing](https://github.com/tapanyemre/BARI/blob/main/911/Codes_Final/911_A_DataProcessing.R)**  
  This script prepares the updated call types file and processes the most recent 911 call data. It handles data cleaning, updates call types, and formats the data for the next stage in the pipeline.

2. **[911_B_CreateEcometrics](https://github.com/tapanyemre/BARI/blob/main/911/Codes_Final/911_B_Create%20Ecometrics.R)**  
  This script generates longitudinal ecometrics files based on the processed 911 call data. These files contain metrics for analyzing 911 call trends over time and across geographic areas.

## Author
This guide is prepared by Yunus Emre Tapan in December 2024.
