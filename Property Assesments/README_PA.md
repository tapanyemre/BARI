# Property Assesments Data Pipeline

## Overview

The Property Assesments Data Pipeline processes raw data to produce cleaned datasets, aggregated metrics, and longitudinal files for spatial and temporal analysis. This pipeline integrates 2020 Census geographies while maintaining compatibility with older data to support comprehensive research on property and neighborhood-level dynamics. It is designed to handle raw data inconsistencies, impute missing values, and track property trends over time.


## Outputs

The outputs of the pipeline include cleaned and standardized property data, aggregated metrics for Census tracts and block groups, and longitudinal files that capture changes in property values over time. These outputs enable detailed spatial and temporal analysis of property dynamics and trends.

## Pipeline Scripts

### **1. PAD_A_UnitImputation**
This script processes raw property data to impute missing values and handle outliers. It prepares a cleaned and standardized dataset that serves as the foundation for further analysis.



### **2. PAD_B_Cross**
This script aggregates property metrics at Census tract and block group levels. It creates summarized datasets that support spatial analysis and visualization of property-level patterns.


### **3. PAD_C_NghbEffects**
This script conducts multilevel modeling to analyze property and neighborhood-level effects. It helps to identify key relationships and trends within neighborhoods.


### **4. PAD_D_Long**
This script generates longitudinal files that track changes in property values over time.

## Execution Sequence

1. **Run Script A**: Prepare imputed and cleaned property assessment data (`PAD.Record.wUnit.csv`).
2. **Run Script B**: Generate aggregated metrics and spatial files for geographic analysis.
3. **Run Script C**: Perform multilevel modeling for neighborhood effects.
4. **Run Script D**: Prepare ecometrics files for changes and trends over years.


## Author

This guide was prepared by Yunus Emre Tapan in November 2024.
