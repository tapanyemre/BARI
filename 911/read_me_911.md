# 911 Data Pipeline

## Overview
The 911 Data Pipeline is designed to process 911 call data and generate longitudinal ecometrics files for analysis. This pipeline ensures the preparation and transformation of raw 911 call data into meaningful metrics for research and reporting purposes. Starting in 2024, the pipeline transitioned to using 2020 Census identifiers, and the merging of older census identifiers has been discontinued. However, starting in 2025, the merging of data from previous years will resume to maintain and update the longitudinal ecometrics datasets.

## Outputs
The pipeline produces outputs that facilitate the longitudinal analysis of 911 call data. These outputs include cleaned and processed datasets, as well as ecometrics files, which are structured to analyze trends over time.

1. **Processed 911 Call Data**  
   This dataset includes cleaned and formatted 911 call records with updated call types. The data is prepared to ensure consistency and accuracy across all records.

2. **Ecometrics Files**  
   The ecometrics files are derived from the processed 911 call data and are designed for longitudinal analysis. These files provide metrics to evaluate trends and patterns in 911 call data over time.

## Pipeline Scripts
The pipeline is executed in sequential order through the following scripts:

- **1. 911_A_DataProcessing**  
  This script prepares the updated call types file and processes the most recent 911 call data. It handles data cleaning, updates call types, and formats the data for the next stage in the pipeline.

- **2. 911_B_CreateEcometrics**  
  This script generates longitudinal ecometrics files based on the processed 911 call data. These files contain metrics for analyzing 911 call trends over time and across geographic areas.

## Author
This guide was prepared by Yunus Emre Tapan in November 2024.
