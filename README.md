---
editor_options: 
  markdown: 
    wrap: 72
---

# YBCUDataManipulation

Merging/amending 10+ years of yellow-billed cuckoo observations

**Data Folder**:

-   pre2022data: data recorded before the start of 2022

-   post2022data: data recorded after the start of 2022

-   SurveySummary: information for an overview of the transect surveyed.

-   VisitSummary: information for an overview of each visit made to the
    transect.

-   DailyData: information from recorded the day of the survey on
    in-field survey sheets.

    Numeric codes at the end of data files are in DD/MM/YYYY format. For
    example, DailyData_14022025.csv is a CSV file that was downloaded
    February 14th, 2025 and contains data collect by surveyors in the
    field.

Code Folder:

-   FormatSurveySummary.R: for survey summaries entries, combine pre and
    post 2022 datasets, convert UTMs to lat/lon, format final data
    frame, and save output as a csv.

-   FormatSurveyVisits.R: for survey visit entries, combine pre and post
    2022 datasets, add site names to entries, convert UTMs to lat/lon,
    format final data frame, and save output as a csv.

-   FormatDailyData.R: for field point observations, combine pre and
    post 2022 datasets, convert UTMs to lat/lon, format final data
    frame, and save output as a csv.
