# Predicting Shared Bike Usage in London

## Description

This is the repository for my London bike analysis, my final project for the capstone course of the HarvardX [*Data Science Professional Certificate*](https://www.edx.org/professional-certificate/harvardx-data-science) programme on [edX](https://www.edx.org/).

In this project, I use data science and machine learning techniques learned on the course to explore data and predict the demand for shared bikes in London. I welcome suggestions on how to improve the report or analysis.

## Files in the Repository

Included in the repository are the following:

1. The `.R` file used to produce figures and train models (`bike_analysis_report.R`);

2. The `.Rmd` file used for writing the report (`bike_analysis_report.Rmd`);

3. The data set used for the analysis, `london_merged.csv`, stored within the `london_bikes` subfolder;

4. The finalized `.pdf` report, `bike_analysis_report.pdf`.

## Steps to Correctly Run the Code and Generate the Report

1. Ensure that the data set is saved at the correct relative path specified in line 28 of the `.R` file.

2. Run the `.R` file. The `.R` file **must** be run first to generate various figures as well as the results of the modelling, in order for the report to be generated correctly.

2. Run the `.Rmd` file to generates the results in the report.

3. Knit the `.Rmd` file to PDF to generate the `.pdf` report.