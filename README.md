# TimeSeries_CO2-GMSL_Analysis_Thesis
# Relationship between CO₂ and Sea Level: An Analysis through Time Series Models

## 📖 About the Project
This repository contains the research work, datasets, and analysis scripts for the bachelor's thesis in Statistical and Economic Sciences developed during the Academic Year 2024/2025. 
The main objective of the study is to explore and quantify the link between atmospheric CO₂ emissions and Global Mean Sea Level (GMSL) rise, adopting a quantitative perspective based on Environmental Statistics and time series analysis.

## 📊 Data Used
The analyzed time series were acquired from the NASA portal "Climate Change: Vital Signs of the Planet". The data includes:
* **CO₂ Concentration**: Monthly readings (expressed in ppm) recorded at the Mauna Loa Observatory in Hawaii starting from March 1958.
* **Global Mean Sea Level (GMSL)**: Monthly data (expressed in mm) derived from satellite altimetry observations since 1993, corrected for Glacial Isostatic Adjustment (GIA).

## 🔬 Methodology and Models
The analysis was conducted by applying advanced time series techniques, divided into two main approaches:
* **Univariate Analysis**: Use of the Box-Jenkins procedure and **SARIMA** models to identify trends and seasonality, generating reliable projections of future values up to 2050.
* **Multivariate Analysis**: Evaluation of the dynamic relationships and causal links between the two variables through:
    * Granger causality tests.
    * **VAR** (Vector AutoRegressive) models.
    * Cointegration analysis (Engle-Granger test) and **ECM** (Error Correction Model).
    * **ARDL** (Autoregressive Distributed Lag) models to simultaneously manage short- and long-term interactions.

## 💻 Technologies and Libraries
The entire project was developed in the **RStudio** environment. The main libraries used are:
* `tseries` for classical time series manipulations and additive decompositions.
* `dplyr` for data manipulation, preprocessing, and dataset merging.
* `ggplot2` for Data Visualization and Exploratory Data Analysis (EDA).
* `vars`, `lmtest`, and `dynlm` for estimating multivariate models and performing statistical tests.
* Custom functions (such as `arimaest`, `forecastplott`, `testnorm`, and `correlresidui`) were used to streamline estimations, residual diagnostics, and visualizations.

## 📈 Main Results
* The forecasts based on the SARIMA model estimate an increase of approximately 63 ppm of atmospheric CO₂ and a sea level rise of about 10 cm by 2050.
* The ARDL model stood out as the most effective and realistic approach for interpreting joint dynamics. It was able to accurately represent the strong dynamic persistence and historical memory of the series, offering a clear view of both the short- and long-term influence of CO₂ levels.

## 🎓 Author
* **Author**: Lorenzo Triolo (Student ID: 895541)
* **Degree Program**: Statistical and Economic Sciences
* **University**: University of Milano-Bicocca (School of Economics and Statistics)
* **Advisor**: Andrea Marletta
