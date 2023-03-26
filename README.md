# Visualization Linear Modelling (QHELP) 

## Goal
Goal of this ShinyApp is to visualize linear models of differing complexity. This app may be used for teaching purposes or refreshing knowledge on linear modelling. Users may play around with the app at [QHELP](https://r.qhelp.eu/qhelp/2022-Padua/Group2/)

## Usage
Users can upload any CSV file to investigate a data set, given variables have the same number of observations. The app distinguishes between three types of regression, namely, **Simple Linear Regression**, **Multiple Linear Regression**, **Hierarchical Multiple Linear Regression**. Each type of regression offers three visualizations:

(1) Scatterplot incl. the regression line (for MLR & HMLR other variables are held constant)

(2) Horizontal Bar Chart visualizing the regression coefficients of the model in question (for HMLR of all models)

(3) Bar Chart visualizing the adjusted R-squared of the model in question (for HMLR of all models)

## Content
The repository contains the app and a data set ("Cars") to test the functionality. R packages required: **shiny**, **ggplot2** and **car**
