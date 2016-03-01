# Data Explorer

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DataExplorer)](http://cran.r-project.org/package=DataExplorer)

Master:
[![Build Status](https://travis-ci.org/boxuancui/DataExplorer.svg?branch=master)](https://travis-ci.org/boxuancui/DataExplorer)
[![codecov.io](https://codecov.io/github/boxuancui/DataExplorer/coverage.svg?branch=master)](https://codecov.io/github/boxuancui/DataExplorer?branch=master)

Develop:
[![Build Status](https://travis-ci.org/boxuancui/DataExplorer.svg?branch=develop)](https://travis-ci.org/boxuancui/DataExplorer)
[![codecov.io](https://codecov.io/github/boxuancui/DataExplorer/coverage.svg?branch=develop)](https://codecov.io/github/boxuancui/DataExplorer?branch=develo[)

-----

## Background
[Exploratory Data Analysis (EDA)](https://en.wikipedia.org/wiki/Exploratory_data_analysis) is the initial and an important phase of data analysis. Through this phase, analysts/modelers will have a first look of the data, and thus generate relevant hypothesis and decide next steps. However, the EDA process could be a hassle at times. This `R` package aims to automate most of data handling and visualization, so that users could focus on studying the data and extracting insights.

## Installation
The package can be installed from `github` using `devtools` package.

    if (!require(devtools)) install.packages("devtools")
    library(devtools)
    install_github("boxuancui/DataExplorer")

If you would like to get the latest development version, you may run the following code in `R`.

    if (!require(devtools)) install.packages("devtools")
    library(devtools)
    install_github("boxuancui/DataExplorer", ref="develop")

## Examples
The package is extremely easy to use. Almost everything could be done in one line of `R` code. Please refer to the package manuals for more information.

To get a report for the `iris` dataset:

    library(DataExplorer)
    GenerateReport(iris)

To get a report for the `diamonds` dataset in `ggplot2` package:

    library(DataExplorer)
    library(ggplot2)
    GenerateReport(diamonds)
    
    




