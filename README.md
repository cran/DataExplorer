# DataExplorer
[![CRAN Version](http://www.r-pkg.org/badges/version/DataExplorer)](https://cran.r-project.org/package=DataExplorer)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/DataExplorer)](https://cran.r-project.org/package=DataExplorer)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/DataExplorer)](https://cran.r-project.org/package=DataExplorer)

[![Master Version](https://img.shields.io/badge/master-0.4.0-orange.svg)](https://github.com/boxuancui/DataExplorer/tree/master)
[![Travis Build Status](https://travis-ci.org/boxuancui/DataExplorer.svg?branch=master)](https://travis-ci.org/boxuancui/DataExplorer/branches)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/boxuancui/DataExplorer?branch=master&svg=true)](https://ci.appveyor.com/project/boxuancui/DataExplorer)
[![Coverage Status](https://img.shields.io/codecov/c/github/boxuancui/DataExplorer/master.svg)](https://codecov.io/gh/boxuancui/DataExplorer/branch/master)

[![Develop Version](https://img.shields.io/badge/develop-0.4.0.9000-orange.svg)](https://github.com/boxuancui/DataExplorer/tree/develop)
[![Travis Build Status](https://travis-ci.org/boxuancui/DataExplorer.svg?branch=develop)](https://travis-ci.org/boxuancui/DataExplorer/branches)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/boxuancui/DataExplorer?branch=develop&svg=true)](https://ci.appveyor.com/project/boxuancui/DataExplorer)
[![Coverage Status](https://img.shields.io/codecov/c/github/boxuancui/DataExplorer/develop.svg)](https://codecov.io/gh/boxuancui/DataExplorer/branch/develop)

---

## Background
[Exploratory Data Analysis (EDA)](https://en.wikipedia.org/wiki/Exploratory_data_analysis) is the initial and an important phase of data analysis. Through this phase, analysts/modelers will have a first look of the data, and thus generate relevant hypothesis and decide next steps. However, the EDA process could be a hassle at times. This [R](https://cran.r-project.org/) package aims to automate most of data handling and visualization, so that users could focus on studying the data and extracting insights.

## Installation
The package can be installed directly from CRAN.

    install.packages("DataExplorer")

However, the latest stable version (if any) could be found on [GitHub](https://github.com/boxuancui/DataExplorer), and installed using `devtools` package.

    if (!require(devtools)) install.packages("devtools")
    devtools::install_github("boxuancui/DataExplorer")

If you would like to install the latest [development version](https://github.com/boxuancui/DataExplorer/tree/develop), you may install the dev branch.

    if (!require(devtools)) install.packages("devtools")
    devtools::install_github("boxuancui/DataExplorer", ref = "develop")

## Examples
The package is extremely easy to use. Almost everything could be done in one line of code. Please refer to the package manuals for more information.

#### Create data profiling report
To get a report for the [airquality](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html) dataset:

    library(DataExplorer)
    GenerateReport(airquality)

To get a report for the [diamonds](http://docs.ggplot2.org/0.9.3.1/diamonds.html) dataset from `ggplot2` package:

    library(DataExplorer)
    library(ggplot2)
    GenerateReport(diamonds)

#### Visualize various distribution
You may also run all the plotting functions individually for your analysis, e.g.,

    library(DataExplorer)
    library(ggplot2)
    
    ## View distribution of all discrete variables
    BarDiscrete(diamonds)
    ## View distribution of cut only
    BarDiscrete(diamonds$cut)
    ## View correlation of all discrete varaibles
    CorrelationDiscrete(diamonds)
    
    ## View distribution of all continuous variables
    HistogramContinuous(diamonds)
    ## View distribution of carat only
    HistogramContinuous(diamonds$carat)
    ## View correlation of all continuous varaibles
    CorrelationContinuous(diamonds)
    
    ## View distribution of missing values for airquality data
    missing_data <- PlotMissing(airquality) # missing data profile will be returned
    missing_data

#### Collapse categorical variables
Sometimes, discrete variables are messy, e.g., too many imbalanced categories, extremely skewed categorical distribution. You may use `CollapseCategory` function to help you group the long tails.

    library(DataExplorer)
    library(ggplot2)
    data(diamonds)
    
    ## View original distribution of variable clarity
    diamonds <- data.table(diamonds)
    table(diamonds$clarity)
    
    ## Trial and error without updating: Group bottom 20% clarity based on frequency
    CollapseCategory(diamonds, "clarity", 0.2)
    ## Group bottom 30% clarity and update original dataset
    CollapseCategory(diamonds, "clarity", 0.3, update = TRUE)
    
    ## View distribution after updating
    table(diamonds$clarity)
    
    ## Group bottom 20% cut using value of carat
    table(diamonds$cut)
    CollapseCategory(diamonds, "cut", 0.2, measure = "carat", update = TRUE)
    table(diamonds$cut)

Note: this function works with [data.table](https://cran.r-project.org/package=data.table) objects only. If you are working with `data.frame`, please add `data.table` class to your object and then remove it later. See example below.

    library(DataExplorer)
    
    ## Set data.frame object to data.table
    USArrests <- data.table(USArrests)
    ## Collapse bottom 10% UrbanPop based on frequency
    CollapseCategory(USArrests, "UrbanPop", 0.1, update = TRUE)
    ## Set object back to data.frame
    class(USArrests) <- "data.frame"

#### Other miscellaneous functions
* `PlotStr`: Plot data structure in network graph.
* `DropVar`: Quickly drop variables with either column index or column names. (**data.table only**)
* `SetNaTo`: Quickly set all missing observations to a value. (**data.table only**)
* `SplitColType`: Split data into two objects: discrete and continous.
