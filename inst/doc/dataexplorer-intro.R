## ----setup, include=FALSE------------------------------------------------
library(rmarkdown)
library(DataExplorer)
library(data.table)
library(nycflights13)
library(networkD3)

knitr::opts_chunk$set(fig.width = 6, fig.height = 6, fig.align = "center", warning = FALSE)

## ----plot-str-template, eval=FALSE---------------------------------------
#  library(DataExplorer)
#  data_list <- list(airlines, airports, flights, planes, weather)
#  PlotStr(data_list)

## ----plot-str-run, echo=FALSE--------------------------------------------
data_list <- list(airlines, airports, flights, planes, weather)
diagonalNetwork(
  PlotStr(data_list, print_network = FALSE),
  width = 700,
  height = 550,
  fontSize = 12,
  margin = list(
    "left" = 80,
    "right" = 150
  )
)

## ----merge-data----------------------------------------------------------
merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

## ----eda-base-r, eval=FALSE----------------------------------------------
#  dim(final_data)
#  summary(final_data)
#  object.size(final_data)

## ----eda-plot-missing----------------------------------------------------
PlotMissing(final_data)

## ----eda-bar-discrete, fig.width=10, fig.height=6------------------------
BarDiscrete(final_data)

## ----eda-histogram-continuous, fig.width=10, fig.height=6----------------
HistogramContinuous(final_data)

## ----eda-bar-discrete-2--------------------------------------------------
BarDiscrete(final_data$manufacturer)

## ----eda-histogram-continuous-2------------------------------------------
HistogramContinuous(final_data$seats)

## ----eda-correlation, fig.width=8, fig.height=8--------------------------
CorrelationDiscrete(final_data)
CorrelationContinuous(final_data, use = "na.or.complete")

## ----fe-set-na-to, collapse=TRUE-----------------------------------------
library(data.table)
final_dt <- data.table(final_data)
SetNaTo(final_dt, list(0L, "unknown"))


## ----fe-collapse-category-count-trial------------------------------------
CollapseCategory(data = final_dt, feature = "manufacturer", threshold = 0.2)

## ----fe-collapse-category-count-update, results='hide'-------------------
CollapseCategory(data = final_dt, feature = "manufacturer", threshold = 0.2, update = TRUE)
BarDiscrete(final_dt$manufacturer)

## ----fe-collapse-category-metric-trial-----------------------------------
CollapseCategory(data = final_dt, feature = "name_carrier", threshold = 0.2, measure = "distance")

## ----fe-collapse-category-metric-update, results='hide'------------------
CollapseCategory(data = final_dt, feature = "name_carrier", threshold = 0.2, measure = "distance", update = TRUE)
BarDiscrete(final_dt$name_carrier)

## ----fe-drop-var, eval=FALSE, collapse=TRUE------------------------------
#  DropVar(final_dt, c("dst_origin", "dst_dest", "tzone_dest"))
#  DropVar(final_dt, c(34, 41, 42))

## ----dr-generate-report, eval=FALSE--------------------------------------
#  GenerateReport(final_data)

