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
#  plot_str(data_list)

## ----plot-str-run, echo=FALSE--------------------------------------------
data_list <- list(airlines, airports, flights, planes, weather)
diagonalNetwork(
  plot_str(data_list, print_network = FALSE),
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
plot_missing(final_data)

## ----eda-plot-bar, fig.width=10, fig.height=6----------------------------
plot_bar(final_data)

## ----eda-plot-histogram, fig.width=10, fig.height=6----------------------
plot_histogram(final_data)

## ----eda-plot-bar-single-------------------------------------------------
plot_bar(final_data$manufacturer)

## ----eda-plot-histogram-single-------------------------------------------
plot_histogram(final_data$seats)

## ----eda-plot-correlation, fig.width=8, fig.height=8---------------------
plot_correlation(final_data, use = "pairwise.complete.obs")

## ----eda-plot-boxplot, fig.width=8, fig.height=8-------------------------
## Reduce data size
arr_delay_df <- final_data[, c("arr_delay", "month", "day", "hour", "minute", "dep_delay", "distance", "year_planes", "seats", "speed")]
plot_boxplot(arr_delay_df, "arr_delay")

## ----eda-plot-scatterplot, fig.width=8, fig.height=8---------------------
## Reduce data size
arr_delay_df2 <- final_data[, c("arr_delay", "month", "distance", "seats", "origin", "carrier", "manufacturer")]
plot_scatterplot(arr_delay_df2, "arr_delay", size = 0.8)

## ----fe-set-missing, collapse=TRUE---------------------------------------
library(data.table)
final_dt <- data.table(final_data)
set_missing(final_dt, list(0L, "unknown"))

## ----fe-group-category-count-trial---------------------------------------
group_category(data = final_dt, feature = "manufacturer", threshold = 0.2)

## ----fe-group-category-count-update, results='hide'----------------------
group_category(data = final_dt, feature = "manufacturer", threshold = 0.2, update = TRUE)
plot_bar(final_dt$manufacturer)

## ----fe-group-category-metric-trial--------------------------------------
group_category(data = final_dt, feature = "name_carrier", threshold = 0.2, measure = "distance")

## ----fe-group-category-metric-update, results='hide'---------------------
group_category(data = final_dt, feature = "name_carrier", threshold = 0.2, measure = "distance", update = TRUE)
plot_bar(final_dt$name_carrier)

## ----fe-drop-columns, eval=FALSE, collapse=TRUE--------------------------
#  drop_columns(final_dt, c("dst_origin", "dst_dest", "tzone_dest"))
#  drop_columns(final_dt, c(34, 41, 42))

## ----dr-create-report, eval=FALSE----------------------------------------
#  create_report(final_data)

