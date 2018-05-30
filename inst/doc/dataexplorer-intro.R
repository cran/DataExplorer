## ----setup, include=FALSE------------------------------------------------
library(rmarkdown)
library(knitr)
library(DataExplorer)
library(data.table)
library(ggplot2)
library(nycflights13)
library(networkD3)

opts_chunk$set(fig.width = 6, fig.height = 6, fig.align = "center", warning = FALSE)

## ----install-data, eval=FALSE--------------------------------------------
#  install.packages("nycflights13")
#  library(nycflights13)

## ----plot-str-template, eval=FALSE---------------------------------------
#  library(DataExplorer)
#  data_list <- list(airlines, airports, flights, planes, weather)
#  plot_str(data_list)

## ----plot-str-run, echo=FALSE--------------------------------------------
data_list <- list(airlines, airports, flights, planes, weather)
diagonalNetwork(
  plot_str(data_list, print_network = FALSE),
  width = 800,
  height = 800,
  fontSize = 20,
  margin = list(
    "left" = 50,
    "right" = 50
  )
)

## ----merge-data----------------------------------------------------------
merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

## ----eda-introduce-template, eval=FALSE----------------------------------
#  introduce(final_data)

## ----eda-introduce-run, echo=FALSE---------------------------------------
kable(introduce(final_data), format.args = list(big.mark = ","))

## ----eda-plot-missing----------------------------------------------------
plot_missing(final_data)

## ----eda-plot-bar-template, eval=FALSE-----------------------------------
#  plot_bar(final_data)

## ----eda-plot-bar-run, echo=FALSE, fig.width=10, fig.height=6------------
plot_bar(final_data, theme_config = list("text" = element_text(size = 4)))

## ----eda-update-manufacturer---------------------------------------------
final_data[which(final_data$manufacturer == "AIRBUS INDUSTRIE"),]$manufacturer <- "AIRBUS"
final_data[which(final_data$manufacturer == "CANADAIR LTD"),]$manufacturer <- "CANADAIR"
final_data[which(final_data$manufacturer %in% c("MCDONNELL DOUGLAS AIRCRAFT CO", "MCDONNELL DOUGLAS CORPORATION")),]$manufacturer <- "MCDONNELL DOUGLAS"

plot_bar(final_data$manufacturer)

## ----eda-plot-bar-with-template, eval=FALSE------------------------------
#  plot_bar(final_data, with = "arr_delay")

## ----eda-plot-bar-with-run, echo=FALSE, fig.width=10, fig.height=6-------
plot_bar(final_data, with = "arr_delay", theme_config = list("text" = element_text(size = 4)))

## ----eda-plot-histogram, fig.width=10, fig.height=6----------------------
plot_histogram(final_data)

## ----eda-update-flight---------------------------------------------------
final_data$flight <- as.factor(final_data$flight)

## ----eda-plot-correlation, fig.width=8, fig.height=8---------------------
plot_correlation(final_data, maxcat = 5L, use = "pairwise.complete.obs")

## ----eda-plot-correlation-type, eval=FALSE-------------------------------
#  plot_correlation(final_data, type = "c", use = "pairwise.complete.obs")
#  plot_correlation(final_data, type = "d", use = "pairwise.complete.obs")

## ----eda-plot-prcomp-----------------------------------------------------
pca_df <- na.omit(final_data[, c("origin", "name_carrier", "type", "engine", "dep_delay", "arr_delay", "air_time", "month", "hour", "year_planes", "seats", "speed")])

plot_prcomp(pca_df)

## ----eda-plot-boxplot, fig.width=8, fig.height=8-------------------------
## Reduce data size for demo purpose
arr_delay_df <- final_data[, c("arr_delay", "month", "day", "hour", "minute", "dep_delay", "distance", "year_planes", "seats", "speed")]

## Call boxplot function
plot_boxplot(arr_delay_df, by = "arr_delay")

## ----eda-plot-scatterplot, fig.width=8, fig.height=8---------------------
## Reduce data size for demo purpose
arr_delay_df2 <- final_data[sample.int(nrow(final_data), 1000), c("arr_delay", "dep_time", "dep_delay", "arr_time", "air_time", "distance", "year_planes", "seats", "speed")]

## Call scatterplot function
plot_scatterplot(arr_delay_df2, by = "arr_delay", size = 0.5)

## ----fe-set-missing, collapse=TRUE---------------------------------------
## Return data.frame
final_df <- set_missing(final_data, list(0L, "unknown"))
plot_missing(final_df)

## Update data.table by reference
# library(data.table)
# final_dt <- data.table(final_data)
# set_missing(final_dt, list(0L, "unknown"))
# plot_missing(final_dt)

## ----fe-group-category-count-trial---------------------------------------
group_category(data = final_data, feature = "manufacturer", threshold = 0.2)

## ----fe-group-category-count-update, results='hide'----------------------
final_df <- group_category(data = final_data, feature = "manufacturer", threshold = 0.2, update = TRUE)
plot_bar(final_df$manufacturer)

## ----fe-group-category-metric-trial--------------------------------------
group_category(data = final_data, feature = "name_carrier", threshold = 0.2, measure = "distance")

## ----fe-group-category-metric-update, results='hide'---------------------
final_df <- group_category(data = final_data, feature = "name_carrier", threshold = 0.2, measure = "distance", update = TRUE)
plot_bar(final_df$name_carrier)

## ----fe-dummify-template, eval=FALSE-------------------------------------
#  plot_str(
#  	list(
#  		"original" = final_data,
#  		"dummified" = dummify(final_data, maxcat = 5L)
#  	)
#  )

## ----fe-dummify-run, echo=FALSE------------------------------------------
diagonalNetwork(
  plot_str(list("original" = final_data,"dummified" = dummify(final_data, maxcat = 5L)), print_network = FALSE),
  width = 800,
  height = 1500,
  fontSize = 20,
  margin = list(
    "left" = 50,
    "right" = 50
  )
)

## ----fe-drop-columns-----------------------------------------------------
identical(
  drop_columns(final_data, c("dst_origin", "dst_dest", "tzone_dest")),
  drop_columns(final_data, c(34, 41, 42))
)

## ----dr-create-report, eval=FALSE----------------------------------------
#  create_report(final_data)

## ----dr-create-report-with-y, eval=FALSE---------------------------------
#  create_report(final_data, y = "arr_delay")

## ----dr-create-report-customize, eval=FALSE------------------------------
#  ## Customize report configuration
#  config <- list(
#    "introduce" = list(),
#    "plot_str" = list(
#      "type" = "diagonal",
#      "fontSize" = 35,
#      "width" = 1000,
#      "margin" = list("left" = 350, "right" = 250)
#    ),
#    "plot_missing" = list(),
#    "plot_histogram" = list(),
#    "plot_bar" = list(),
#    "plot_correlation" = list("use" = "pairwise.complete.obs"),
#    "plot_prcomp" = list(),
#    "plot_boxplot" = list(),
#    "plot_scatterplot" = list()
#  )
#  ## Create final report
#  create_report(final_data, y = "arr_delay", config = config)

