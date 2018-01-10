#' Plot missing value profile
#'
#' This function returns and plots frequency of missing values for each feature.
#' @param data input data to be profiled, in either \link{data.frame} or \link{data.table} format.
#' @param title plot title
#' @keywords plot_missing
#' @aliases PlotMissing
#' @details The returned object is suppressed by \link{invisible}.
#' @return missing value information, such as frequency, percentage and suggested action.
#' @import data.table
#' @import ggplot2
#' @importFrom scales comma
#' @export plot_missing PlotMissing
#' @examples
#' # load packages
#' library(data.table)
#'
#' # manipulate data
#' dt <- data.table(iris)
#' for (j in 1:4) set(dt, i = sample(150, j * 30), j, value = NA_integer_)
#'
#' # plot and assign missing value information
#' na_profile <- plot_missing(dt)
#' na_profile

plot_missing <- function(data, title = NULL) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) {data <- data.table(data)}
  ## Extract missing value distribution
  missing_value <- data.table("feature" = names(data), "num_missing" = sapply(data, function(x) {sum(is.na(x))}))
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) {class(missing_value) <- data_class}
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity", colour = "black", alpha = 0.4) +
    geom_text(aes(label = paste0(round(100 * pct_missing, 0), "%")), hjust = -0.15, size = 3.5) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = c("bottom")) + coord_flip() +
    xlab("Features") + ylab("Number of missing rows") + ggtitle(title)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}

PlotMissing <- function(data, title = NULL) {
  .Deprecated("plot_missing")
  plot_missing(data = data, title = title)
}
