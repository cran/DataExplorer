#' Plot missing value profile
#'
#' This function returns and plots frequency of missing values for each feature.
#' @param data input data
#' @param group missing profile band taking a list of group name and group upper bounds. Default is \code{list("Good" = 0.05, "OK" = 0.4, "Bad" = 0.8, "Remove" = 1)}.
#' @param group_color bar color list for each of the group. Default is \code{list("Good" = "#1B9E77", "OK" = "#E6AB02", "Bad" = "#D95F02", "Remove" = "#E41A1C")}.
#' @param missing_only plot features with missing values only? Default is \code{FALSE}.
#' @param geom_label_args a list of other arguments to \link[ggplot2]{geom_label}
#' @param title plot title
#' @param ggtheme complete ggplot2 themes. The default is \link[ggplot2]{theme_gray}.
#' @param theme_config a list of configurations to be passed to \link[ggplot2]{theme}.
#' @return invisibly return the ggplot object
#' @keywords plot_missing
#' @import ggplot2
#' @export
#' @seealso \link{profile_missing}
#' @examples
#' plot_missing(airquality)
#' plot_missing(airquality, missing_only = TRUE)
#' 
#' ## Customize band
#' plot_missing(airquality, group = list("B1" = 0, "B2" = 0.06, "B3" = 1))
#' plot_missing(airquality, group = list("Awesome!" = 0.2, "Oh no!" = 1),
#' group_color = list("Awesome!" = "green", "Oh no!" = "red"))
#' 
#' ## Shrink geom_label size
#' library(ggplot2)
#' plot_missing(airquality, geom_label_args = list("size" = 2,
#' "label.padding" = unit(0.1, "lines")))

plot_missing <- function(data,
                         group = list("Good" = 0.05, "OK" = 0.4, "Bad" = 0.8, "Remove" = 1),
                         group_color = list("Good" = "#1B9E77", "OK" = "#E6AB02", "Bad" = "#D95F02", "Remove" = "#E41A1C"),
                         missing_only = FALSE,
                         geom_label_args = list(),
                         title = NULL,
                         ggtheme = theme_gray(),
                         theme_config = list("legend.position" = c("bottom"))) {
  
  ## Declare variable first to pass R CMD check
  num_missing <- pct_missing <- Band <- NULL
  ## Profile missing values
  missing_value <- data.table(profile_missing(data))
  if (missing_only) missing_value <- missing_value[num_missing > 0]
  ## Sort group based on value
  group <- group[sort.list(unlist(group))]
  invisible(lapply(seq_along(group), function(i) {
    if (i == 1) {
      missing_value[pct_missing <= group[[i]], Band := names(group)[i]]
    } else {
      missing_value[pct_missing > group[[i-1]] & pct_missing <= group[[i]], Band := names(group)[i]]
    }
  }))
  
  ## Determine ordinal levels from group supplied
  ordinal_levels <- names(group[sort.list(unlist(group))])
  
  ## Convert character to ordered factor to support ordinal legend
  missing_value[, Band := factor(Band, levels = ordinal_levels, ordered = TRUE)]
  
  ## Determine bar fill color
  if (length(setdiff(names(group), names(group_color))) > 0) {
    bar_fill <- scale_fill_discrete("Band")
  } else {
    bar_fill <- scale_fill_manual(values = group_color)
  }
  
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "Band")) +
    geom_bar(stat = "identity") +
    bar_fill +
    coord_flip() +
    xlab("Features") + ylab("Missing Rows") +
    guides(fill = guide_legend(override.aes = aes(label = "")))
  geom_label_args_list <- list("mapping" = aes(label = paste0(round(100 * pct_missing, 2), "%")))
  output <- output +
    do.call("geom_label", c(geom_label_args_list, geom_label_args))
  ## Plot object
  class(output) <- c("single", class(output))
  plotDataExplorer(
    plot_obj = output,
    title = title,
    ggtheme = ggtheme,
    theme_config = theme_config
  )
}
