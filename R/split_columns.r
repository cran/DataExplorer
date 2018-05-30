#' Split data into discrete and continuous parts
#'
#' This function splits the input data into two \link{data.table} objects: discrete and continuous. A feature is continuous if \code{is.numeric} returns \code{TRUE}.
#' @param data input data
#' @keywords split_columns
#' @aliases SplitColType
#' @details Features with all missing values will be dropped from the output data, but will be counted towards the column count.
#' @details The elements in the output list will have the same class as the input data.
#' @return \code{discrete} all discrete features
#' @return \code{continous} all continuous features
#' @return \code{num_discrete} number of discrete features
#' @return \code{num_continuous} number of continuous features
#' @return \code{num_all_missing} number of features with no observations (all values are missing)
#' @import data.table
#' @export split_columns SplitColType
#' @examples
#' output <- split_columns(iris)
#' output$discrete
#' output$continuous
#' output$num_discrete
#' output$num_continuous
#' output$num_all_missing

split_columns <- function(data) {
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Find indicies for continuous features
  all_missing_ind <- .getAllMissing(data)
  ind <- sapply(data[, which(!all_missing_ind), with = FALSE], is.numeric)
  ## Count number of discrete, continuous and all-missing features
  n_all_missing <- sum(all_missing_ind)
  n_continuous <- sum(ind)
  n_discrete <- ncol(data) - n_continuous - n_all_missing
  ## Create object for continuous features
  continuous <- data[, which(ind), with = FALSE]
  ## Create object for discrete features
  discrete <- data[, which(!ind), with = FALSE]
  ## Set data class back to original
  if (!is_data_table) class(discrete) <- class(continuous) <- data_class
  ## Set return object
  return(
    list(
      "discrete" = discrete,
      "continuous" = continuous,
      "num_discrete" = n_discrete,
      "num_continuous" = n_continuous,
      "num_all_missing" = n_all_missing
    )
  )
}

SplitColType <- function(data) {
  .Deprecated("split_columns")
  split_columns(data)
}
