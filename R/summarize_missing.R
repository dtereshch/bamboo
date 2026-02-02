#' Missing Values Summary for Panel Data
#'
#' This function calculates summary statistics for missing values (NAs) in panel data,
#' providing both overall and detailed period-specific missing value counts.
#'
#' @param data A data.frame containing panel data.
#' @param selection A character vector specifying which variables to analyze for missing values.
#'        If not specified, all variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#' @param detailed A logical flag indicating whether to return detailed period-specific NA counts.
#'        Default = FALSE.
#'
#' @return A data.frame with missing value summary statistics.
#'
#' @details
#' When `detailed = FALSE` (default), returns a data.frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{na_count}}{Total number of missing values (NAs) for the variable}
#'   \item{\code{na_share}}{Proportion of observations that are missing (0 to 1)}
#'   \item{\code{entities_with_na}}{Number of entities/groups with at least one missing value for this variable}
#'   \item{\code{periods_with_na}}{Number of time periods with at least one missing value for this variable}
#' }
#'
#' When `detailed = TRUE`, additional columns are included:
#' \describe{
#'   \item{\code{[period1]}}{Column with NA counts for period1 (name matches the time period value)}
#'   \item{\code{[period2]}}{Column with NA counts for period2 (name matches the time period value)}
#'   \item{...}{Additional columns for each unique time period in the data}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{group_var}}{The grouping variable name}
#'   \item{\code{time_var}}{The time variable name}
#'   \item{\code{detailed}}{Logical indicating detailed output}
#'   \item{\code{total_observations}}{Total number of observations in the data}
#'   \item{\code{total_entities}}{Total number of unique entities/groups}
#'   \item{\code{total_periods}}{Total number of unique time periods}
#' }
#'
#' @seealso
#' [describe_incomplete()], [describe_balance()], [summarize_data()], [summarize_panel()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage with statistics for all variables
#' summarize_missing(production, group = "firm", time = "year")
#'
#' # Detailed output with period-specific NA counts
#' summarize_missing(production, group = "firm", time = "year", detailed = TRUE)
#'
#' # Show missing values for a specific variable
#' summarize_missing(production, selection = "sales", group = "firm", time = "year")
#'
#' # Show missing values for multiple variables
#' summarize_missing(production, selection = c("capital", "labor"), group = "firm", time = "year")
#'
#' @export
summarize_missing <- function(
  data,
  selection = NULL,
  group,
  time,
  detailed = FALSE
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!is.null(selection) && !is.character(selection)) {
    stop(
      "'selection' must be a character vector or NULL, not ",
      class(selection)[1]
    )
  }

  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string, not ", class(group)[1])
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string, not ", class(time)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("'detailed' must be a single logical value, not ", class(detailed)[1])
  }

  # Get data for analysis (similar to other functions)
  data_df <- .check_and_convert_data_robust(data, arg_name = "data")

  # If selection is not specified, use all variables except group and time
  if (is.null(selection)) {
    selection <- setdiff(names(data_df), c(group, time))

    if (length(selection) == 0) {
      stop("no variables found to analyze (besides group and time variables)")
    }

    message(
      "Analyzing all variable(s): ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection
  missing_vars <- selection[!selection %in% names(data_df)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Remove group and time from selection if included
  selection <- setdiff(selection, c(group, time))
  if (length(selection) == 0) {
    stop("no variables to analyze (excluding group and time variables)")
  }

  # Get unique time periods and sort them
  time_values <- as.character(data_df[[time]])
  unique_periods <- unique(time_values)

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    ordered_periods <- as.character(sort(as.numeric(unique_periods)))
  } else {
    ordered_periods <- sort(unique_periods)
  }

  # Get unique groups
  unique_groups <- unique(as.character(data_df[[group]]))

  # Total counts for attributes
  total_obs <- nrow(data_df)
  total_entities <- length(unique_groups)
  total_periods <- length(ordered_periods)

  # Initialize result list
  results <- list()

  # Calculate missing value statistics for each variable
  for (var in selection) {
    # Total NA count
    na_count <- sum(is.na(data_df[[var]]))

    # NA share (proportion)
    na_share <- ifelse(total_obs > 0, na_count / total_obs, 0)

    # Entities with at least one NA
    if (na_count > 0) {
      # Group by entity and check if any NA in the variable
      entity_has_na <- tapply(data_df[[var]], data_df[[group]], function(x) {
        any(is.na(x))
      })
      entities_with_na <- sum(entity_has_na, na.rm = TRUE)
    } else {
      entities_with_na <- 0
    }

    # Periods with at least one NA
    if (na_count > 0) {
      # Group by period and check if any NA in the variable
      period_has_na <- tapply(data_df[[var]], data_df[[time]], function(x) {
        any(is.na(x))
      })
      periods_with_na <- sum(period_has_na, na.rm = TRUE)
    } else {
      periods_with_na <- 0
    }

    # Create base result row
    result_row <- data.frame(
      variable = var,
      na_count = na_count,
      na_share = na_share,
      entities_with_na = entities_with_na,
      periods_with_na = periods_with_na,
      stringsAsFactors = FALSE
    )

    # Add detailed period-specific NA counts if requested
    if (detailed) {
      # Calculate NA counts for each period
      for (period in ordered_periods) {
        period_data <- data_df[time_values == period, var, drop = FALSE]
        period_na_count <- sum(is.na(period_data[[var]]))
        result_row[[period]] <- period_na_count
      }
    }

    results[[var]] <- result_row
  }

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add attributes
  attr(result_df, "group_var") <- group
  attr(result_df, "time_var") <- time
  attr(result_df, "detailed") <- detailed
  attr(result_df, "total_observations") <- total_obs
  attr(result_df, "total_entities") <- total_entities
  attr(result_df, "total_periods") <- total_periods

  return(result_df)
}
