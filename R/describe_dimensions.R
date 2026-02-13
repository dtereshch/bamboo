#' Panel Data Dimensions Description
#'
#' This function provides summary information about panel data structure across
#' three presence types: nominal, observed, and complete.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with 4 columns:
#' \describe{
#'   \item{\code{dimension}}{Dimension name: "rows", "entities", or "periods"}
#'   \item{\code{nominal}}{Count based on presence of any row (even with only panel ID variables)}
#'   \item{\code{observed}}{Count based on presence of at least one non-NA substantive variable}
#'   \item{\code{complete}}{Count based on presence of no NA values in all substantive variables}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{panel_group}}{The grouping variable name}
#'   \item{\code{panel_time}}{The time variable name}
#'   \item{\code{entity_values}}{Vector of all unique entity/group values}
#'   \item{\code{period_values}}{Vector of all unique time period values}
#'   \item{\code{panel_total_rows}}{Total number of rows in the data}
#'   \item{\code{panel_substantive_vars}}{Vector of substantive variable names}
#' }
#'
#' @details
#' This function provides panel data structure information across three presence
#' definitions:
#' \itemize{
#'   \item{\bold{nominal:}}{ Entity/time is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\bold{observed:}}{ Entity is present if it has at least one non-NA substantive variable}
#'   \item{\bold{complete:}}{ Entity/time is present only if it has no NA values in all substantive variables}
#' }
#'
#' For "entities", counts represent the number of unique entities present in the data.
#' For "periods", counts represent the number of unique time periods present in the data.
#' For "rows", counts represent the total number of rows meeting each presence criterion.
#'
#' When provided with a data.frame that has panel attributes (created by set_panel()),
#' the function automatically extracts group and time variable names from the attributes.
#'
#' @examples
#' data(production)
#'
#' # Method 1: With regular data.frame
#' panel_desc <- describe_dimensions(production, group = "firm", time = "year")
#' print(panel_desc)
#'
#' # Method 2: With data.frame with panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_desc <- describe_dimensions(panel_data)
#' print(panel_desc)
#'
#' @seealso
#' [check_panel()], [describe_balance()], [describe_periods()], [set_panel()]
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
  # Check if data has panel attributes
  has_panel_attrs <- !is.null(attr(data, "panel_group")) &&
    !is.null(attr(data, "panel_time"))

  if (has_panel_attrs) {
    # Extract group and time from attributes
    group <- attr(data, "panel_group")
    time <- attr(data, "panel_time")
  } else {
    # Handle regular data.frame
    # Input validation for regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation for both cases
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('group variable "', group, '" not found in data')
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!time %in% names(data)) {
    stop('time variable "', time, '" not found in data')
  }

  if (time == group) {
    stop("'time' and 'group' cannot be the same variable")
  }

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Get unique values
  entity_values <- sort(unique(data[[group]]))
  period_values <- sort(unique(data[[time]]))

  # Sort time periods if they appear numeric
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", period_values))) {
    period_values <- as.character(sort(as.numeric(period_values)))
  } else {
    period_values <- sort(period_values)
  }

  # Calculate counts for each presence type

  # 1. ROWS
  # Nominal: all rows
  rows_nominal <- nrow(data)

  # Observed: rows with at least one non-NA in substantive variables
  rows_observed <- sum(apply(data[substantive_vars], 1, function(x) {
    any(!is.na(x))
  }))

  # Complete: rows with all substantive variables non-NA
  rows_complete <- sum(apply(data[substantive_vars], 1, function(x) {
    all(!is.na(x))
  }))

  # 2. ENTITIES
  # Create entity-level presence indicators
  entity_nominal <- length(entity_values)

  # For observed and complete, we need to check per entity across all their rows
  entity_observed_count <- 0
  entity_complete_count <- 0

  for (ent in entity_values) {
    ent_rows <- data[[group]] == ent

    # Check if entity has any rows with observed data
    if (
      any(apply(data[ent_rows, substantive_vars, drop = FALSE], 1, function(x) {
        any(!is.na(x))
      }))
    ) {
      entity_observed_count <- entity_observed_count + 1
    }

    # Check if entity has at least one complete row
    if (
      any(apply(data[ent_rows, substantive_vars, drop = FALSE], 1, function(x) {
        all(!is.na(x))
      }))
    ) {
      entity_complete_count <- entity_complete_count + 1
    }
  }

  # 3. PERIODS
  # Create period-level presence indicators
  period_nominal <- length(period_values)

  # For observed and complete, we need to check per period across all their rows
  period_observed_count <- 0
  period_complete_count <- 0

  for (per in period_values) {
    per_rows <- data[[time]] == per

    # Check if period has any rows with observed data
    if (
      any(apply(data[per_rows, substantive_vars, drop = FALSE], 1, function(x) {
        any(!is.na(x))
      }))
    ) {
      period_observed_count <- period_observed_count + 1
    }

    # Check if period has at least one complete row
    if (
      any(apply(data[per_rows, substantive_vars, drop = FALSE], 1, function(x) {
        all(!is.na(x))
      }))
    ) {
      period_complete_count <- period_complete_count + 1
    }
  }

  # Create result data.frame
  result <- data.frame(
    dimension = c("rows", "entities", "periods"),
    nominal = c(rows_nominal, entity_nominal, period_nominal),
    observed = c(rows_observed, entity_observed_count, period_observed_count),
    complete = c(rows_complete, entity_complete_count, period_complete_count),
    stringsAsFactors = FALSE
  )

  # Add standardized attributes
  attr(result, "panel_group") <- group
  attr(result, "panel_time") <- time
  attr(result, "entity_values") <- entity_values
  attr(result, "period_values") <- period_values
  attr(result, "panel_total_rows") <- nrow(data)
  attr(result, "panel_substantive_vars") <- substantive_vars

  return(result)
}
