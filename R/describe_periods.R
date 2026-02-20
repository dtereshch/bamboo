#' Time Periods Completeness Description
#'
#' This function caluclates number of enitities/groups for each time period in panel data.
#' The function uses different ways to detect entity/group presence in each period.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with entities/groups presence summary by time period.
#'
#' @details
#' The data.frame contains the following columns:
#' \describe{
#'   \item{\code{[time]}}{Time period identifier (name matches the input `time` argument)}
#'   \item{\code{nominal}}{Number of rows in each period (all rows, regardless of data completeness)}
#'   \item{\code{observed}}{Number of rows with at least one non-NA value in substantive variables}
#'   \item{\code{complete}}{Number of rows without any NA values in substantive variables}
#' }
#'
#' Time periods are sorted naturally (numeric values as numbers, others alphabetically).
#'
#' The returned data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing additional information: `entities_nominal`, `entities_observed`, and `entities_complete`.
#'         Each is a named list where names are time periods and values are vectors of entity identifiers present in that period.}
#' }
#'
#' @seealso
#' [plot_periods()], [describe_balance()], [describe_patterns()]
#'
#' @examples
#' data(production)
#' describe_periods(production, group = "firm", time = "year")
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' describe_periods(panel_data)
#'
#' # Get entities with complete data in the 6th period
#' result <- describe_periods(production, group = "firm", time = "year")
#' attr(result, "details")$entities_complete[["6"]]
#'
#' @export
describe_periods <- function(data, group = NULL, time = NULL) {
  # Helper to sort unique values preserving original class
  sort_unique_preserve <- function(x) {
    ux <- unique(x)
    if (is.numeric(ux)) {
      sort(ux)
    } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
      sort(ux)
    } else if (is.factor(ux)) {
      # Convert to character, sort, then rebuild factor with sorted levels
      char_lev <- as.character(ux)
      sorted_char <- sort(char_lev)
      factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
    } else {
      sort(ux) # character, logical, etc.
    }
  }

  # Check for panel_data class and extract info from metadata
  if (inherits(data, "panel_data")) {
    metadata <- attr(data, "metadata")
    if (
      is.null(metadata) || is.null(metadata$group) || is.null(metadata$time)
    ) {
      stop(
        "Object has class 'panel_data' but missing or incomplete 'metadata' attribute."
      )
    }
    group <- metadata$group
    time <- metadata$time
  } else {
    # Handle regular data.frame
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame, not ", class(data)[1])
    }

    if (is.null(group) || is.null(time)) {
      stop(
        "For regular data.frames, both 'group' and 'time' arguments must be provided"
      )
    }
  }

  # Common validation
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

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  time_char <- as.character(time_orig)

  # Get unique time periods in original class, sorted appropriately
  unique_times_orig <- sort_unique_preserve(time_orig)
  # Character version for ordering and indexing
  unique_times_char <- as.character(unique_times_orig)

  # Identify substantive variables (excluding group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Initialize result vectors and entity lists
  nominal_counts <- integer(length(unique_times_orig))
  observed_counts <- integer(length(unique_times_orig))
  complete_counts <- integer(length(unique_times_orig))

  entities_nominal <- vector("list", length(unique_times_orig))
  entities_observed <- vector("list", length(unique_times_orig))
  entities_complete <- vector("list", length(unique_times_orig))

  names(entities_nominal) <- unique_times_char
  names(entities_observed) <- unique_times_char
  names(entities_complete) <- unique_times_char

  # Calculate statistics for each time period
  for (i in seq_along(unique_times_orig)) {
    current_time_char <- unique_times_char[i]

    # Get indices for current time period (using character)
    time_indices <- which(time_char == current_time_char)

    # Nominal count (all rows in this period)
    nominal_counts[i] <- length(time_indices)

    # Get all entities in this period (nominal) – original class
    if (length(time_indices) > 0) {
      entities_nominal[[i]] <- unique(group_orig[time_indices])
    } else {
      entities_nominal[[i]] <- vector(class(group_orig), 0)
    }

    if (length(time_indices) > 0) {
      # Extract data for current time period
      period_data <- data[time_indices, substantive_vars, drop = FALSE]
      period_groups_orig <- group_orig[time_indices]

      # Observed: at least one non-NA value in substantive variables
      has_some_data <- apply(period_data, 1, function(x) any(!is.na(x)))
      observed_counts[i] <- sum(has_some_data)

      # Get entities with observed data (original class)
      if (any(has_some_data)) {
        entities_observed[[i]] <- unique(period_groups_orig[has_some_data])
      } else {
        entities_observed[[i]] <- vector(class(group_orig), 0)
      }

      # Complete: no NA values in substantive variables
      has_all_data <- apply(period_data, 1, function(x) all(!is.na(x)))
      complete_counts[i] <- sum(has_all_data)

      # Get entities with complete data (original class)
      if (any(has_all_data)) {
        entities_complete[[i]] <- unique(period_groups_orig[has_all_data])
      } else {
        entities_complete[[i]] <- vector(class(group_orig), 0)
      }
    } else {
      observed_counts[i] <- 0
      complete_counts[i] <- 0
      entities_observed[[i]] <- vector(class(group_orig), 0)
      entities_complete[[i]] <- vector(class(group_orig), 0)
    }
  }

  # Create result data.frame – first column uses original class time values
  result_df <- data.frame(
    time_period = unique_times_orig,
    nominal = nominal_counts,
    observed = observed_counts,
    complete = complete_counts,
    stringsAsFactors = FALSE
  )

  # Rename first column to match the time variable name
  names(result_df)[1] <- time

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list with entity lists (original class)
  details <- list(
    entities_nominal = entities_nominal,
    entities_observed = entities_observed,
    entities_complete = entities_complete
  )

  # Set attributes in desired order
  attr(result_df, "metadata") <- metadata
  attr(result_df, "details") <- details

  # Set class
  class(result_df) <- c("panel_description", "data.frame")

  return(result_df)
}
