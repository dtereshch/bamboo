#' Time Period Coverage Description
#'
#' Provides detailed coverage statistics for each time period in panel data.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#'
#' @return A data.frame with coverage statistics for each time period.
#'   The data.frame has the following columns:
#'   - Time period identifier (name matches the input `time` argument)
#'   - `Count`: Number of entities observed in each period
#'   - `Share (obs.)`: Share of total observations in each period (0 to 1)
#'   - `Share (entities)`: Share of entities observed in each period (0 to 1)
#'
#' @examples
#' data(production)
#' describe_periods(production, group = "firm", time = "year")
#'
#' @export
describe_periods <- function(data, group, time) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
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

  # Filter data for analysis
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  filtered_data <- data[has_data, ]

  # Extract variables
  group_var <- as.character(filtered_data[[group]])
  time_var <- as.character(filtered_data[[time]])

  # Get unique values
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  # Order time periods
  if (all(grepl("^-?\\d+\\.?\\d*$", unique_times))) {
    time_order <- order(as.numeric(unique_times))
  } else {
    time_order <- order(unique_times)
  }
  ordered_times <- unique_times[time_order]

  # Create presence matrix
  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(ordered_times),
    dimnames = list(unique_groups, ordered_times)
  )

  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(ordered_times == time_var[i])
    if (length(col_idx) > 0) {
      presence_matrix[row_idx, col_idx] <- 1
    }
  }

  # Calculate coverage statistics
  total_entities <- length(unique_groups)
  total_obs <- sum(presence_matrix)

  period_coverage <- colSums(presence_matrix)
  period_obs <- colSums(presence_matrix) # Same as coverage for binary presence

  # Calculate shares as proportions (0 to 1)
  share_entities <- period_coverage / total_entities
  share_obs <- period_obs / total_obs

  # Create result data.frame with new column names and order
  result_df <- data.frame(
    time_period = ordered_times,
    entities_count = period_coverage,
    obs_share = share_obs,
    entities_share = share_entities,
    stringsAsFactors = FALSE
  )

  # Rename columns according to new specification
  names(result_df) <- c(
    time,
    "Count",
    "Share (obs.)",
    "Share (entities)"
  )

  return(result_df)
}
