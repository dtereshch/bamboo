#' Panel Data Description
#'
#' Provides basic summary statistics for panel data structure.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#' @param time A character string specifying the name of the time variable.
#'
#' @return Invisible list containing panel description statistics.
#'
#' @examples
#' data(production)
#' describe_panel(production, group = "firm", time = "year")
#'
#' @export
describe_panel <- function(data, group, time) {
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

  # Create presence matrix
  unique_groups <- unique(group_var)
  unique_times <- unique(time_var)

  presence_matrix <- matrix(
    0,
    nrow = length(unique_groups),
    ncol = length(unique_times),
    dimnames = list(unique_groups, unique_times)
  )

  for (i in seq_along(group_var)) {
    row_idx <- which(unique_groups == group_var[i])
    col_idx <- which(unique_times == time_var[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Calculate statistics
  total_entities <- length(unique_groups)
  n_periods <- length(unique_times)
  total_obs <- sum(presence_matrix)
  balanced_obs <- total_entities * n_periods
  missing_obs <- balanced_obs - total_obs
  pct_missing <- missing_obs / balanced_obs * 100
  entities_with_gaps <- sum(rowSums(presence_matrix) < n_periods)
  pct_entities_with_gaps <- entities_with_gaps / total_entities * 100

  # Count participation patterns
  pattern_strings <- apply(presence_matrix, 1, paste, collapse = "")
  n_patterns <- length(unique(pattern_strings))

  # Count filtered rows
  filtered_obs <- nrow(data) - nrow(filtered_data)

  # Create results list
  stats <- list(
    n_entities = total_entities,
    n_periods = n_periods,
    total_obs = total_obs,
    balanced_obs = balanced_obs,
    missing_obs = missing_obs,
    pct_missing = pct_missing,
    entities_with_gaps = entities_with_gaps,
    pct_entities_with_gaps = pct_entities_with_gaps,
    n_patterns = n_patterns,
    filtered_obs = filtered_obs,
    presence_matrix = presence_matrix,
    group_var = group,
    time_var = time,
    filtered_data = filtered_data
  )

  # Print formatted output
  cat("Panel Data Description\n\n")
  cat("Basic Statistics:\n")
  cat(sprintf("  Number of entities: %d\n", stats$n_entities))
  cat(sprintf("  Number of time periods: %d\n", stats$n_periods))
  cat(sprintf("  Total observations: %d\n", stats$total_obs))
  cat(sprintf("  Potential observations (balanced): %d\n", stats$balanced_obs))
  cat(sprintf(
    "  Missing observations: %d (%.1f%%)\n",
    stats$missing_obs,
    stats$pct_missing
  ))
  cat(sprintf(
    "  Entities with gaps: %d (%.1f%%)\n",
    stats$entities_with_gaps,
    stats$pct_entities_with_gaps
  ))
  cat(sprintf("  Number of participation patterns: %d\n", stats$n_patterns))
  if (stats$filtered_obs > 0) {
    cat(sprintf("  Rows filtered (all NAs): %d\n", stats$filtered_obs))
  }
  cat("\n")

  invisible(stats)
}
