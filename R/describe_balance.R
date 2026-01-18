#' Panel Data Balance Description
#'
#' Provides comprehensive summary statistics for panel data structure with focus on balance
#' and data completeness.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#'
#' @return A data.frame with 6 columns and 3 rows containing panel data summary statistics.
#'   The data.frame has the following columns:
#'   \describe{
#'     \item{\code{Panel Info}}{Character vector describing the type of panel element.
#'       Contains three values: "Observations", "Entities", and "Periods".}
#'     \item{\code{Count (total)}}{Numeric vector with total counts for each panel element.
#'       For "Observations": total number of rows in the original data.
#'       For "Entities": total number of unique groups present in the data.
#'       For "Periods": total number of unique time periods present in the data.}
#'     \item{\code{Count (balanced)}}{Numeric vector with counts of balanced cases.
#'       For "Observations": always NA (balance concept not applicable at observation level).
#'       For "Entities": number of groups that are present in all time periods.
#'       For "Periods": number of time periods where all groups are present.}
#'     \item{\code{Share (balanced)}}{Numeric vector with proportions of balanced cases
#'       (ranging from 0 to 1). For "Observations": always NA. For "Entities" and "Periods":
#'       calculated as \code{Count (balanced)} / \code{Count (total)} for each row.}
#'     \item{\code{Count (without NA)}}{Numeric vector with counts of cases without missing values.
#'       For "Observations": number of rows with no NAs in substantive variables.
#'       For "Entities": number of groups with no NAs in any of their observations.
#'       For "Periods": number of time periods with no NAs in any observation.}
#'     \item{\code{Share (without NA)}}{Numeric vector with proportions of cases without missing values
#'       (ranging from 0 to 1). Calculated as \code{Count (without NA)} / \code{Count (total)} for each row.}
#'   }
#'   The data.frame has three rows corresponding to:
#'   \enumerate{
#'     \item{\strong{Observations}: Row-level information}
#'     \item{\strong{Entities}: Group-level balance and data completeness}
#'     \item{\strong{Periods}: Time-level balance and data completeness}
#'   }
#'
#' @seealso
#' [explore_balance()], [describe_periods()], [describe_participation()], [explore_participation], [plot_participation()]
#'
#' @examples
#' data(production)
#' describe_balance(production, group = "firm", time = "year")
#'
#' @export
describe_balance <- function(data, group, time) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame, not ", class(data)[1])
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Use original data for balance analysis (structural completeness)
  data_for_balance <- data

  # Get unique groups and periods from ALL data
  unique_groups <- unique(as.character(data_for_balance[[group]]))
  unique_periods <- unique(as.character(data_for_balance[[time]]))

  # Sort time periods if they appear numeric (matching explore_balance behavior)
  if (all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", unique_periods))) {
    unique_periods <- sort(as.numeric(unique_periods))
    unique_periods <- as.character(unique_periods)
  } else {
    unique_periods <- sort(unique_periods)
  }

  total_entities <- length(unique_groups)
  total_periods <- length(unique_periods)

  # Create presence matrix (1 = observation exists for entity-time pair)
  presence_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Create NA matrix (1 = observation has no NAs in substantive variables)
  na_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Fill presence and NA matrices
  group_vec <- as.character(data_for_balance[[group]])
  time_vec <- as.character(data_for_balance[[time]])

  # Pre-compute which rows have no NAs in substantive variables
  has_no_na <- apply(data_for_balance[substantive_vars], 1, function(x) {
    all(!is.na(x))
  })

  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_periods == time_vec[i])
    presence_matrix[row_idx, col_idx] <- 1
    if (has_no_na[i]) {
      na_matrix[row_idx, col_idx] <- 1
    }
  }

  # Calculate statistics for output data.frame

  # 1. Observations
  total_obs <- nrow(data)
  obs_without_na <- sum(has_no_na)
  share_obs_without_na <- obs_without_na / total_obs
  # For observations, balance concept is not applicable - set to NA
  balanced_obs <- NA
  share_balanced_obs <- NA

  # 2. Entities
  total_entities_count <- total_entities

  # Balanced entities (present in all periods)
  balanced_entities_count <- sum(rowSums(presence_matrix) == total_periods)
  share_entities <- balanced_entities_count / total_entities_count

  # Entities without NA (all observations for entity have no NAs)
  entities_without_na_count <- 0
  for (i in 1:total_entities) {
    entity_rows <- which(presence_matrix[i, ] == 1)
    if (length(entity_rows) > 0 && all(na_matrix[i, entity_rows] == 1)) {
      entities_without_na_count <- entities_without_na_count + 1
    }
  }
  share_entities_without_na <- entities_without_na_count / total_entities_count

  # 3. Periods
  total_periods_count <- total_periods

  # Balanced periods (all entities present)
  balanced_periods_count <- sum(colSums(presence_matrix) == total_entities)
  share_periods <- balanced_periods_count / total_periods_count

  # Periods without NA (all observations in period have no NAs)
  periods_without_na_count <- 0
  for (j in 1:total_periods) {
    period_cols <- which(presence_matrix[, j] == 1)
    if (length(period_cols) > 0 && all(na_matrix[period_cols, j] == 1)) {
      periods_without_na_count <- periods_without_na_count + 1
    }
  }
  share_periods_without_na <- periods_without_na_count / total_periods_count

  # Create and return the result data.frame
  result_df <- data.frame(
    `Panel Info` = c("Observations", "Entities", "Periods"),
    `Count (total)` = c(total_obs, total_entities_count, total_periods_count),
    `Count (balanced)` = c(
      balanced_obs,
      balanced_entities_count,
      balanced_periods_count
    ),
    `Share (balanced)` = c(share_balanced_obs, share_entities, share_periods),
    `Count (without NA)` = c(
      obs_without_na,
      entities_without_na_count,
      periods_without_na_count
    ),
    `Share (without NA)` = c(
      share_obs_without_na,
      share_entities_without_na,
      share_periods_without_na
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  return(result_df)
}
