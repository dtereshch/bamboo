#' Panel Data Description
#'
#' Provides basic summary statistics for panel data structure.
#'
#' @param data A data.frame containing panel data.
#' @param group A character string specifying the name of the entity/group variable.
#' @param time A character string specifying the name of the time variable.
#'
#' @return A data.frame with 4 columns and 3 rows containing panel data summary statistics.
#'   The data.frame has the following columns:
#'   \describe{
#'     \item{\code{Panel Info}}{Character vector describing the type of panel element.
#'       Contains three values: "Observations", "Entities", and "Periods".}
#'     \item{\code{Count (total)}}{Numeric vector with total counts for each panel element.
#'       For "Observations": total number of rows in the original data.
#'       For "Entities": total number of unique groups present in the data.
#'       For "Periods": total number of unique time periods present in the data.}
#'     \item{\code{Count (complete)}}{Numeric vector with counts of complete cases.
#'       For "Observations": number of rows where all substantive variables (excluding
#'       group and time variables) have non-missing values.
#'       For "Entities": number of groups that are present in all time periods.
#'       For "Periods": number of time periods where all groups are present.}
#'     \item{\code{Share (complete)}}{Numeric vector with proportions of complete cases
#'       (ranging from 0 to 1). Calculated as \code{Count (complete)} / \code{Count (total)}
#'       for each row.}
#'   }
#'   The data.frame has three rows corresponding to:
#'   \enumerate{
#'     \item{\strong{Observations}: Row-level completeness of substantive variables}
#'     \item{\strong{Entities}: Group-level presence across all time periods}
#'     \item{\strong{Periods}: Time-level presence of all groups}
#'   }
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

  # Remove rows where all substantive variables are NA
  has_data <- apply(data[substantive_vars], 1, function(x) any(!is.na(x)))
  data_clean <- data[has_data, ]

  # Get unique groups and periods
  unique_groups <- unique(as.character(data_clean[[group]]))
  unique_periods <- unique(as.character(data_clean[[time]]))

  total_entities <- length(unique_groups)
  total_periods <- length(unique_periods)

  # Create presence matrix (1 = observation exists)
  presence_matrix <- matrix(
    0,
    nrow = total_entities,
    ncol = total_periods,
    dimnames = list(unique_groups, unique_periods)
  )

  # Fill presence matrix
  group_vec <- as.character(data_clean[[group]])
  time_vec <- as.character(data_clean[[time]])

  for (i in seq_along(group_vec)) {
    row_idx <- which(unique_groups == group_vec[i])
    col_idx <- which(unique_periods == time_vec[i])
    presence_matrix[row_idx, col_idx] <- 1
  }

  # Calculate statistics for output data.frame

  # 1. Observations
  total_obs <- nrow(data)
  complete_obs <- sum(apply(data[substantive_vars], 1, function(x) {
    all(!is.na(x))
  }))
  share_obs <- complete_obs / total_obs

  # 2. Entities (complete = present in all periods)
  total_entities_count <- total_entities
  complete_entities_count <- sum(rowSums(presence_matrix) == total_periods)
  share_entities <- complete_entities_count / total_entities_count

  # 3. Periods (complete = all entities present)
  total_periods_count <- total_periods
  complete_periods_count <- sum(colSums(presence_matrix) == total_entities)
  share_periods <- complete_periods_count / total_periods_count

  # Create and return the result data.frame
  result_df <- data.frame(
    `Panel Info` = c("Observations", "Entities", "Periods"),
    `Count (total)` = c(total_obs, total_entities_count, total_periods_count),
    `Count (complete)` = c(
      complete_obs,
      complete_entities_count,
      complete_periods_count
    ),
    `Share (complete)` = c(share_obs, share_entities, share_periods),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  return(result_df)
}
