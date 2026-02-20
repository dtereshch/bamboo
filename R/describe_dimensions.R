#' Panel Data Dimensions Description
#'
#' This function calculates number of rows, entities/groups, and time periods for panel data.
#' The function uses different ways to define the completeness of each row, entity/group, and period.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with panel dimension counts.
#'
#' @details
#' This function provides panel data structure information across three presence
#' definitions:
#' \itemize{
#'   \item{\bold{nominal:}}{ Entity/time is present if it has a row in the data (even with only panel ID variables)}
#'   \item{\bold{observed:}}{
#'     \itemize{
#'       \item{For rows: Row has at least one non-NA substantive variable}
#'       \item{For entities: Entity has at least one non-NA substantive variable at \bold{each} time period}
#'       \item{For periods: Period has at least one non-NA substantive variable for \bold{all} entities}
#'     }
#'   }
#'   \item{\bold{complete:}}{
#'     \itemize{
#'       \item{For rows: Row has no NA values in all substantive variables}
#'       \item{For entities: Entity has no NA values in all substantive variables at \bold{each} time period}
#'       \item{For periods: Period has no NA values in all substantive variables for \bold{all} entities}
#'     }
#'   }
#' }
#'
#' The returned data.frame has 4 columns:
#' \describe{
#'   \item{\code{dimension}}{Dimension name: "rows", "entities", or "periods"}
#'   \item{\code{nominal}}{Count based on presence of any row (even with only panel ID variables)}
#'   \item{\code{observed}}{Count based on presence of at least one non-NA substantive variable}
#'   \item{\code{complete}}{Count based on presence of no NA values in all substantive variables}
#' }
#'
#' The data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing detailed information about which entities, periods,
#'         and entity-period combinations meet each presence criterion:
#'         \itemize{
#'           \item{\code{entities_nominal}: All entity values present in the data}
#'           \item{\code{entities_observed}: Entity values that have observed data in all periods where they appear}
#'           \item{\code{entities_complete}: Entity values that have complete data in all periods where they appear}
#'           \item{\code{periods_nominal}: All time period values present in the data}
#'           \item{\code{periods_observed}: Period values that have observed data for all entities that appear in that period}
#'           \item{\code{periods_complete}: Period values that have complete data for all entities that appear in that period}
#'           \item{\code{rows_nominal}: All entity-period combinations (as "entity_time" strings) present in the data}
#'           \item{\code{rows_observed}: Entity-period combinations that have at least one non-NA substantive variable}
#'           \item{\code{rows_complete}: Entity-period combinations that have no NA values in all substantive variables}
#'         }
#'       }
#' }
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
#' # Access detailed information
#' attr(panel_desc, "details")$entities_observed
#' attr(panel_desc, "details")$rows_complete
#'
#' @seealso
#' [check_panel()], [describe_balance()], [describe_periods()], [set_panel()]
#'
#' @export
describe_dimensions <- function(data, group = NULL, time = NULL) {
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

  # Original vectors
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  group_char <- as.character(group_orig)
  time_char <- as.character(time_orig)

  # Sorted unique values in original class (for storage)
  entity_values <- sort_unique_preserve(group_orig)
  period_values <- sort_unique_preserve(time_orig)

  # Character versions of these (for dimnames, etc.)
  entity_char <- as.character(entity_values)
  period_char <- as.character(period_values)

  # Get substantive variables (all except group and time)
  substantive_vars <- setdiff(names(data), c(group, time))

  if (length(substantive_vars) == 0) {
    stop("no substantive variables found (besides group and time variables)")
  }

  # Create presence matrices for different criteria
  # Matrix dimensions: entities × periods
  observed_matrix <- matrix(
    FALSE,
    nrow = length(entity_values),
    ncol = length(period_values),
    dimnames = list(entity_char, period_char)
  )

  complete_matrix <- matrix(
    FALSE,
    nrow = length(entity_values),
    ncol = length(period_values),
    dimnames = list(entity_char, period_char)
  )

  # Fill matrices based on data completeness (using character matching)
  for (i in seq_len(nrow(data))) {
    i_row <- match(group_char[i], entity_char)
    j_col <- match(time_char[i], period_char)

    # Check observed criteria (at least one non-NA)
    if (any(!is.na(data[i, substantive_vars]))) {
      observed_matrix[i_row, j_col] <- TRUE
    }

    # Check complete criteria (all non-NA)
    if (all(!is.na(data[i, substantive_vars]))) {
      complete_matrix[i_row, j_col] <- TRUE
    }
  }

  # Calculate counts for each presence type

  # 1. ROWS
  rows_nominal <- nrow(data)
  rows_observed <- sum(apply(data[substantive_vars], 1, function(x) {
    any(!is.na(x))
  }))
  rows_complete <- sum(apply(data[substantive_vars], 1, function(x) {
    all(!is.na(x))
  }))

  # 2. ENTITIES
  entities_nominal <- length(entity_values)

  # Logical vectors indicating which entities satisfy observed/complete
  entity_observed_logical <- logical(entities_nominal)
  entity_complete_logical <- logical(entities_nominal)

  for (i in seq_along(entity_values)) {
    ent_char <- entity_char[i]
    # Get periods where this entity has any row (using character)
    entity_rows_logical <- group_char == ent_char
    periods_for_entity_char <- unique(time_char[entity_rows_logical])

    if (length(periods_for_entity_char) > 0) {
      # Check observed: for all periods this entity appears, it must have observed data
      observed_in_all <- all(sapply(periods_for_entity_char, function(p) {
        col_idx <- which(period_char == p)
        observed_matrix[i, col_idx]
      }))
      if (observed_in_all) {
        entity_observed_logical[i] <- TRUE
      }

      # Check complete: for all periods this entity appears, it must have complete data
      complete_in_all <- all(sapply(periods_for_entity_char, function(p) {
        col_idx <- which(period_char == p)
        complete_matrix[i, col_idx]
      }))
      if (complete_in_all) entity_complete_logical[i] <- TRUE
    }
  }

  entities_observed <- entity_values[entity_observed_logical]
  entities_complete <- entity_values[entity_complete_logical]

  # 3. PERIODS
  periods_nominal <- length(period_values)

  period_observed_logical <- logical(periods_nominal)
  period_complete_logical <- logical(periods_nominal)

  for (j in seq_along(period_values)) {
    per_char <- period_char[j]
    # Get entities that exist in this period (have any row)
    period_rows_logical <- time_char == per_char
    entities_in_period_char <- unique(group_char[period_rows_logical])

    if (length(entities_in_period_char) > 0) {
      # Check observed: for all entities in this period, they must have observed data
      observed_for_all <- all(sapply(entities_in_period_char, function(e) {
        row_idx <- which(entity_char == e)
        observed_matrix[row_idx, j]
      }))
      if (observed_for_all) {
        period_observed_logical[j] <- TRUE
      }

      # Check complete: for all entities in this period, they must have complete data
      complete_for_all <- all(sapply(entities_in_period_char, function(e) {
        row_idx <- which(entity_char == e)
        complete_matrix[row_idx, j]
      }))
      if (complete_for_all) period_complete_logical[j] <- TRUE
    }
  }

  periods_observed <- period_values[period_observed_logical]
  periods_complete <- period_values[period_complete_logical]

  # Create result data.frame
  result <- data.frame(
    dimension = c("rows", "entities", "periods"),
    nominal = c(rows_nominal, entities_nominal, periods_nominal),
    observed = c(
      rows_observed,
      length(entities_observed),
      length(periods_observed)
    ),
    complete = c(
      rows_complete,
      length(entities_complete),
      length(periods_complete)
    ),
    stringsAsFactors = FALSE
  )

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Build details list with vectors in original class
  details <- list(
    # Entity values by presence type
    entities_nominal = entity_values,
    entities_observed = entities_observed,
    entities_complete = entities_complete,

    # Period values by presence type
    periods_nominal = period_values,
    periods_observed = periods_observed,
    periods_complete = periods_complete,

    # Entity-period combinations (rows) by presence type (character strings – unchanged)
    rows_nominal = paste(group_char, time_char, sep = "_"),
    rows_observed = paste(group_char, time_char, sep = "_")[apply(
      data[substantive_vars],
      1,
      function(x) any(!is.na(x))
    )],
    rows_complete = paste(group_char, time_char, sep = "_")[apply(
      data[substantive_vars],
      1,
      function(x) all(!is.na(x))
    )]
  )

  # Set attributes in desired order
  attr(result, "metadata") <- metadata
  attr(result, "details") <- details

  # Set class
  class(result) <- c("panel_description", "data.frame")

  return(result)
}
