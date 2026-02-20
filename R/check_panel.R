#' Panel Data Structure Validation
#'
#' This function performs comprehensive validation of panel data structure
#' and returns a data.frame with validation test results along with diagnostic
#' information for any issues found.
#'
#' @param data A data.frame containing panel data, or a data.frame with panel attributes.
#' @param group A character string specifying the name of the entity/group variable in panel data.
#'        Not required if data has panel attributes.
#' @param time A character string specifying the name of the time variable in panel data.
#'        Not required if data has panel attributes.
#'
#' @return A data.frame with validation test results.
#'
#' @details
#' The function performs the following validation tests:
#' \itemize{
#'   \item \strong{Data structure}: Checks if data is a valid data.frame
#'   \item \strong{Group variable}: Verifies existence of group variable
#'   \item \strong{Group completeness}: Checks for missing values in group variable
#'   \item \strong{Time variable}: Verifies existence of time variable
#'   \item \strong{Time completeness}: Checks for missing values in time variable
#'   \item \strong{Group-time distinct}: Ensures group and time are different variables
#'   \item \strong{Duplicates}: Identifies duplicate group-time combinations
#'   \item \strong{Balance}: Checks if panel is balanced (all groups have same time points)
#'   \item \strong{Time sequence}: Checks regularity of time sequence in entire panel
#'   \item \strong{Group intervals}: Checks regularity of time intervals within groups
#' }
#'
#' The returned data.frame has three columns: "test", "status", "message".
#' The first row contains an overall panel status, and subsequent rows contain
#' individual test results with human-readable test names.
#'
#' The data.frame has class `"panel_description"` and the following attributes:
#' \describe{
#'   \item{`metadata`}{List containing the function name and the arguments used.}
#'   \item{`details`}{List containing diagnostic information for each test,
#'         including problematic observations/entities/periods, and logical
#'         summary of test results.}
#' }
#'
#' @seealso
#' [set_panel()], [describe_dimensions()], [describe_balance()]
#'
#' @examples
#' data(production)
#'
#' # Basic usage
#' panel_check <- check_panel(production, group = "firm", time = "year")
#'
#' # View test results
#' print(panel_check)
#'
#' # Access test results as data.frame
#' test_results <- as.data.frame(panel_check)
#'
#' # Access attributes
#' details_info <- attr(panel_check, "details")
#' meta_info <- attr(panel_check, "metadata")
#'
#' # Get specific details
#' has_complete_groups <- details_info$group_completeness
#' duplicate_obs <- details_info$duplicate_observations
#' unbalanced_entities <- details_info$unbalanced_groups
#'
#' # With panel attributes
#' panel_data <- set_panel(production, group = "firm", time = "year")
#' panel_check2 <- check_panel(panel_data)
#'
#' @export
check_panel <- function(data, group = NULL, time = NULL) {
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

  # Original vectors (preserve class)
  group_orig <- data[[group]]
  time_orig <- data[[time]]

  # Character versions for internal matching
  group_char <- as.character(group_orig)
  time_char <- as.character(time_orig)

  # Sorted unique values in original class (for storage)
  unique_groups_orig <- sort_unique_preserve(group_orig)
  unique_times_orig <- sort_unique_preserve(time_orig)

  # Check for missing values
  if (any(is.na(group_orig))) {
    warning("group variable '", group, "' contains missing values")
  }

  if (any(is.na(time_orig))) {
    warning("time variable '", time, "' contains missing values")
  }

  # Initialize exploration results
  exploration_results <- data.frame(
    variable = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )

  # Panel structure
  n_groups <- length(unique_groups_orig)
  n_periods <- length(unique_times_orig)
  n_obs <- nrow(data)

  # Create combo identifier (character)
  combos <- paste(group_char, time_char, sep = "|")

  # Check for duplicate group-time combinations
  has_duplicates <- any(duplicated(combos))

  # Identify duplicate indices
  duplicate_indices <- which(
    duplicated(combos) | duplicated(combos, fromLast = TRUE)
  )
  duplicate_rows <- data[duplicate_indices, ]

  # Create unique identifier for duplicates
  duplicate_combos <- unique(combos[duplicated(combos)])
  duplicate_summary <- data.frame(
    combo = duplicate_combos,
    count = sapply(duplicate_combos, function(x) sum(combos == x)),
    stringsAsFactors = FALSE
  )

  # Split combos into group and time for the summary
  if (nrow(duplicate_summary) > 0) {
    split_combos <- strsplit(duplicate_summary$combo, "\\|")
    duplicate_summary$group <- sapply(split_combos, function(x) x[1])
    duplicate_summary$time <- sapply(split_combos, function(x) x[2])
  }

  # Check for balanced panel
  time_by_group <- split(time_orig, group_orig) # keep original for internal use
  unique_time_sets <- lapply(time_by_group, unique)
  time_set_lengths <- sapply(unique_time_sets, length)

  if (length(unique(time_set_lengths)) == 1) {
    all_time_sets <- unique(unique_time_sets)
    is_balanced <- length(all_time_sets) == 1
  } else {
    is_balanced <- FALSE
  }

  # Identify groups with missing time points (if unbalanced)
  missing_time_info <- list()
  if (!is_balanced) {
    # All unique time points (character for easy comparison)
    all_times_char <- sort(unique(time_char))

    # For each group (original class), find missing time points
    for (grp_val in unique_groups_orig) {
      grp_char <- as.character(grp_val)
      group_times_char <- unique(time_char[group_char == grp_char])
      missing_times_char <- setdiff(all_times_char, group_times_char)
      if (length(missing_times_char) > 0) {
        # Convert back to original class by matching
        missing_times_orig <- unique_times_orig[match(
          missing_times_char,
          as.character(unique_times_orig)
        )]
        missing_time_info[[grp_char]] <- missing_times_orig
      }
    }
  }

  # Check for irregular time sequence in entire panel
  has_irregular_time_sequence <- FALSE
  time_sequence_regular <- TRUE

  if (
    is.numeric(time_orig) ||
      inherits(time_orig, "Date") ||
      inherits(time_orig, "POSIXt")
  ) {
    all_unique_times <- sort(unique(time_orig)) # preserves class
    if (length(all_unique_times) > 1) {
      global_intervals <- diff(all_unique_times)
      if (length(unique(global_intervals)) > 1) {
        has_irregular_time_sequence <- TRUE
        time_sequence_regular <- FALSE
      }
    }
  }

  # Check for irregular time intervals within groups
  has_irregular_intervals <- FALSE
  irregular_groups_orig <- vector("list", 0) # will store original class values
  interval_details <- list()

  if (
    is.numeric(time_orig) ||
      inherits(time_orig, "Date") ||
      inherits(time_orig, "POSIXt")
  ) {
    for (grp_val in unique_groups_orig) {
      grp_char <- as.character(grp_val)
      times <- time_orig[group_char == grp_char] # original class
      unique_times <- sort(unique(times))
      if (length(unique_times) > 1) {
        intervals <- diff(unique_times)
        if (length(unique(intervals)) > 1) {
          has_irregular_intervals <- TRUE
          irregular_groups_orig <- c(irregular_groups_orig, grp_val)
          interval_details[[grp_char]] <- list(
            times = unique_times,
            intervals = intervals,
            is_regular = FALSE
          )
        } else {
          interval_details[[grp_char]] <- list(
            times = unique_times,
            intervals = intervals,
            is_regular = TRUE
          )
        }
      } else {
        interval_details[[grp_char]] <- list(
          times = unique_times,
          intervals = numeric(0),
          is_regular = TRUE
        )
      }
    }
  }

  # Calculate observations per group
  group_table <- table(group_orig)
  avg_obs_per_group <- mean(group_table)

  # Get groups with minimum and maximum observations
  min_obs <- min(group_table)
  max_obs <- max(group_table)
  groups_with_min_obs <- names(group_table)[group_table == min_obs]
  groups_with_max_obs <- names(group_table)[group_table == max_obs]

  # Build exploration results with human-readable test names (unchanged)
  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Data structure",
      status = "PASS",
      message = "Valid data.frame structure",
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group variable",
      status = "PASS",
      message = paste("Group variable '", group, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group completeness",
      status = ifelse(any(is.na(group_orig)), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(group_orig)),
        paste(
          "Group variable has",
          sum(is.na(group_orig)),
          "missing values"
        ),
        "No missing values in group variable"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Time variable",
      status = "PASS",
      message = paste("Time variable '", time, "' found", sep = ""),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Time completeness",
      status = ifelse(any(is.na(time_orig)), "WARNING", "PASS"),
      message = ifelse(
        any(is.na(time_orig)),
        paste("Time variable has", sum(is.na(time_orig)), "missing values"),
        "No missing values in time variable"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Group-time distinct",
      status = "PASS",
      message = "Group and time are distinct variables",
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Duplicates",
      status = ifelse(has_duplicates, "FAIL", "PASS"),
      message = ifelse(
        has_duplicates,
        paste(sum(duplicated(combos)), "duplicate group-time pairs found"),
        "No duplicate group-time pairs"
      ),
      stringsAsFactors = FALSE
    )
  )

  exploration_results <- rbind(
    exploration_results,
    data.frame(
      variable = "Balance",
      status = ifelse(is_balanced, "PASS", "FAIL"),
      message = ifelse(is_balanced, "Panel is balanced", "Panel is unbalanced"),
      stringsAsFactors = FALSE
    )
  )

  # Check time sequence regularity (entire panel)
  if (
    is.numeric(time_orig) ||
      inherits(time_orig, "Date") ||
      inherits(time_orig, "POSIXt")
  ) {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Time sequence",
        status = ifelse(has_irregular_time_sequence, "FAIL", "PASS"),
        message = ifelse(
          has_irregular_time_sequence,
          "Irregular time sequence in entire panel",
          "Regular time sequence in entire panel"
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Time sequence",
        status = "INFO",
        message = "Time variable is not numeric/Date-like",
        stringsAsFactors = FALSE
      )
    )
  }

  # Check interval regularity within groups
  if (
    is.numeric(time_orig) ||
      inherits(time_orig, "Date") ||
      inherits(time_orig, "POSIXt")
  ) {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Group intervals",
        status = ifelse(has_irregular_intervals, "FAIL", "PASS"),
        message = ifelse(
          has_irregular_intervals,
          "Irregular time intervals within groups",
          "Regular time intervals within groups"
        ),
        stringsAsFactors = FALSE
      )
    )
  } else {
    exploration_results <- rbind(
      exploration_results,
      data.frame(
        variable = "Group intervals",
        status = "INFO",
        message = "Time variable is not numeric/Date-like",
        stringsAsFactors = FALSE
      )
    )
  }

  # Determine overall status
  overall_status <- ifelse(
    any(exploration_results$status %in% c("FAIL", "WARNING")),
    ifelse(any(exploration_results$status == "FAIL"), "FAIL", "WARNING"),
    "PASS"
  )

  # Create simplified details list with problematic observations only,
  # now using original class for group/time identifiers.
  details_list <- list()

  if (any(is.na(group_orig))) {
    details_list$missing_group_values <- which(is.na(group_orig))
  }

  if (any(is.na(time_orig))) {
    details_list$missing_time_values <- which(is.na(time_orig))
  }

  if (has_duplicates) {
    details_list$duplicate_observations <- duplicate_indices
    details_list$duplicate_combinations <- duplicate_combos # character combos – fine
  }

  if (!is_balanced) {
    # unbalanced_groups: original class
    details_list$unbalanced_groups <- unique_groups_orig[sapply(
      unique_groups_orig,
      function(g) {
        grp_char <- as.character(g)
        !is.null(missing_time_info[[grp_char]])
      }
    )]
    if (length(details_list$unbalanced_groups) > 0) {
      # missing_time_points already stored with original class times
      details_list$missing_time_points <- missing_time_info
    }
  }

  if (has_irregular_time_sequence) {
    details_list$irregular_time_intervals <- diff(sort(unique(time_orig)))
    details_list$all_time_points <- sort(unique(time_orig)) # original class
  }

  if (has_irregular_intervals) {
    # irregular_interval_groups: original class
    details_list$irregular_interval_groups <- irregular_groups_orig
    # group_interval_details already contains original class times
    details_list$group_interval_details <- interval_details
  }

  # Logical summaries (unchanged)
  details_list$group_completeness <- !any(is.na(group_orig))
  details_list$time_completeness <- !any(is.na(time_orig))
  details_list$no_duplicates <- !has_duplicates
  details_list$balance <- is_balanced
  details_list$time_sequence <- !has_irregular_time_sequence
  details_list$group_intervals <- !has_irregular_intervals

  # Create the output data.frame with test results
  test_results <- exploration_results

  # Add overall status as first row
  overall_row <- data.frame(
    test = "Overall",
    status = overall_status,
    message = ifelse(
      overall_status == "PASS",
      "Panel structure is valid",
      "Panel structure has issues"
    ),
    stringsAsFactors = FALSE
  )

  # Rename columns and combine
  names(test_results) <- c("test", "status", "message")
  final_results <- rbind(overall_row, test_results)

  # Build metadata
  call <- match.call()
  metadata <- list(
    function_name = as.character(call[[1]]),
    group = group,
    time = time
  )

  # Set attributes in desired order
  attr(final_results, "metadata") <- metadata
  attr(final_results, "details") <- details_list

  # Set class
  class(final_results) <- c("panel_description", "data.frame")

  return(final_results)
}
