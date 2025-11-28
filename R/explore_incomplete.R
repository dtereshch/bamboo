#' Explore entities with incomplete observations (missing values)
#'
#' This function provides a detailed table of entities with incomplete observations,
#' showing the number of variables with missing values and total missing observations
#' for each entity. It works with both regular data frames and plm pdata.frame objects.
#'
#' @param data The data frame or pdata.frame to analyze
#' @param group Character string specifying the entities' identifier (column name)
#' @param time Character string specifying the time identifier (optional, for checking panel balance)
#' @param detailed Logical indicating whether to include detailed missing counts
#'        for each variable (TRUE) or just summary counts (FALSE). Default is FALSE.
#'
#' @return A data frame containing entities with missing values, including:
#' \itemize{
#'   \item The group identifier
#'   \item Number of variables with at least one missing value for that entity
#'   \item Total number of missing observations for that entity
#'   \item (If detailed = TRUE) Additional columns with NA counts for each variable
#' }
#' The data frame is arranged in descending order by number of variables with
#' missing values, then by total missing observations. If no incomplete entities
#' are found, returns a character string message.
#'
#' @examples
#' # Using regular data frame - summary view
#' data(production)
#' explore_incomplete(production, group = "firm")
#'
#' # Detailed view with variable-level NA counts
#' explore_incomplete(production, group = "firm", detailed = TRUE)
#'
#' # Check for panel balance
#' explore_incomplete(production, group = "firm", time = "year")
#'
#' # With plm pdata.frame
#' library(plm)
#' pdf <- pdata.frame(production, index = c("firm", "year"))
#' explore_incomplete(pdf, group = "firm", time = "year")
#'
#' @seealso \code{\link{find_incomplete}} for a simpler function that returns only
#' the identifiers of incomplete entities
#'
#' @export
explore_incomplete <- function(data, group, time = NULL, detailed = FALSE) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or pdata.frame")
  }

  if (missing(group) || is.null(group)) {
    stop("Argument 'group' is required")
  }

  # Validate group is a character string
  if (!is.character(group) || length(group) != 1) {
    stop("'group' must be a single character string specifying the column name")
  }

  # Validate detailed parameter
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("Argument 'detailed' must be a single logical value (TRUE or FALSE)")
  }

  # Convert to regular data frame to avoid pdata.frame/pseries issues
  # This ensures consistent behavior regardless of input data type
  data_df <- as.data.frame(data)

  # Validate group column exists
  if (!group %in% names(data_df)) {
    stop(sprintf("Group variable '%s' not found in data", group))
  }

  group_var_orig <- data_df[[group]]

  # Check if group variable has missing values itself
  if (any(is.na(group_var_orig))) {
    warning(
      "Group variable contains missing values. These will be included in the results."
    )
  }

  # Convert group variable to appropriate type for consistent handling
  group_var <- group_var_orig
  if (is.factor(group_var)) {
    group_var <- as.character(group_var)
  }

  # Check if time variable is provided for unbalanced panel check
  if (!is.null(time)) {
    if (!is.character(time) || length(time) != 1) {
      stop(
        "'time' must be a single character string specifying the column name"
      )
    }
    if (!time %in% names(data_df)) {
      stop(sprintf("Time variable '%s' not found in data", time))
    }

    time_var_orig <- data_df[[time]]

    # Convert time variable to appropriate type for consistent handling
    time_var <- time_var_orig
    if (is.factor(time_var)) {
      time_var <- as.character(time_var)
    }

    # Check for unbalanced panel
    time_counts <- tapply(time_var, group_var, function(x) length(unique(x)))
    if (length(unique(time_counts)) > 1) {
      warning("The panel is unbalanced.")
    }
  }

  # Get unique groups
  unique_groups <- unique(group_var)

  # Get variable names excluding group and time variables
  exclude_vars <- group
  if (!is.null(time)) {
    exclude_vars <- c(exclude_vars, time)
  }
  vars <- setdiff(names(data_df), exclude_vars)

  if (length(vars) == 0) {
    stop("No variables to analyze (only group and time variables found)")
  }

  # Initialize base results
  result <- data.frame(
    group = unique_groups,
    n_vars_with_na = 0,
    total_na = 0,
    stringsAsFactors = FALSE
  )
  names(result)[1] <- group

  # If detailed = TRUE, pre-allocate columns for each variable
  if (detailed) {
    for (var in vars) {
      result[[var]] <- 0
    }
  }

  # For each group, calculate missing statistics
  for (i in seq_along(unique_groups)) {
    current_group <- unique_groups[i]

    # Handle NA values in group variable
    if (is.na(current_group)) {
      group_indices <- is.na(group_var)
    } else {
      group_indices <- group_var == current_group
    }

    group_data <- data_df[group_indices, vars, drop = FALSE]

    # Count variables with at least one NA
    vars_with_na <- sum(vapply(
      group_data,
      function(x) any(is.na(x)),
      logical(1)
    ))

    # Count total number of NA observations
    total_na <- sum(vapply(group_data, function(x) sum(is.na(x)), numeric(1)))

    result$n_vars_with_na[i] <- vars_with_na
    result$total_na[i] <- total_na

    # If detailed = TRUE, add NA counts for each variable
    if (detailed) {
      for (var in vars) {
        result[[var]][i] <- sum(is.na(group_data[[var]]))
      }
    }
  }

  # Filter groups with any missing variables and arrange
  result <- result[result$n_vars_with_na > 0, ]

  # Check if there are any incomplete groups
  if (nrow(result) == 0) {
    return("There are no incomplete groups/entities in the data.")
  }

  # Sort by primary and secondary criteria
  result <- result[order(-result$n_vars_with_na, -result$total_na), ]

  # Reset row names
  rownames(result) <- NULL

  # Convert back to original type if possible
  original_type <- class(group_var_orig)
  if ("numeric" %in% original_type || "integer" %in% original_type) {
    # Handle numeric conversion carefully to avoid warnings
    numeric_groups <- suppressWarnings(as.numeric(result[[group]]))
    if (!any(is.na(numeric_groups))) {
      result[[group]] <- numeric_groups
    }
  } else if ("factor" %in% original_type) {
    result[[group]] <- factor(
      result[[group]],
      levels = levels(group_var_orig)
    )
  }

  return(result)
}
