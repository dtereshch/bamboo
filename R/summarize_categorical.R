#' Categorical Variable Summary for Panel Data
#'
#' This function performs one-way tabulations and decomposes counts into
#' between and within components for categorical variables in panel data.
#'
#' @param data A data.frame containing panel data.
#' @param selection A character vector specifying which categorical variables to analyze.
#'   If not specified, all factor variables in the data.frame will be used.
#' @param group A character string specifying the name of the entity/group variable.
#'   Required for decomposition.
#' @param time A character string specifying the name of the time variable.
#'   Required for correct ordering within groups.
#' @param digits An integer indicating the number of decimal places to round percentages.
#'   Default = 3.
#'
#' @return A data.frame with categorical panel data summary statistics.
#'
#' @details
#' Returns a data.frame with the following columns:
#' \describe{
#'   \item{\code{variable}}{The name of the analyzed variable}
#'   \item{\code{category}}{The category level of the variable}
#'   \item{\code{n_overall}}{Overall frequency (person-time observations)}
#'   \item{\code{share_overall}}{Overall percentage (n_overall / total_obs * 100)}
#'   \item{\code{n_between}}{Between-group frequency (number of groups ever having this category)}
#'   \item{\code{share_between}}{Between-group percentage (n_between / total_groups * 100)}
#'   \item{\code{share_within}}{Within-group percentage (average percent of time groups have this category)}
#' }
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{group_var}}{The grouping variable name}
#'   \item{\code{time_var}}{The time variable name}
#'   \item{\code{n_groups}}{Number of unique groups}
#'   \item{\code{digits}}{Number of decimal places used for rounding}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttab` command.
#'
#' @seealso
#' [summarize_panel()], [summarize_transition()], [summarize_data()]
#'
#' @examples
#' # Assuming 'production' dataset has factor variables
#' data(production)
#'
#' # Basic usage with statistics for all factor variables
#' summarize_categorical(production, group = "firm", time = "year")
#'
#' # Show statistics for a single categorical variable
#' summarize_categorical(production, selection = "industry",
#'                       group = "firm", time = "year")
#'
#' # Show statistics for multiple categorical variables
#' summarize_categorical(production, selection = c("industry", "status"),
#'                       group = "firm", time = "year")
#'
#' # Show statistics with two digits rounding
#' summarize_categorical(production, group = "firm", time = "year", digits = 2)
#'
#' @export
summarize_categorical <- function(
  data,
  selection = NULL,
  group,
  time,
  digits = 3
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
    stop("'group' must be a single character string")
  }

  if (!is.character(time) || length(time) != 1) {
    stop("'time' must be a single character string")
  }

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
  }

  if (!time %in% names(data)) {
    stop('variable "', time, '" not found in data')
  }

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
    stop(
      "'digits' must be a single non-negative integer, not ",
      class(digits)[1]
    )
  }

  # Validate digits parameter
  if (digits != round(digits)) {
    stop("'digits' must be an integer")
  }

  # If selection is not specified, use all factor variables
  if (is.null(selection)) {
    # Check for factor variables
    is_factor <- vapply(
      data,
      function(x) is.factor(x) || is.character(x),
      FUN.VALUE = logical(1)
    )

    # Exclude group and time variables from selection
    is_factor[group] <- FALSE
    is_factor[time] <- FALSE

    selection <- names(data)[is_factor]

    if (length(selection) == 0) {
      stop("no factor or character variables found in the dataset")
    }

    message(
      "Analyzing all factor/character variables: ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection
  missing_vars <- selection[!selection %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if specified columns are factor or character
  for (var in selection) {
    if (!(is.factor(data[[var]]) || is.character(data[[var]]))) {
      stop(
        "variable '",
        var,
        "' is not a factor or character variable. ",
        "Class: ",
        class(data[[var]])[1]
      )
    }
  }

  # Convert group and time to character for consistent handling
  data[[group]] <- as.character(data[[group]])
  data[[time]] <- as.character(data[[time]])

  # Order data by group and time
  data <- data[order(data[[group]], data[[time]]), ]

  # Get total number of groups
  n_groups <- length(unique(data[[group]]))

  # Helper function to calculate categorical statistics for one variable
  summarize_categorical_1 <- function(df, varname, grp, tme, digits_val) {
    # Remove rows with NA in the variable, group, or time
    complete_cases <- complete.cases(df[[varname]], df[[grp]], df[[tme]])
    df_clean <- df[complete_cases, , drop = FALSE]

    if (nrow(df_clean) == 0) {
      return(data.frame(
        variable = character(),
        category = character(),
        n_overall = integer(),
        share_overall = numeric(),
        n_between = integer(),
        share_between = numeric(),
        share_within = numeric(),
        stringsAsFactors = FALSE
      ))
    }

    # Ensure the variable is treated as factor
    if (!is.factor(df_clean[[varname]])) {
      df_clean[[varname]] <- factor(df_clean[[varname]])
    }

    # Get all categories
    categories <- levels(df_clean[[varname]])

    # Calculate overall statistics (person-time)
    overall_counts <- table(df_clean[[varname]])
    total_obs <- sum(overall_counts)

    # Calculate between statistics (groups ever having category)
    # For each group, check which categories it ever had
    group_categories <- tapply(
      df_clean[[varname]],
      df_clean[[grp]],
      function(x) unique(as.character(x))
    )

    # Count groups that ever had each category
    between_counts <- sapply(categories, function(cat) {
      sum(sapply(group_categories, function(gcats) cat %in% gcats))
    })

    # Calculate within statistics (average percent of time groups have category)
    # For each group, calculate the percentage of observations for each category
    group_percentages <- tapply(
      df_clean[[varname]],
      df_clean[[grp]],
      function(x) {
        tab <- table(x)
        tab / sum(tab) * 100
      }
    )

    # Average these percentages across groups for each category
    # Weighted by the number of groups that ever had the category
    within_shares <- sapply(categories, function(cat) {
      percents <- sapply(group_percentages, function(gp) {
        if (cat %in% names(gp)) gp[cat] else 0
      })
      # Only average over groups that ever had this category
      relevant_groups <- which(between_counts[cat] > 0)
      if (length(relevant_groups) == 0) {
        return(0)
      }
      mean(percents[relevant_groups])
    })

    # Calculate shares (percentages)
    share_overall <- overall_counts / total_obs * 100
    share_between <- between_counts / n_groups * 100

    # Apply rounding if digits is specified
    round_if_needed <- function(value) {
      if (is.numeric(value) && !is.na(value)) {
        round(value, digits_val)
      } else {
        value
      }
    }

    # Prepare result data frame
    result <- data.frame(
      variable = rep(varname, length(categories)),
      category = categories,
      n_overall = as.integer(overall_counts),
      share_overall = round_if_needed(as.numeric(share_overall)),
      n_between = as.integer(between_counts),
      share_between = round_if_needed(as.numeric(share_between)),
      share_within = round_if_needed(as.numeric(within_shares)),
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # Calculate statistics for each variable
  results <- lapply(selection, function(varname) {
    summarize_categorical_1(data, varname, group, time, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add data source information as attribute
  attr(result_df, "group_var") <- group
  attr(result_df, "time_var") <- time
  attr(result_df, "n_groups") <- n_groups
  attr(result_df, "digits") <- digits

  return(result_df)
}
