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
#' @param digits An integer indicating the number of decimal places to round shares.
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
#'   \item{\code{share_overall}}{Overall share (n_overall / total_obs)}
#'   \item{\code{n_between}}{Between-group frequency (number of groups ever having this category)}
#'   \item{\code{share_between}}{Between-group share (n_between / total_groups)}
#'   \item{\code{share_within}}{Within-group share (average share of time groups have this category)}
#' }
#' All shares are proportions ranging from 0 to 1.
#'
#' The data.frame has additional attributes:
#' \describe{
#'   \item{\code{group_var}}{The grouping variable name}
#'   \item{\code{n_groups}}{Number of unique groups}
#'   \item{\code{digits}}{Number of decimal places used for rounding}
#' }
#'
#' @references
#' For Stata users: This corresponds to the `xttab` command, but returns shares (proportions) instead of percentages.
#'
#' @seealso
#' [summarize_panel()], [summarize_transition()], [summarize_data()]
#'
#' @examples
#' # Assuming 'production' dataset has factor variables
#' data(production)
#'
#' # Basic usage with statistics for all factor variables
#' summarize_categorical(production, group = "firm")
#'
#' # Show statistics for a single categorical variable
#' summarize_categorical(production, selection = "industry", group = "firm")
#'
#' # Show statistics for multiple categorical variables
#' summarize_categorical(production, selection = c("industry", "status"), group = "firm")
#'
#' # Show statistics with two digits rounding
#' summarize_categorical(production, group = "firm", digits = 2)
#'
#' @export
summarize_categorical <- function(
  data,
  selection = NULL,
  group,
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

  if (!group %in% names(data)) {
    stop('variable "', group, '" not found in data')
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

    # Exclude group variable from selection
    is_factor[group] <- FALSE

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

  # Convert group to character for consistent handling
  data[[group]] <- as.character(data[[group]])

  # Get total number of groups
  n_groups <- length(unique(data[[group]]))

  # Helper function to calculate categorical statistics for one variable
  summarize_categorical_1 <- function(df, varname, grp, digits_val) {
    # Remove rows with NA in the variable or group
    complete_cases <- complete.cases(df[[varname]], df[[grp]])
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
    group_data <- split(df_clean[[varname]], df_clean[[grp]])

    # Count groups that ever had each category
    between_counts <- sapply(categories, function(cat) {
      sum(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))
    })

    # Calculate within shares (proportions)
    # For each category, calculate average share across groups that ever had it
    within_shares <- sapply(categories, function(cat) {
      # Get groups that ever had this category
      groups_with_cat <- which(sapply(group_data, function(gdata) {
        any(as.character(gdata) == cat)
      }))

      if (length(groups_with_cat) == 0) {
        return(0)
      }

      # For each group that ever had the category, calculate what share
      # of its observations have this category
      group_shares <- sapply(groups_with_cat, function(i) {
        gdata <- group_data[[i]]
        sum(as.character(gdata) == cat) / length(gdata)
      })

      # Average these shares
      mean(group_shares)
    })

    # Calculate shares (proportions, not percentages)
    share_overall <- as.numeric(overall_counts / total_obs)
    share_between <- as.numeric(between_counts / n_groups)

    # Apply rounding if digits is specified
    round_if_needed <- function(value) {
      if (is.numeric(value) && !any(is.na(value))) {
        round(value, digits_val)
      } else {
        value
      }
    }

    # Round the share columns
    share_overall_rounded <- round_if_needed(share_overall)
    share_between_rounded <- round_if_needed(share_between)
    within_shares_rounded <- round_if_needed(within_shares)

    # Prepare result data frame
    result <- data.frame(
      variable = rep(varname, length(categories)),
      category = categories,
      n_overall = as.integer(overall_counts),
      share_overall = share_overall_rounded,
      n_between = as.integer(between_counts),
      share_between = share_between_rounded,
      share_within = within_shares_rounded,
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # Calculate statistics for each variable
  results <- lapply(selection, function(varname) {
    summarize_categorical_1(data, varname, group, digits)
  })

  # Combine all results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Add data source information as attribute
  attr(result_df, "group_var") <- group
  attr(result_df, "n_groups") <- n_groups
  attr(result_df, "digits") <- digits

  return(result_df)
}
