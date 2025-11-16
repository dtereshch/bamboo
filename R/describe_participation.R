#' Describe panel data participation patterns
#'
#' This function replicates Stata's xtdes command, showing the participation patterns
#' of entities in panel data over time.
#'
#' @param data A data frame with panel data structure
#' @param group Character string specifying the entity/group variable name
#' @param time Character string specifying the time variable name
#' @param detailed Logical indicating whether to return detailed patterns (default: TRUE)
#'
#' @return A data frame with participation patterns containing:
#' \itemize{
#'   \item Pattern: Pattern identifier (1, 2, 3, ...)
#'   \item Columns for each time period showing participation (1 = present, 0 = missing)
#'   \item Count: Number of entities with this pattern
#'   \item Share: Proportion of entities with this pattern
#'   \item Cumul.: Cumulative proportion of entities
#' }
#'
#' @examples
#' # Load the production dataset
#' data(production)
#'
#' # Basic usage with all patterns shown
#' result <- describe_participation(production, "firm", "year")
#' print(result)
#'
#' # Show simplified version without time period columns
#' result_simple <- describe_participation(production, "firm", "year", detailed = FALSE)
#' print(result_simple)
#'
#' # Example with unbalanced panel (introducing NAs)
#' production_unbalanced <- production
#' # Remove some observations to create unbalanced panel
#' production_unbalanced <- production_unbalanced[-c(5, 15, 25), ]
#' result_unbalanced <- describe_participation(production_unbalanced, "firm", "year")
#' print(result_unbalanced)
#'
#' @export
describe_participation <- function(
  data,
  group = NULL,
  time = NULL,
  detailed = TRUE
) {
  # Input validation
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data frame")
  }

  # Check if group and time are specified for regular data frames
  if (is.null(group) || is.null(time)) {
    stop(
      "Arguments 'group' and 'time' must be specified for regular data frames"
    )
  }

  # Validate that group and time variables exist in data
  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data")
  }

  if (!time %in% names(data)) {
    stop("Time variable '", time, "' not found in data")
  }

  # Convert group and time to character to handle different classes
  group_vec <- as.character(data[[group]])
  time_vec <- as.character(data[[time]])

  # Create unique combinations of group and time
  unique_combinations <- unique(data.frame(group = group_vec, time = time_vec))

  # Get all unique time periods and sort them
  all_times <- sort(unique(time_vec))

  # Create participation matrix
  participation <- table(unique_combinations$group, unique_combinations$time)

  # Convert to binary matrix (1 = present, 0 = missing)
  participation_binary <- ifelse(participation > 0, 1, 0)

  # Convert to data frame for pattern analysis
  participation_df <- as.data.frame(participation_binary)
  participation_df$group <- rownames(participation_df)

  # Ensure all time periods are present as columns
  for (t in all_times) {
    if (!t %in% names(participation_df)) {
      participation_df[[t]] <- 0
    }
  }

  # Reorder columns to have time periods in order
  time_cols <- as.character(sort(all_times))
  participation_df <- participation_df[c("group", time_cols)]

  # Count patterns
  pattern_cols <- setdiff(names(participation_df), "group")
  pattern_strings <- apply(participation_df[pattern_cols], 1, function(x) {
    paste(x, collapse = "")
  })

  pattern_counts <- table(pattern_strings)

  # Create result data frame
  result <- data.frame(
    Pattern = seq_along(pattern_counts),
    stringsAsFactors = FALSE
  )

  # Add time period columns
  for (i in seq_along(time_cols)) {
    result[[time_cols[i]]] <- as.numeric(substr(names(pattern_counts), i, i))
  }

  # Add count, share and cumulative columns with rounding
  result$Count <- as.numeric(pattern_counts)
  result$Share <- round(result$Count / sum(result$Count), 2)
  result$Cumul. <- round(cumsum(result$Share), 2)

  # Sort by count (descending)
  result <- result[order(-result$Count), ]
  result$Pattern <- seq_len(nrow(result))
  rownames(result) <- NULL

  if (!detailed) {
    # Return simplified version without individual time period columns
    simplified_result <- data.frame(
      Pattern = result$Pattern,
      Count = result$Count,
      Share = result$Share,
      Cumul. = result$Cumul.
    )
    return(simplified_result)
  }

  return(result)
}
