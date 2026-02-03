#' Heterogeneity Visualization
#'
#' Creates visualizations of heterogeneity among groups. Handles multiple numeric
#' variables and multiple grouping variables, arranging them in a grid where
#' selection variables form rows and group variables form columns. For multi-plot
#' grids, variable names are displayed as row and column headers instead of
#' individual plot axis labels for cleaner presentation.
#'
#' @param data A data.frame containing the variables for analysis.
#' @param selection A character vector specifying the numeric variable(s) of interest.
#'        If NULL (default), all numeric variables in the dataset will be used.
#' @param group A character vector specifying the grouping variable(s).
#' @param colors A character vector of two colors: first for individual points,
#'        second for mean line and points. Default = c("#D55E00", "#0072B2").
#'
#' @return Invisibly returns a list with summary statistics. Creates plot(s) showing
#'         group heterogeneity.
#'
#' @details
#' When both `selection` and `group` contain multiple variables, plots are arranged
#' in a grid:
#' - Rows correspond to variables in `selection` (Y-axis variables)
#' - Columns correspond to variables in `group` (X-axis grouping variables)
#'
#' Plot labeling strategy:
#' - **Single plot**: Variable names are used as axis labels, with a legend in the plot
#' - **Multi-plot grid**:
#'     - Row variable names are displayed as y-axis labels on the left side
#'     - Column variable names are displayed as x-axis labels on the top
#'     - Individual plot axes show only tick marks and values, not labels
#'     - A single common horizontal legend is placed below the grid
#'
#' When only one variable is specified for either parameter, a single plot or
#' single row/column is created with appropriate axis labels.
#'
#' @seealso
#' [summarize_panel()], [plot_participation()]
#'
#' @examples
#' \dontrun{
#' data(production)
#'
#' # Plot labor by year (single plot with axis labels and legend)
#' plot_heterogeneity(production, selection = "labor", group = "year")
#'
#' # Plot capital by firm (single plot with axis labels and legend)
#' plot_heterogeneity(production, selection = "capital", group = "firm")
#'
#' # Plot multiple variables with single grouping variable
#' # Y-axis labels show variable names, x-axis shows group variable
#' # Single common legend below the plots
#' plot_heterogeneity(production,
#'                    selection = c("sales", "labor", "capital"),
#'                    group = "year")
#'
#' # Plot single variable with multiple grouping variables
#' # Single y-axis label, x-axis labels show different grouping approaches
#' # Single common legend below the plots
#' plot_heterogeneity(production,
#'                    selection = "sales",
#'                    group = c("firm", "year", "industry"))
#'
#' # Plot multiple variables with multiple grouping variables (grid)
#' # Row labels = selection variables, column labels = group variables
#' # No individual plot axis labels for cleaner grid
#' # Single common legend below the grid
#' plot_heterogeneity(production,
#'                    selection = c("sales", "labor"),
#'                    group = c("year", "industry"))
#'
#' # Use all numeric variables with default
#' plot_heterogeneity(production, group = "year")
#'
#' # Customize colors
#' plot_heterogeneity(production, selection = "sales", group = "year",
#'                    colors = c("gray", "black"))
#' }
#'
#' @export
plot_heterogeneity <- function(
  data,
  selection = NULL,
  group,
  colors = c("#D55E00", "#0072B2")
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

  if (!is.character(group)) {
    stop(
      "'group' must be a character string or vector of character strings, not ",
      class(group)[1]
    )
  }

  # Check for missing variables in data
  missing_vars <- setdiff(c(selection, group), names(data))
  if (length(missing_vars) > 0) {
    stop(
      "the following variables were not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (!is.character(colors) || length(colors) != 2) {
    stop(
      "'colors' must be a character vector of length 2, not ",
      class(colors)[1]
    )
  }

  data <- .check_and_convert_data_robust(data, arg_name = "data")

  if (nrow(data) == 0) {
    stop("'data' must have at least one row")
  }

  # If selection is NULL, use all numeric variables
  if (is.null(selection)) {
    # Identify numeric variables
    numeric_vars <- vapply(data, is.numeric, FUN.VALUE = logical(1))
    selection <- names(data)[numeric_vars]

    # Remove group variables from selection if they are numeric
    selection <- setdiff(selection, group)

    if (length(selection) == 0) {
      stop(
        "no numeric variables found in the dataset (excluding group variables)"
      )
    }

    message(
      "Analyzing all numeric variable(s): ",
      paste(selection, collapse = ", ")
    )
  }

  # Validate selection variables are numeric
  for (var in selection) {
    if (!is.numeric(data[[var]])) {
      stop(
        "variable '",
        var,
        "' must be numeric, not ",
        class(data[[var]])[1]
      )
    }
  }

  # Extract colors
  point_col <- colors[1]
  mean_col <- colors[2]

  # Set default parameter values
  point_alpha <- 0.6
  mean_lwd <- 2
  cex <- 1
  las <- 1
  plot <- TRUE

  # Function to create single plot
  create_single_plot <- function(
    data_sub,
    y_var_name,
    group_var_name,
    show_axes_labels = TRUE,
    show_legend = TRUE # New parameter to control legend display
  ) {
    y_var <- data_sub[[y_var_name]]
    x_var <- data_sub[[group_var_name]]

    # Check group variable type
    if (!is.factor(x_var) && !is.character(x_var) && !is.numeric(x_var)) {
      stop(
        "group variable '",
        group_var_name,
        "' must be a factor, character, or numeric variable, not ",
        class(x_var)[1]
      )
    }

    # Convert grouping variable to factor for consistent handling
    if (!is.factor(x_var)) {
      x_var <- as.factor(x_var)
    }

    # Create color with alpha
    point_col_rgb <- col2rgb(point_col) / 255
    point_col_alpha <- rgb(
      point_col_rgb[1],
      point_col_rgb[2],
      point_col_rgb[3],
      alpha = point_alpha
    )

    # Calculate group means
    group_means <- tapply(y_var, x_var, mean, na.rm = TRUE)

    # Create the plot - use variable names as labels only for single plots
    plot(
      NA,
      xlim = c(0.5, length(levels(x_var)) + 0.5),
      ylim = range(y_var, na.rm = TRUE),
      xlab = if (show_axes_labels) group_var_name else "",
      ylab = if (show_axes_labels) y_var_name else "",
      main = "",
      xaxt = "n",
      frame.plot = FALSE
    )

    # Add x-axis with labels
    axis(1, at = seq_along(levels(x_var)), labels = levels(x_var))

    # Add individual points with jitter
    x_jitter <- jitter(as.numeric(x_var), amount = 0.2)
    points(
      x_jitter,
      y_var,
      col = point_col_alpha,
      pch = 16,
      cex = 0.8 * cex
    )

    # Add mean line and points
    lines(
      seq_along(group_means),
      group_means,
      col = mean_col,
      lwd = mean_lwd,
      type = "o",
      pch = 18,
      cex = 1.5 * cex
    )

    # Add grid
    grid()

    # Add legend only if requested
    if (show_legend) {
      legend(
        "topright",
        legend = c("Individual observations", "Group means"),
        col = c(point_col, mean_col),
        pch = c(16, 18),
        lty = c(NA, 1),
        pt.cex = c(0.8, 1.5),
        bty = "n",
        cex = 0.8 * cex
      )
    }

    # Return summary statistics for this combination
    list(
      means = group_means,
      sd = tapply(y_var, x_var, sd, na.rm = TRUE),
      n = tapply(y_var, x_var, function(x) sum(!is.na(x)))
    )
  }

  # Initialize summary statistics list
  summary_stats <- list(
    overall_stats = list(),
    group_stats = list()
  )

  # Calculate overall summary statistics for each selection variable
  for (y_var_name in selection) {
    y_var <- data[[y_var_name]]
    summary_stats$overall_stats[[y_var_name]] <- list(
      mean = mean(y_var, na.rm = TRUE),
      sd = sd(y_var, na.rm = TRUE),
      n = sum(!is.na(y_var))
    )
  }

  if (plot) {
    # Set up plotting layout
    n_rows <- length(selection)
    n_cols <- length(group)
    is_multiplot <- n_rows > 1 || n_cols > 1

    # Save current par settings
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    if (is_multiplot) {
      # Calculate dynamic left margin based on the longest selection variable name
      # Get maximum character count among selection variable names
      max_char_count <- max(nchar(selection))

      # Calculate required margin:
      # - Base margin of 4 lines
      # - Add 0.3 lines per character beyond 8 characters
      # - Minimum of 4 lines, but scales with long variable names
      base_margin <- 4
      if (max_char_count > 8) {
        extra_chars <- max_char_count - 8
        required_left_margin_lines <- base_margin + (extra_chars * 0.3)
      } else {
        required_left_margin_lines <- base_margin
      }

      # Round up and ensure reasonable bounds
      required_left_margin_lines <- min(
        max(ceiling(required_left_margin_lines), 4),
        10
      )

      # Create layout with space for common legend
      layout_matrix <- matrix(
        1:(n_rows * n_cols),
        nrow = n_rows,
        ncol = n_cols,
        byrow = TRUE
      )

      # Add an extra row at the bottom for the legend
      layout_matrix <- rbind(layout_matrix, rep(n_rows * n_cols + 1, n_cols))

      # Set up the layout with dynamic left margin
      layout(
        layout_matrix,
        heights = c(rep(1, n_rows), 0.3), # Legend gets 0.3 relative height
        widths = rep(1, n_cols)
      )

      # Adjust margins for grid plots - use outer margins for the variable names
      # This is the key fix: Use outer margins for labels and keep inner margins small
      par(
        mar = c(2, 1, 3, 1) + 0.1, # Small inner margins
        oma = c(0, required_left_margin_lines, 0, 0), # Outer left margin for labels
        las = las
      )
    } else {
      # Standard margins for single plot
      par(
        mfrow = c(1, 1),
        mar = c(5, 4, 4, 2) + 0.1,
        oma = c(0, 0, 0, 0),
        las = las
      )
    }

    # Create plots in grid: rows = selection variables, columns = group variables
    for (i in seq_along(selection)) {
      y_var_name <- selection[i]

      for (j in seq_along(group)) {
        group_var_name <- group[j]

        # Determine if we should show axis labels and legend
        # Only show legend in individual plots for single plot case
        show_axes_labels <- !is_multiplot
        show_legend <- !is_multiplot # No legend in individual plots for multi-plot case

        # Create plot for this combination
        group_stats <- create_single_plot(
          data,
          y_var_name,
          group_var_name,
          show_axes_labels = show_axes_labels,
          show_legend = show_legend
        )

        # Store statistics
        if (!y_var_name %in% names(summary_stats$group_stats)) {
          summary_stats$group_stats[[y_var_name]] <- list()
        }
        summary_stats$group_stats[[y_var_name]][[group_var_name]] <- group_stats

        # Add variable labels for multi-plot grids
        if (is_multiplot) {
          # Add row labels (selection variables) on the left side in outer margin area
          if (j == 1) {
            # Calculate the exact position in the outer margin
            # Each plot gets 1/n_rows of the total height in the layout
            # We want to center the label in the middle of each row
            plot_position <- (n_rows - i + 0.5) / n_rows

            # Use outer=TRUE to place in outer margin area
            mtext(
              y_var_name,
              side = 2,
              line = required_left_margin_lines - 2, # Position within outer margin
              outer = TRUE, # Critical: place in outer margin, not inner
              cex = 0.9,
              font = 2,
              at = plot_position
            ) # Position vertically within the outer margin
          }

          # Add column labels (group variables) on the top in inner margin
          if (i == 1) {
            mtext(
              group_var_name,
              side = 3,
              line = 1,
              outer = FALSE, # Keep in inner margin for top labels
              cex = 0.9,
              font = 2
            )
          }
        }
      }
    }

    # Add common legend for multi-plot case
    if (is_multiplot) {
      # Switch to the legend panel with zero margins
      par(mar = c(0, 0, 0, 0))
      plot.new()

      # Create horizontal legend centered at the bottom
      legend(
        "center",
        legend = c("Individual observations", "Group means"),
        col = c(point_col, mean_col),
        pch = c(16, 18),
        lty = c(NA, 1),
        pt.cex = c(1.2, 1.8),
        bty = "n",
        horiz = TRUE,
        cex = 1.2,
        xpd = TRUE # Allow plotting outside the plot region
      )
    }
  } else {
    # Calculate statistics without plotting
    for (y_var_name in selection) {
      summary_stats$group_stats[[y_var_name]] <- list()

      for (group_var_name in group) {
        x_var <- data[[group_var_name]]
        y_var <- data[[y_var_name]]

        if (!is.factor(x_var)) {
          x_var <- as.factor(x_var)
        }

        summary_stats$group_stats[[y_var_name]][[group_var_name]] <- list(
          means = tapply(y_var, x_var, mean, na.rm = TRUE),
          sd = tapply(y_var, x_var, sd, na.rm = TRUE),
          n = tapply(y_var, x_var, function(x) sum(!is.na(x)))
        )
      }
    }
  }

  # Return summary statistics invisibly
  invisible(summary_stats)
}
