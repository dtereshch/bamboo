#' Convert Input to Data Frame
#'
#' @description Internal helper to ensure input is a data frame.
#' Attempts conversion with `as.data.frame()` if needed, providing
#' clear errors on failure.
#'
#' @param data Input object to convert
#' @param arg_name Argument name for error messages (default: "data")
#' @return A data frame
#'
#' @details Uses `stringsAsFactors = FALSE` for conversion. Returns
#' data frames unchanged. Handles matrices, lists, and other
#' `as.data.frame()`-compatible objects.
#'
#' @keywords internal
#' @noRd
.check_and_convert_data_minimal <- function(data, arg_name = "data") {
  # Null check
  if (is.null(data)) {
    stop("'", arg_name, "' cannot be NULL", call. = FALSE)
  }

  # Already a data.frame (includes tibbles)
  if (is.data.frame(data)) {
    return(data)
  }

  # Try conversion
  converted <- try(as.data.frame(data, stringsAsFactors = FALSE), silent = TRUE)

  if (inherits(converted, "try-error")) {
    stop(
      "'",
      arg_name,
      "' must be a data.frame or convertible to data.frame.\n",
      "  Class: ",
      paste(class(data), collapse = ", "),
      "\n",
      "  Error: ",
      as.character(converted),
      call. = FALSE
    )
  }

  # Double-check
  if (!is.data.frame(converted)) {
    stop(
      "Conversion of '",
      arg_name,
      "' failed to produce a data.frame.\n",
      "  Got class: ",
      paste(class(converted), collapse = ", "),
      call. = FALSE
    )
  }

  return(converted)
}

#' Validate and Convert Input to Data Frame
#'
#' @description
#' More robust version that handles special data.frame subclasses that
#' may cause issues with standard data.frame operations.
#'
#' @param data Input data object
#' @param arg_name Argument name for error messages
#' @param convert_special_df Should special data.frame subclasses (like
#'   pdata.frame) be converted to plain data.frames? Default TRUE.
#' @param warn_special_df Warn when converting special data.frame subclasses?
#'   Default TRUE.
#'
#' @return A plain data.frame suitable for standard operations
#'
#' @keywords internal
#' @noRd
.check_and_convert_data_robust <- function(
  data,
  arg_name = "data",
  convert_special_df = TRUE,
  warn_special_df = TRUE
) {
  # Null check
  if (is.null(data)) {
    stop("'", arg_name, "' cannot be NULL", call. = FALSE)
  }

  # Check for problematic data.frame subclasses
  data_class <- class(data)

  # List of known problematic data.frame subclasses
  # These pass is.data.frame() but may break standard operations
  problematic_classes <- c(
    "pdata.frame", # plm package
    "tbl_df",
    "tbl", # tibble (usually fine, but listed for completeness)
    "grouped_df", # dplyr grouped data
    "rowwise_df", # dplyr rowwise data
    "data.table", # data.table (if not using data.table package)
    "sf" # simple features (spatial data)
  )

  # Check if any problematic classes are present
  is_problematic <- any(data_class %in% problematic_classes) ||
    any(grepl("^pdata\\.frame$", data_class)) ||
    any(grepl("^pdataframe$", data_class, ignore.case = TRUE))

  # If it's a data.frame (or subclass)
  if (is.data.frame(data)) {
    if (is_problematic && convert_special_df) {
      # Convert problematic subclass to plain data.frame
      converted <- try(
        as.data.frame(data, stringsAsFactors = FALSE),
        silent = TRUE
      )

      if (inherits(converted, "try-error")) {
        stop(
          "Failed to convert ",
          data_class[1],
          " '",
          arg_name,
          "' to regular data.frame.\n",
          "  Error: ",
          as.character(converted),
          call. = FALSE
        )
      }

      if (warn_special_df) {
        warning(
          "Converted ",
          data_class[1],
          " to regular data.frame. ",
          "Special features (index, groups, etc.) may be lost.",
          call. = FALSE,
          immediate. = TRUE
        )
      }

      return(converted)
    } else if (is_problematic && !convert_special_df) {
      # Don't convert, but warn about potential issues
      if (warn_special_df) {
        warning(
          "Using ",
          data_class[1],
          " object '",
          arg_name,
          "'. ",
          "Some operations may not work as expected with this data type.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
      return(data)
    } else {
      # Regular data.frame or non-problematic subclass
      return(data)
    }
  }

  # Not a data.frame at all - try conversion
  converted <- try(as.data.frame(data, stringsAsFactors = FALSE), silent = TRUE)

  if (inherits(converted, "try-error")) {
    stop(
      "'",
      arg_name,
      "' must be a data.frame or convertible to data.frame.\n",
      "  Class: ",
      paste(data_class, collapse = ", "),
      "\n",
      "  Error: ",
      as.character(converted),
      call. = FALSE
    )
  }

  # Double-check conversion produced data.frame
  if (!is.data.frame(converted)) {
    stop(
      "Conversion of '",
      arg_name,
      "' failed to produce a data.frame.\n",
      "  Got class: ",
      paste(class(converted), collapse = ", "),
      call. = FALSE
    )
  }

  return(converted)
}
