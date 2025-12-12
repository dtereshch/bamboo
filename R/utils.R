#' @title Convert Input to Data Frame
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
