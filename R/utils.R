# Internal helper functions for the panel package

#' Round numeric values if needed
#' @param x A vector (typically numeric).
#' @param digits Number of decimal places.
#' @return Rounded vector if numeric and not all NA, otherwise unchanged.
#' @keywords internal
round_if_needed <- function(x, digits) {
  if (is.numeric(x) && !all(is.na(x))) round(x, digits) else x
}

#' Sort unique values preserving original type where sensible
#' @param x A vector.
#' @return Sorted unique values.
#' @keywords internal
sort_unique_preserve <- function(x) {
  ux <- unique(x)
  if (is.numeric(ux)) {
    sort(ux)
  } else if (inherits(ux, "Date") || inherits(ux, "POSIXt")) {
    sort(ux)
  } else if (is.factor(ux)) {
    sorted_char <- sort(as.character(ux))
    factor(sorted_char, levels = sorted_char, ordered = is.ordered(ux))
  } else {
    sort(ux)
  }
}
