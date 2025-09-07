#' @title Check a named vector
#'
#' @description This experimental function assesses whether named lists and vectors have
#' invalid values (like `NULL` or `NA`), invalid names (such as missing or empty names),
#' confirms that the count of valid names matches the count of provided values, and verifies
#' that the valid names obtained from the named object align with the supplied names.
#'
#' @param x A named list or vector.
#' @param cols A character vector containing column names or variable stems to match.
#' @param default Default value to return
#'
#' @return Either the original object or some default value.
#'
#'
#' @export
check_named_obj <- function(x, cols, default) {
UseMethod("check_named_obj")
}
#'
#' @export
check_named_obj.default <- function(x, cols, default) {
    return(default)
  }
#'
#' @export
check_named_obj.list <- function(x, cols, default) {

  if (check_invalid_list_values(x)) {
    return(default)
  }

  if (length(x) != length(cols)) {
    return(default)
  }

  if (!all(names(x) %in% cols)) {
    return(default)
  }

  return(x)
}
#' @export
check_named_obj.logical <- function(x, cols, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(cols)) {
    return(default)
  }

  if (!all(names(x) %in% cols)) {
    return(default)
  }

  return(x)
}

#' @export
check_named_obj.character <- function(x, cols, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(cols)) {
    return(default)
  }

  if (!all(names(x) %in% cols)) {
    return(default)
  }

  return(x)
}
#'
#' @export
check_named_obj.numeric <- function(x, cols, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(cols)) {
    return(default)
  }

  if (!all(names(x) %in% cols)) {
    return(default)
  }

  return(x)
}
#'
#' @keywords internal
check_invalid_values <- function(x) {

  values <- unname(x)

  has_invalid_names <- any(trimws(names(x)) == "")
  has_invalid_values <- any(trimws(values) == "") || any(values ==   "") || any(is.na(values))

  return(has_invalid_names || has_invalid_values)
}
#'
#' @keywords internal
check_invalid_list_values <- function(x) {

  has_invalid_names <- is.null(names(x)) || any(trimws(names(x)) == "")
  has_invalid_values <- is.null(unname(x)) || any(trimws(unname(x)) == "") || any(is.na(unname(x)))

  return(has_invalid_names || has_invalid_values)
}

