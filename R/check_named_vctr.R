#' @title Check a named vector
#'
#' @description This experimental function assesses whether named lists and vectors have
#' invalid values (like `NULL` or `NA`), invalid names (such as missing or empty names),
#' confirms that the count of valid names matches the count of provided values, and verifies
#' that the valid names obtained from the named object align with the supplied names.
#'
#' @param x A named vector.
#' @param names A character vector specifying the names to be matched.
#' @param default Default value to return
#'
#' @return Either the original object or some default value.
#'
#'
#' @export
check_named_vctr <- function(x, names, default) {
UseMethod("check_named_vctr")
}
#'
#' @export
check_named_vctr.default <- function(x, names, default) {
    return(default)
  }
#'
#' @export
check_named_vctr.list <- function(x, names, default) {

  if (check_invalid_list_values(x)) {
    return(default)
  }

  if (length(x) != length(names)) {
    return(default)
  }

  if (!all(names(x) %in% names)) {
    return(default)
  }

  return(x)
}
#' @export
check_named_vctr.logical <- function(x, names, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(names)) {
    return(default)
  }

  if (!all(names(x) %in% names)) {
    return(default)
  }

  return(x)
}

#' @export
check_named_vctr.character <- function(x, names, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(names)) {
    return(default)
  }

  if (!all(names(x) %in% names)) {
    return(default)
  }

  return(x)
}
#'
#' @export
check_named_vctr.numeric <- function(x, names, default) {

  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(names)) {
    return(default)
  }

  if (!all(names(x) %in% names)) {
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

