#' @title Check a named vector
#'
#' @description This function assesses whether named lists and vectors have 
#' invalid values (like `NULL` or `NA`), invalid names (such as missing or 
#' empty names), confirms that the count of valid names matches the count of 
#' provided values, and verifies that the valid names obtained from the named 
#' object align with the supplied names. If any checks fail, the `default` 
#' value is returned.
#'
#' @param x A named vector.
#' @param names A character vector or list of character vectors of length one 
#' specifying the names to be matched.
#' @param default Default value to return
#'
#' @return Either the original object, `x`, or the `default` value.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#'
#' # returns NULL
#' check_named_vctr(x = c(one = 1, two = 2, 3), 
#'                  names = c("one", "two", "three"),
#'                  default = NULL)
#'                  
#' # returns x
#' check_named_vctr(x = list(one = 1, two = 2, three = 3), 
#'                  names = list("one", "two", "three"),
#'                  default = NULL)  
#' 
#' # also returns x
#' check_named_vctr(x = c(baako = 1, mmienu = 2, mmiensa = 3), 
#'                  names = list("baako", "mmienu", "mmiensa"),
#'                  default = NULL)              
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
  validated_names <- check_name_arg(names) 
  
  if (is.null(validated_names)) {
    return(default)
  }
  
  if (check_invalid_list_values(x)) {
    return(default)
  }

  if (length(x) != length(validated_names)) {
    return(default)
  }

  if (!all(names(x) %in% validated_names)) {
    return(default)
  }

  return(x)
}
#' @export
check_named_vctr.logical <- function(x, names, default) {
  validated_names <- check_name_arg(names) 
  
  if (is.null(validated_names)) {
    return(default)
  }
  
  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(validated_names)) {
    return(default)
  }

  if (!all(names(x) %in% validated_names)) {
    return(default)
  }

  return(x)
}

#' @export
check_named_vctr.character <- function(x, names, default) {
  validated_names <- check_name_arg(names) 
  
  if (is.null(validated_names)) {
    return(default)
  }
  
  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(validated_names)) {
    return(default)
  }

  if (!all(names(x) %in% validated_names)) {
    return(default)
  }

  return(x)
}
#'
#' @export
check_named_vctr.numeric <- function(x, names, default) {
  validated_names <- check_name_arg(names) 
  
  if (is.null(validated_names)) {
    return(default)
  }
  
  if (check_invalid_values(x)) {
    return(default)
  }

  if (length(x) != length(validated_names)) {
    return(default)
  }

  if (!all(names(x) %in% validated_names)) {
    return(default)
  }

  return(x)
}
#'
#' @keywords internal
check_invalid_values <- function(x) {

  has_invalid_names <- any(trimws(names(x)) == "")
  has_invalid_values <- any(trimws(unname(x)) == "") || any(unname(x) ==   "") || any(is.na(unname(x)))

  return(has_invalid_names || has_invalid_values)
}
#'
#' @keywords internal
check_invalid_list_values <- function(x) {

  has_invalid_names <- is.null(names(x)) || any(trimws(names(x)) == "")
  has_invalid_values <- 
    is.null(unname(x)) || any(trimws(unname(x)) == "") || 
    any(is.na(unname(x))) || any(sapply(unname(x), is.null))

  return(has_invalid_names || has_invalid_values)
}
#' 
#' @keywords internal
check_name_arg <- function(names) { 
  if (!is.character(names) && (!is.list(names) || !all(vapply(names, function(n) is.character(n) && length(n) == 1, logical(1))))) { 
    return(NULL) 
  } 
  
  if (is.list(names)) unlist(names) else names 
}

