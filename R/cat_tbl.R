#' @title Summarize a categorical variable
#'
#' @description `cat_group_tbl()` presents frequency counts and percentages 
#' (count, percent) for nominal or categorical variables. Missing data can 
#' be excluded from the calculations.
#'
#' @param data A data frame.
#' @param var A character string of the name of a variable in `data` containing 
#' categorical data.
#' @param na.rm A logical value indicating whether missing values should be 
#' removed before calculations. Default is `FALSE`.
#' @param only A character string, or vector of character strings, of the 
#' types of summary data to return. Default is `NULL`, which returns both 
#' counts and percentages. To return only counts or percentages, use `count` 
#' or `percent`, respectively.
#' @param ignore An optional vector that contains values to exclude from the data. 
#' Default is `NULL`, which includes all present values.
#'
#' @returns A tibble displaying the relative frequency counts and/or percentages 
#' of `row_var`.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' cat_tbl(data = nlsy, var = "gender")
#'
#' cat_tbl(data = nlsy, var = "race", only = "count")
#'
#' cat_tbl(data = nlsy,
#'         var = "race",
#'         ignore = "Hispanic",
#'         only = "percent",
#'         na.rm = TRUE)
#'
#' @export
cat_tbl <- function(data, var, na.rm = FALSE, only = NULL, ignore = NULL) {

  # Check 'data' is a data frame with at least one row/column
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.")
  }

  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.")
  }

  # Check 'var' is a character vector of length one and exists in 'data'
  if (!is.character(var) || length(var) != 1) {
    stop("Invalid 'var' argument. 'var' must be a character vector of length one.")
  }

  if (!(var %in% colnames(data))) {
    stop("The 'var' argument is not a column in 'data'.")
  }

  # Check 'na.rm' is logical and length one
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("Invalid 'na.rm' argument. 'na.rm' must be a logical vector of length one.")
  }

  # Check 'only'
  if (is.null(only)) {
    only <- only_type("cat")
  } else {
    only <- tolower(trimws(only))
  }

  if (!(all(only %in% only_type("cat"))) || length(only) == 0){
    stop("Invalid 'only' argument. 'only' must be a character vector of length at least one.")
  }

  # Remove values that are set to 'ignore'
  if (!is.null(ignore) && is.vector(ignore) && length(ignore) > 0) {
    
    data <-
      data |>
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(var) , 
                                  .fns = ~ ifelse(. %in% ignore, NA, .)))
  }

  # Remove rows with NAs if requested
  if (na.rm) {
    data <- stats::na.omit(data[var])
  }

  # Create table
  cat_tabl <-
    data |>
    dplyr::group_by(.data[[var]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(percent = .data[["count"]] / sum(.data[["count"]]))

  # Remove unrequested 'only' columns
  cat_tabl <- drop_only_cols(data = cat_tabl,
                             only = only,
                             only_type = only_type("cat"))

  cat_tabl
}


