#' @title Summarize a categorical variable by a grouping variable
#'
#' @description `cat_group_tbl()` presents frequency counts and percentages 
#' (count, percent) for nominal or categorical variables by some grouping variable. 
#' Relative frequencies and percentages of each level of the primary categorical 
#' variable (`row_var`) within each level of the grouping variable (`col_var`) can 
#' be returned. Missing data can be excluded for either variable from the calculations. 
#' By default, the table is returned in the long format.
#'
#' @param data A data frame.
#' @param row_var  A character string of the name of a column in `data` containing 
#' categorical data. This is the primary categorical variable. When pivoted to the 
#' `wider` format, the categories of this variable will appear in the rows of the 
#' table. 
#' @param col_var A character string of the name of a column in `data` containing 
#' categorical data. This is the primary categorical variable. When pivoted to the `wider`
#' format, the categories of this variable will appear in the rows of the table.
#' @param na.rm.row_var A logical value indicating whether missing values for `row_var`
#' should be removed before calculations. Default is `FALSE`.
#' @param na.rm.col_var A logical value indicating whether missing values for `col_var`
#' should be removed before calculations. Default is `FALSE`.
#' @param ignore A named character vector or list containing values to ignore from 
#' `row_var` and `col_var`.
#' @param pivot A character string specifying the format of the returned summary table.
#' The default is `longer`, which returns the data in long format. To return the data 
#' in wide format, use `wider`.
#' @param only A character string or vector of strings indicating the types of summary 
#' data to return. The default is `NULL`, which includes both counts and percentages. To 
#' return only one type, specify `count` or `percent`. Percentages are calculated column-
#' wise, grouped by `col_var`.
#'
#' @returns A tibble displaying relative frequency counts and/or percentages of `row_var`, 
#' grouped by `col_var.` When the output is in wider format, `columns` prefixed with `count_` 
#' and `percent_` contain the frequency and proportion, respectively, for each distinct 
#' response value of `row_var` within each level of `col_var.` 
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' cat_group_tbl(data = nlsy,
#'               row_var = "gender",
#'               col_var = "bthwht",
#'               pivot = "wider",
#'               only = "count")
#'
#' cat_group_tbl(data = nlsy,
#'               row_var = "birthord",
#'               col_var = "breastfed",
#'               pivot = "longer")
#'
#' @export
cat_group_tbl <- function(data,
                          row_var,
                          col_var,
                          na.rm.row_var = FALSE,
                          na.rm.col_var = FALSE,
                          only = NULL,
                          ignore = NULL,
                          pivot = "longer") {

  # Check 'data' is a data frame with at least one row/column
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.")
  }

  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.")
  }

  # Check 'row_var' is a character vector of length one and exists in 'data'
  if (!is.character(row_var) || length(row_var) != 1) {
    stop("Invalid 'row_var' argument. 'row_var' must be a character vector of length one.")
  }

  if (!(row_var %in% colnames(data))) {
    stop("The 'row_var' argument is not a column in 'data'.")
  }

  # Check 'col_var' is a character vector of length one and exists in 'data'
  if (!is.character(col_var) || length(col_var) != 1) {
    stop("Invalid 'col_var' argument. 'col_var' must be a character vector of length one.")
  }

  if (!(col_var %in% colnames(data))) {
    stop("The 'col_var' argument is not a column in 'data'.")
  }

  # Check 'na.rm.row_var' is a logical vector of length one
  if (!is.logical(na.rm.row_var) || length(na.rm.row_var) != 1) {
    stop("Invalid 'na.rm.row_var' argument. 'na.rm.row_var' must be a logical vector of length one.")
  }

  # Check 'na.rm.col_var' is a logical vector of length one
  if (!is.logical(na.rm.col_var) || length(na.rm.col_var) != 1) {
    stop("Invalid 'na.rm.col_var' argument. 'na.rm.col_var' must be a logical vector of length one.")
  }

  # Check 'pivot' is a character vector of length one and is one of 'wider', 'longer'
  if (!is.character(pivot) || length(pivot) != 1) {
    stop("Invalid 'pivot' argument. 'pivot' must be a character vector of length one.")
  }

  if (!(pivot %in% c("longer", "wider"))) {
    stop("Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'.")
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
  if (!is.null(ignore) && is.vector(ignore) && length(ignore) > 0 && !is.null(names(ignore))) {

    if (row_var %in% names(ignore)) {
      row_ignore <- ignore[row_var]
    } else {
      row_ignore <- NULL
    }

    if (col_var %in% names(ignore)) {
      col_ignore <- ignore[col_var]
    } else {
      col_ignore <- NULL
    }

    all_ignore <- c(col_ignore, row_ignore)

    if (length(all_ignore) > 0) {
      data <-
        data |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(unique(names(all_ignore))),
          .fns = ~ ifelse(. %in% all_ignore[[dplyr::cur_column()]], NA, .)
          ))
    }
  }

  # Remove NA rows from 'row_var' if requested
  if (na.rm.row_var) {
    data <-
      data |>
      dplyr::filter(!is.na(!!rlang::sym(row_var)))
  }

  # Remove NA rows from 'col_var' if requested
  if (na.rm.col_var) {
    data <-
      data |>
      dplyr::filter(!is.na(!!rlang::sym(col_var)))
  }

  # Create table
  cat_group_tabl <-
    data |>
    dplyr::group_by(.data[[col_var]],.data[[row_var]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::mutate(percent = .data[["count"]] / sum(.data[["count"]])) |>
    dplyr::select(dplyr::all_of(c(row_var, col_var, "count", "percent"))) |>
    dplyr::ungroup()

  # Set data format: Wider / Longer
  if (pivot == "wider") {
    cat_group_tabl <-
      cat_group_tabl |>
      tidyr::pivot_wider(id_cols = row_var,
                         names_from = col_var,
                         names_prefix = paste0(col_var, "_"),
                         values_from  = c(count, percent))
  }

  # Remove unrequested 'only' columns
  cat_group_tabl <- drop_only_cols(data = cat_group_tabl,
                                   only = only,
                                   only_type = only_type("cat"))

  cat_group_tabl
}


