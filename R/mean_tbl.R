#' @title Summarize continuous variables
#'
#' @description `mean_tbl()` calculates summary statistics (i.e., mean, standard 
#' deviation, minimum, maximum, and count of non-missing values) for interval and 
#' ratio-level variables that share a common prefix (i.e., variable stem). A variable 
#' 'stem' is a shared naming pattern across related variables, often representing 
#' repeated measures of the same concept or a series of items measuring a single 
#' construct. Missing data are excluded using `listwise` deletion by default.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable 
#' in `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default 
#' is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param na_removal A character string that specifies the method for handling missing 
#' values: `pairwise` or `listwise`. Defaults to `listwise`.
#' @param only A character string or vector of character strings specifying which summary 
#' statistics to return. Defaults to NULL, which includes mean (mean), standard deviation 
#' (sd), minimum (min), maximum (max), and count of non-missing values (nobs).
#' @param var_labels An optional named character vector or list used to assign custom 
#' labels to variable names. Each element should be named and correspond to a variable in 
#' the returned table. If any element is unnamed or references a variable not returned in 
#' the table, all labels will be ignored and the table will be printed without them.
#' @param ignore An optional vector of values to exclude from variables identified by 
#' `var_stem`. Defaults to `NULL`, which retains all values.
#'
#' @returns A tibble showing summary statistics for continuous variables sharing a common 
#' variable stem.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' sdoh_child_ages <- 
#'   dplyr::select(sdoh, c(ACS_PCT_AGE_0_4, ACS_PCT_AGE_5_9,
#'                         ACS_PCT_AGE_10_14, ACS_PCT_AGE_15_17))
#' 
#' mean_tbl(data = sdoh_child_ages, var_stem = "ACS_PCT_AGE")
#' 
#' mean_tbl(data = sdoh_child_ages,
#'          var_stem = "ACS_PCT_AGE",
#'          na_removal = "pairwise",
#'          var_labels = c(
#'            ACS_PCT_AGE_0_4 = "% of population between ages 0-4",
#'            ACS_PCT_AGE_5_9 = "% of population between ages 5-9",
#'            ACS_PCT_AGE_10_14 = "% of population between ages 10-14",
#'            ACS_PCT_AGE_15_17 = "% of population between ages 15-17"))
#'                         
#' @export
mean_tbl <- function(data,
                     var_stem,
                     escape_stem = FALSE,
                     ignore_stem_case = FALSE,
                     na_removal = "listwise",
                     only = NULL,
                     var_labels = NULL,
                     ignore = NULL) {
  args <- list(
    data = data,
    table_type = "mean",
    group_func = FALSE,
    var_stem = var_stem,
    var_label = "var_stem",
    escape_stem = escape_stem,
    ignore_stem_case = ignore_stem_case,
    na_removal = na_removal,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_mean_args(args)
  cols <- checks$var_stem$cols
  col_labels_checked <- checks$var_stem$var_labels
  df <- checks$data$df
  
  data_sub <- df[cols]
  
  ignore_result <-
    extract_ignore_map(
      vars = c(checks$var_stem$var_stem),
      ignore = checks$ignore$ignore,
      var_stem_map = stats::setNames(cols, rep(checks$var_stem$var_stem, length(cols)))
    )
  ignore_map <- ignore_result$ignore_map
  
  if (!is.null(ignore_map)) {
    cols_to_modify <- names(ignore_map)
    data_sub[cols_to_modify] <- lapply(cols_to_modify, function(col) {
      replace_with_na(data_sub[[col]], ignore_map[[col]])
    })
  }
  
  if (checks$na_rm$na_removal == "listwise") {
    data_sub <- stats::na.omit(data_sub)
  }
  
  mean_tabl <- 
    purrr::map(cols, ~ generate_mean_tabl(data_sub, .x, checks$na_rm$na_removal)) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(variable, mean, sd, min, max, nobs)
  
  if (!is.null(col_labels_checked)) {
    mean_tabl <-
      mean_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!tbl_key(
          values_from = names(col_labels_checked),
          values_to = unname(col_labels_checked)
        ),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  mean_tabl <- 
    drop_only_cols(
      data = mean_tabl,
      only = checks$only$only,
      only_type = only_type(checks$table_type)
    )
  
  return(tibble::as_tibble(mean_tabl))
}


#' @keywords internal
generate_mean_tabl <- function(data, col, na_removal) {
  if (na_removal == "pairwise") {
    data <- data[!is.na(data[[col]]), ]
  }
  
  result <- data.frame(
    variable = col,
    mean = mean(data[[col]], na.rm = TRUE),
    sd = stats::sd(data[[col]], na.rm = TRUE),
    min = min(data[[col]], na.rm = TRUE),
    max = max(data[[col]], na.rm = TRUE),
    nobs = sum(!is.na(data[[col]]))
  )
  
  return(result)
}
