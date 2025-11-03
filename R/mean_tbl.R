#' @title Summarize continuous variables
#'
#' @description `mean_tbl()` calculates summary statistics (i.e., mean, 
#' standard deviation, minimum, maximum, and count of non-missing values) 
#' for continuous (i.e., interval and ratio-level) variables.
#'
#' @param data A data frame.
#' @param var_stem A character vector containing at least one element, 
#' each representing either a variable stem or a full variable name found 
#' in `data`. A variable 'stem' is a shared naming pattern across related 
#' variables, often representing repeated measures of the same concept or 
#' a series of items measuring a single construct.
#' @param var_input A character string specifying whether the values supplied 
#' to `var_stem` should be treated as variable stems (`stem`) or as complete 
#' variable names (`name`). By default, this is set to `stem`, so the function 
#' searches for variables that begin with each stem provided. Setting this 
#' argument to `name` directs the function to look for variables that exactly 
#' match the provided names.
#' @param regex_stem A logical value indicating whether to use Perl-compatible 
#' regular expressions when searching for variable stems. Default is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for 
#' columns matching the supplied `var_stem` is case-insensitive. Default is 
#' `FALSE`.
#' @param na_removal A character string that specifies the method for handling 
#' missing values: `pairwise` or `listwise`. Defaults to `listwise`.
#' @param only A character string or vector of character strings specifying 
#' which summary statistics to return. Defaults to NULL, which includes mean 
#' (mean), standard deviation (sd), minimum (min), maximum (max), and count 
#' of non-missing values (nobs).
#' @param var_labels An optional named character vector or list used to assign
#' custom labels to variable names. Each element must be named and correspond 
#' to a variable included in the returned table. If `var_input` is set to `stem`, 
#' and any element is either unnamed or refers to a variable not present in the 
#' table, all labels will be ignored and the table will be printed without them.
#' @param ignore An optional named vector or list indicating values to exclude 
#' from variables matching specified stems (or names). Defaults to `NULL`, 
#' indicating that all values are retained. To specify exclusions for variables 
#' identified by `var_stem`, use the corresponding stems or variable names as 
#' names in the vector or list. To exclude multiple values from these variables, 
#' supply them as a named list.
#'
#' @returns A tibble showing summary statistics for continuous variables.
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
                     var_input = "stem",
                     regex_stem = FALSE,
                     ignore_stem_case = FALSE,
                     na_removal = "listwise",
                     only = NULL,
                     var_labels = NULL,
                     ignore = NULL) {
  set_call()
  on.exit({ .summarytabl$env <- NULL }, add = TRUE)
  
  args <- list(
    data = data,
    table_type = "mean",
    group_func = FALSE,
    var_stem = var_stem,
    var_label = "var_stem",
    var_input = var_input,
    valid_var_type = "valid_var_types",
    regex_stem = regex_stem,
    ignore_stem_case = ignore_stem_case,
    na_removal = na_removal,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_mean_args(args)
  check_stems <- checks$var_stem
  check_cols <- checks$cols
  check_col_labels <- checks$col_labels
  check_stem_map <- checks$var_stem_map
  check_ignore <- checks$ignore
  check_na_removal <- checks$na_removal
  check_only <- checks$only
  check_table_type <- checks$table_type
  
  data_sub <- checks$df[check_cols]
  
  ignore_result <-
    extract_ignore_map(
      vars = check_stems,
      ignore = check_ignore,
      var_stem_map = check_stem_map
    )
  ignore_map <- ignore_result$ignore_map
  
  if (!is.null(ignore_map)) {
    cols_to_modify <- names(ignore_map)
    data_sub[cols_to_modify] <- lapply(cols_to_modify, function(col) {
      replace_with_na(data_sub[[col]], ignore_map[[col]])
    })
  }
  
  if (check_na_removal == "listwise") {
    data_sub <- stats::na.omit(data_sub)
  }
  
  mean_tabl <- 
    purrr::map(check_cols, ~ generate_mean_tabl(data_sub, .x, check_na_removal)) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(variable, mean, sd, min, max, nobs)
  
  if (!is.null(check_col_labels)) {
    mean_tabl <-
      mean_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!generate_tbl_key(
          values_from = names(check_col_labels),
          values_to = unname(check_col_labels)),
        .default = NA_character_
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  mean_tabl <- 
    drop_only_cols(
      data = mean_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
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
