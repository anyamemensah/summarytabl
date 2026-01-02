#' @title Summarize multiple response variables by group or pattern
#'
#' @description `mean_group_tbl()` calculates summary statistics (i.e., 
#' mean, median, standard deviation, minimum, maximum, and count of 
#' non-missing values) for continuous (i.e., interval and ratio-level) 
#' variables, grouped either by another variable in your dataset or by 
#' a matched pattern in the variable names.
#'
#' @param data A data frame.
#' @param var_stem A character vector with one or more elements, where each 
#' represents either a variable stem or the complete name of a variable present 
#' in `data`. A variable 'stem' refers to a common naming pattern shared among 
#' related variables, typically reflecting repeated measures of the same idea 
#' or a group of items assessing a single concept.
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
#' @param group A character string representing a variable name or a pattern 
#' used to search for variables in `data`.
#' @param group_type A character string that defines how the `group` argument 
#' should be interpreted. Should be one of `pattern` or `variable`. Defaults 
#' to `variable`, which searches for a matching variable name in `data`.
#' @param group_name An optional character string used to rename the `group` 
#' column in the final table When `group_type` is set to `variable`, the column 
#' name defaults to the matched variable name from `data`. When set to `pattern`, 
#' the default column name is `group`.
#' @param regex_group A logical value indicating whether to use Perl-compatible 
#' regular expressions when searching for `group` variables or matching variable 
#' name patterns. Default is `FALSE`.
#' @param ignore_group_case A logical value specifying whether the search for a 
#' grouping variable (if `group_type` is `variable`) or for variables matching a 
#' pattern (if `group_type` is `pattern`) should be case-insensitive. Default is 
#' `FALSE`. Set to `TRUE` to ignore case. 
#' @param remove_group_non_alnum A logical value indicating whether to remove 
#' all non-alphanumeric characters (i.e., anything that is not a letter or 
#' number) from `group`. Default is `TRUE`.
#' @param na_removal A character string specifying how missing values are 
#' handled. Must be one of `listwise` or `pairwise`. Defaults to `listwise`.
#' - `listwise`: Removes any row that has at least one missing value 
#' across all variables returned or analyzed. (Effectively uses complete cases 
#' only.)
#' - `pairwise`: Handles missing values per variable or per pair of variables, 
#' using all available data, even if other variables in the row have missing 
#' values.
#' @param only A character string or vector of character strings specifying 
#' which summary statistics to return. Defaults to `NULL`, which includes mean 
#' (mean), median (median) standard deviation (sd), minimum (min), maximum 
#' (max), and count of non-missing values (nobs).
#' @param var_labels An optional named character vector or list used to assign
#' custom labels to variable names. Each element must be named and correspond 
#' to a variable included in the returned table. If `var_input` is set to `stem`, 
#' and any element is either unnamed or refers to a variable not present in the 
#' table, all labels will be ignored and the table will be printed without them.
#' @param ignore An optional named vector or list indicating values to exclude 
#' from variables matching specified stems (or names), and, if applicable, from 
#' a grouping variable in `data`. Defaults to `NULL`, indicating that all values 
#' are retained. To specify exclusions for variables identified by `var_stem`, 
#' use the corresponding stems or variable names as names in the vector or list. 
#' To exclude multiple values from these variables or a grouping variable, supply 
#' them as a named list.
#'
#' @returns A tibble showing summary statistics for continuous variables, grouped 
#' either by a specified variable in the dataset or by matching patterns in variable 
#' names.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' sdoh_child_ages_region <- 
#'   dplyr::select(sdoh, c(REGION, ACS_PCT_AGE_0_4, ACS_PCT_AGE_5_9,
#'                         ACS_PCT_AGE_10_14, ACS_PCT_AGE_15_17))
#' 
#' mean_group_tbl(data = sdoh_child_ages_region,
#'                var_stem = "ACS_PCT_AGE",
#'                group = "REGION",
#'                group_name = "us_region",
#'                na_removal = "pairwise",
#'                var_labels = c(
#'                  ACS_PCT_AGE_0_4 = "% of population between ages 0-4",
#'                  ACS_PCT_AGE_5_9 = "% of population between ages 5-9",
#'                  ACS_PCT_AGE_10_14 = "% of population between ages 10-14",
#'                  ACS_PCT_AGE_15_17 = "% of population between ages 15-17"))
#' 
#' set.seed(0222)
#' grouped_data <-
#'   data.frame(
#'     symptoms.t1 = sample(c(0:10, -999), replace = TRUE, size = 50),
#'     symptoms.t2 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50)
#'   )
#' 
#' mean_group_tbl(data = grouped_data,
#'                var_stem = "symptoms",
#'                group = ".t\\d",
#'                group_type = "pattern",
#'                na_removal = "listwise",
#'                ignore = c(symptoms = -999))
#'
#' @export
mean_group_tbl <- function(data,
                           var_stem,
                           group,
                           var_input = "stem",
                           regex_stem = FALSE,
                           ignore_stem_case = FALSE,
                           group_type = "variable",
                           group_name = NULL,
                           regex_group = FALSE,
                           ignore_group_case = FALSE,
                           remove_group_non_alnum = TRUE,
                           na_removal = "listwise",
                           only = NULL,
                           var_labels = NULL,
                           ignore = NULL) {
  set_call()
  on.exit({ .summarytabl$env <- NULL }, add = TRUE)
  
  args <- list(
    data = data,
    table_type = "mean",
    group_func = TRUE,
    var_stem = var_stem,
    var_label = "var_stem",
    var_input = var_input,
    valid_var_type = "valid_var_types",
    regex_stem = regex_stem,
    ignore_stem_case = ignore_stem_case,
    group_var = group,
    group_type = group_type,
    valid_grp_type = "valid_grp_types",
    group_name = group_name,
    regex_group = regex_group,
    ignore_group_case = ignore_group_case,
    remove_group_non_alnum = remove_group_non_alnum,
    na_removal = na_removal,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_mean_group_args(args)
  check_stems <- checks$var_stem
  check_cols <- checks$cols
  check_col_labels <- checks$col_labels
  check_group_var <- if (checks$group_type == "variable") checks$group_var else NULL
  check_group_name <- checks$group_name
  check_group_type <- checks$group_type
  check_stem_map <- checks$var_stem_map
  check_ignore <- checks$ignore
  check_na_removal <- checks$na_removal
  check_only <- checks$only
  check_table_type <- checks$table_type
  
  data_sub <- checks$df[c(check_group_var, check_cols)]
  
  ignore_result <-
    extract_ignore_map(
      vars = c(check_stems, check_group_var),
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
  
  mean_group_tabl <-
    purrr::map(
      .x = unique(check_cols),
      .f = ~ generate_mean_group_tabl(data_sub, .x, checks, check_group_var)
    ) |>
    dplyr::bind_rows()
  
  if (!is.null(check_group_name)) {
    mean_group_tabl <-
      mean_group_tabl |>
      dplyr::rename(
        !!rlang::sym(check_group_name) := ifelse(check_group_type == "pattern", "group", check_group_var)
      )
  }
  
  if (!is.null(check_col_labels)) {
    mean_group_tabl <-
      mean_group_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!generate_tbl_key(
          values_from = names(check_col_labels),
          values_to = unname(check_col_labels)),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  mean_group_tabl <- 
    drop_only_cols(
      data = mean_group_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
    )
  
  return(tibble::as_tibble(mean_group_tabl))
}
#'
#' @keywords internal
generate_mean_group_tabl <- function(data,
                                     variable,
                                     checks,
                                     group_var) {
  subset_data <- data[c(variable, group_var)]
  
  if (checks$group_type == "pattern") {
    group_pattern <- 
      extract_group_flags(
        cols = variable,
        pattern = checks$group_var,
        perl = checks$regex_group,
        ignore.case = checks$ignore_group_case,
        remove_non_alum = checks$remove_group_non_alnum
      )
    
    subset_data <- subset_data |> dplyr::mutate(group = group_pattern)
    group_var <- "group"
  }
  
  summarized_data <- 
    subset_data |>
    dplyr::select(dplyr::all_of(c(variable, group_var))) |>
    dplyr::filter(
      if (checks$na_removal == "pairwise") {
        !is.na(.data[[variable]]) & !is.na(.data[[group_var]])
      } else {
        TRUE
      }) |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarize(
      variable = variable,
      mean = mean(.data[[variable]], na.rm = TRUE),
      median = stats::median(.data[[variable]], na.rm = TRUE),
      sd = stats::sd(.data[[variable]], na.rm = TRUE),
      min = min(.data[[variable]], na.rm = TRUE),
      max = max(.data[[variable]], na.rm = TRUE),
      nobs = sum(!is.na(.data[[variable]]))) |>
    dplyr::ungroup() |>
    dplyr::relocate(!!rlang::sym(group_var), .after = variable)
  
  return(summarized_data)
}