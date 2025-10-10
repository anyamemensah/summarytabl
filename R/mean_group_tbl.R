#' Summarize continuous variables by group
#'
#' @description `mean_group_tbl()` calculates descriptive statistics (mean, standard 
#' deviation, minimum, maximum, and number of non-missing observations) for interval 
#' and ratio-level variables that share a common prefix (variable stem), grouped either 
#' by another variable in your dataset or by a matched pattern in the variable names. A 
#' variable 'stem' is a shared naming pattern across related variables, often representing 
#' repeated measures of the same concept or a series of items measuring a single construct. 
#' By default, missing data are excluded using `listwise` deletion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable in 
#' `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default is
#' `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param group A character string representing a variable name or a pattern used to search 
#' for variables in `data`.
#' @param group_type A character string that defines how the `group` argument should be
#' interpreted. Should be one of `pattern` or `variable`. Defaults to `variable`, which 
#' searches for a matching variable name in `data`.
#' @param group_name An optional character string used to rename the `group` column in the 
#' final table. When `group_type` is set to `variable`, the column name defaults to the 
#' matched variable name from `data.` When set to `pattern`, the default column name is 
#' `group`.
#' @param escape_group A logical value indicating whether to escape string supplied to 
#' `group`.
#' @param ignore_group_case A logical value specifying whether the search for a grouping 
#' variable (if `group_type` is `variable`) or for variables matching a pattern (if 
#' `group_type` is `pattern`) should be case-insensitive. Default is `FALSE`. Set to `TRUE` 
#' to ignore case. 
#' @param remove_group_non_alnum A logical value indicating whether to remove all non-
#' alphanumeric characters (i.e., anything that is not a letter or number) from `group`. 
#' Default is `TRUE`.
#' @param na_removal A character string that specifies the method for handling missing 
#' values: `pairwise` or `listwise`. Defaults to `listwise`.
#' @param only A character string or vector of character strings specifying which summary 
#' statistics to return. Defaults to NULL, which includes mean (mean), standard deviation 
#' (sd), minimum (min), maximum (max), and count of non-missing values (nobs).
#' @param var_labels An optional named character vector or list used to assign custom 
#' labels to variable names. Each element should be named and correspond to a variable in 
#' the returned table. If any element is unnamed or references a variable not returned in 
#' the table, all labels will be ignored and the table will be printed without them.
#' @param ignore An optional named vector or list that defines values to exclude from 
#' variables matching the specified variable stem and, if applicable, a grouping variable 
#' in `data.` If set to `NULL` (default), all values are retained. To exclude values from 
#' variables identified by `var_stem`, use the stem name as the key. To exclude multiple 
#' values from both `var_stem` variables and a grouping variable, supply a named list.
#'
#' @returns A tibble presenting summary statistics for continuous variables that share a 
#' common stem in their names. The statistics are grouped either by a specified grouping 
#' variable within the dataset or by a matched pattern in the variable names.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' sdoh_child_ages_region <- dplyr::select(sdoh, c(REGION, ACS_PCT_AGE_0_4, ACS_PCT_AGE_5_9,
#'                                                 ACS_PCT_AGE_10_14, ACS_PCT_AGE_15_17))
#' mean_group_tbl(data = sdoh_child_ages_region,
#'                var_stem = "ACS_PCT_AGE",
#'                group = "REGION",
#'                group_name = "us_region",
#'                na_removal = "pairwise",
#'                var_labels = c(ACS_PCT_AGE_0_4 = "% of population between ages 0-4",
#'                               ACS_PCT_AGE_5_9 = "% of population between ages 5-9",
#'                               ACS_PCT_AGE_10_14 = "% of population between ages 10-14",
#'                               ACS_PCT_AGE_15_17 = "% of population between ages 15-17"))
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
#'                escape_group = TRUE,
#'                na_removal = "listwise",
#'                ignore = c(symptoms = -999))
#'                
#' @export
mean_group_tbl <- function(data,
                           var_stem,
                           group,
                           escape_stem = FALSE,
                           ignore_stem_case = FALSE,
                           group_type = "variable",
                           group_name = NULL,
                           escape_group = FALSE,
                           ignore_group_case = FALSE,
                           remove_group_non_alnum = TRUE,
                           na_removal = "listwise",
                           only = NULL,
                           var_labels = NULL,
                           ignore = NULL) {
  args <- list(
    data = data,
    table_type = "mean",
    group_func = TRUE,
    var_stem = var_stem,
    var_label = "var_stem",
    escape_stem = escape_stem,
    ignore_stem_case = ignore_stem_case,
    group_var = group,
    group_type = group_type,
    group_name = group_name,
    escape_group = escape_group,
    ignore_group_case = ignore_group_case,
    remove_group_non_alnum = remove_group_non_alnum,
    na_removal = na_removal,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_mean_group_args(args)
  cols <- checks$var_stem$cols
  df <- checks$data$df
  group_var <- if (checks$group_type == "variable") checks$var_stem$group else NULL
  data_sub <- df[c(cols, group_var)]
  
  dtypes <- setNames(
    lapply(cols, function(x) {
      check_data_type(
        data_type = get_data_type(data_sub[[x]]),
        table_type = checks$table_type,
        variable_type = "valid_var_types",
        arg_name = "cols"
      )
    }),
    cols
  )
  
  if (!is.null(group_var)) {
    group_dtype <- check_data_type(
      data_type = get_data_type(data_sub[[group_var]]),
      table_type = checks$table_type,
      variable_type = "valid_grp_types",
      arg_name = "group"
    )
    
    if (group_dtype$dtype == "haven_labelled") {
      data_sub[[group_var]] <- convert_labelled_to_chr(data_sub[[group_var]])
    }
  }
  
  ignore_map <- extract_ignore_map(
    vars = c(checks$var_stem$var_stem),
    ignore = checks$ignore,
    var_stem_map = stats::setNames(cols, rep(checks$var_stem$var_stem, length(cols)))
  )$ignore_map
  
  if (!is.null(ignore_map)) {
    data_sub <- data_sub |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(names(ignore_map)),
        .fns = ~ ifelse(. %in% ignore_map[[dplyr::cur_column()]], NA, .)
      ))
  }
  
  if (checks$na_rm$na_removal == "listwise") {
    data_sub <- stats::na.omit(data_sub)
  }
  
  mean_group_tabl <- tibble::tibble()
  
  if (checks$group_type == "pattern") {
    for (current_cols in unique(checks$var_stem$cols_no_group)) {
      current_cols_set <- grep(
        pattern = paste0(current_cols, checks$var_stem$group),
        x = checks$var_stem$cols,
        value = TRUE
      )
      
      mean_group <- data_sub |>
        dplyr::select(dplyr::all_of(current_cols_set)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(current_cols_set),
          names_to = "variable",
          values_to = "values"
        ) |>
        dplyr::mutate(
          group = extract_group_flags(
            cols = variable,
            group_flag = checks$var_stem$group,
            escape_pattern = checks$escape_group,
            ignore.case = checks$ignore_group_case,
            remove_non_alum = checks$remove_group_non_alnum
          )
        ) |>
        dplyr::filter(
          if (checks$na_rm$na_removal == "pairwise")
            !is.na(.data[["group"]]) & !is.na(.data[["values"]])
          else TRUE
        ) |>
        dplyr::group_by(variable, group) |>
        dplyr::summarize(
          mean = mean(values, na.rm = TRUE),
          sd = stats::sd(values, na.rm = TRUE),
          min = min(values, na.rm = TRUE),
          max = max(values, na.rm = TRUE),
          nobs = sum(!is.na(values))
        ) |>
        dplyr::ungroup()
      
      mean_group_tabl <- dplyr::bind_rows(mean_group_tabl, mean_group)
    }
    
  } else {
    for (current_col in unique(cols)) {
      temp_data <- data_sub |>
        dplyr::select(dplyr::all_of(c(current_col, group_var))) |>
        dplyr::filter(
          if (na_removal == "pairwise")
            !is.na(.data[[group_var]]) & !is.na(.data[[current_col]])
          else TRUE
        )
      
      mean_group <- summarize_mean_group(
        df = temp_data,
        var_col = current_col,
        group_col = group_var
      )
      
      mean_group_tabl <- dplyr::bind_rows(mean_group_tabl, mean_group)
    }
  }
  
  if (length(checks$group_name) > 0) {
    mean_group_tabl <- dplyr::rename(
      mean_group_tabl,
      !!rlang::sym(checks$group_name) := dplyr::all_of(
        ifelse(checks$group_type == "pattern", "group", group_var)
      )
    )
  }
  
  var_labels <- checks$var_stem$var_labels
  
  if (!is.null(var_labels)) {
    mean_group_tabl <- mean_group_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(
          variable,
          !!!tbl_key(values_from = names(var_labels), values_to = unname(var_labels)),
          .default = variable
        )
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  mean_group_tabl <- drop_only_cols(
    data = mean_group_tabl,
    only = checks$only$only,
    only_type = only_type(checks$table_type)
  )
  
  return(mean_group_tabl)
}

#' @keywords internal
summarize_mean_group <- function(df, var_col, group_col) {
  df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarize(
      variable = var_col,
      mean = mean(.data[[var_col]], na.rm = TRUE),
      sd = stats::sd(.data[[var_col]], na.rm = TRUE),
      min = min(.data[[var_col]], na.rm = TRUE),
      max = max(.data[[var_col]], na.rm = TRUE),
      nobs = sum(!is.na(.data[[var_col]]))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(!!group_col := .data[[group_col]]) |>
    dplyr::relocate(!!group_col, .after = variable)
}