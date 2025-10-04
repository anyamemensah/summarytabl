#' @title Summarize continuous variables
#'
#' @description `mean_tbl()` presents descriptive statistics (mean, sd, minimum, maximum,
#'  number of non-missing observations) for interval (e.g., Test scores) and ratio level
#'  (e.g., Age) variables with the same variable stem. A variable stem is a common prefix
#'  found in related variable names, often corresponding to similar survey items, that
#'  represents a shared concept before unique identifiers (like timep oints) are added. For
#'  example, in the `stem_social_psych` dataset, the two variables 'belong_belongStem_w1'
#'  and 'belong_belongStem_w2' share the variable stem 'belong_belongStem' (e.g., "I feel
#'  like an outsider in STEM"), with suffixes (_w1, _w2) indicating different measurement
#'  waves. By default, missing data are excluded from the calculations in a listwise 
#'  fashion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable in
#' `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default is
#' `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param na_removal A character string specifying how to remove missing values. Should be
#' one of `pairwise` or `listwise`. Default is `listwise`.
#' @param only A character string or vector of character strings of the kinds of summary
#' statistics to return. Default is `NULL`, which returns mean (mean), standard
#' deviation (sd), minimum value (min), maximum value (max), and non-missing responses
#' (nobs).
#' @param var_labels An optional named character vector or list where each element maps
#' labels to variable names. If any element is unnamed or if any labels do not match 
#' variables in returned from `data`, all labels will be ignored and the table will be 
#' printed without them.
#' @param ignore An optional vector that contains values to exclude from the data. Default 
#' is `NULL`, which includes all present values.
#'
#' @returns A tibble presenting summary statistics for series of continuous variables with
#' the same variable stem.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#'
#' mean_tbl(data = social_psy_data,
#'          var_stem = "belong")
#'
#' mean_tbl(data = social_psy_data,
#'          var_stem = "belong",
#'          na_removal = "pairwise",
#'          var_labels = c(belong_1 = "I feel like I belong at this institution",
#'                         belong_2 = "I feel like part of the community",
#'                         belong_3 = "I feel valued by this institution"))
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
    var_labels = var_labels,
    escape_stem = escape_stem,
    ignore_stem_case = ignore_stem_case,
    na_removal = na_removal,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_mean_args(args)
  cols <- checks$var_stem$cols
  df <- checks$data$df
  
  data_sub <- df[cols]
  
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
  
  ignore_map <- extract_ignore_map(
    vars = c(checks$var_stem$var_stem),
    ignore = checks$ignore$ignore,
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
  
  mean_tabl <- purrr::map(cols, ~ generate_mean_tabl(data_sub, .x, checks$na_rm$na_removal)) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(variable, mean, sd, min, max, nobs)
  
  var_labels <- checks$var_stem$var_labels
  
  if (!is.null(var_labels)) {
    mean_tabl <- mean_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(
          variable,
          !!!tbl_key(values_from = names(var_labels), values_to = unname(var_labels)),
          .default = variable
        )
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  mean_tabl <- drop_only_cols(
    data = mean_tabl,
    only = checks$only$only,
    only_type = only_type(checks$table_type)
  )
  
  return(mean_tabl)
}


#' @keywords internal
generate_mean_tabl <- function(data, col, na_removal) {
  data |>
    dplyr::filter(if (na_removal == "pairwise") !is.na(.data[[col]]) else TRUE) |>
    dplyr::summarize(
      variable = col,
      mean = mean(.data[[col]], na.rm = TRUE),
      sd = stats::sd(.data[[col]], na.rm = TRUE),
      min = min(.data[[col]], na.rm = TRUE),
      max = max(.data[[col]], na.rm = TRUE),
      nobs = sum(!is.na(.data[[col]]))
    ) |>
    dplyr::ungroup()
}
