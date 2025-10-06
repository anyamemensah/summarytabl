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
#' categorical data. This is the primary categorical variable. When pivoted to the 
#' `wider` format, the categories of this variable will appear in the columns of 
#' the table.
#' @param margins A character string that determines how percentage values are 
#' calculated; whether they sum to one across rows, columns, or the entire table. 
#' Defaults to `all`, but can also be set to `rows` or `columns`.
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
#' grouped by `col_var`. 
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
                          margins = "all",
                          na.rm.row_var = FALSE,
                          na.rm.col_var = FALSE,
                          pivot = "longer",
                          only = NULL,
                          ignore = NULL) {
  args <- list(
    data = data,
    table_type = "cat",
    group_func = TRUE,
    row_var = row_var,
    col_var = col_var,
    margins = margins,
    var_label_row = "row_var",
    var_label_col = "col_var",
    variable_type = "valid_var_types",
    na_rm_row_var = na.rm.row_var,
    na_rm_col_var = na.rm.col_var,
    label_na_rm_row = "na.rm.row_var",
    label_na_rm_col = "na.rm.col_var",
    pivot = pivot,
    only = only,
    ignore = ignore
  )
  
  checks <- check_cat_group_args(args)
  row_name <- checks$row_var$var
  col_name <- checks$col_var$var
  df <- checks$data$df
  
  data_sub <- df[c(row_name, col_name)]
  
  labelled_vars <- c(row_name, col_name)[c(checks$row_dtype$dtype, checks$col_dtype$dtype) == "haven_labelled"]
  if (length(labelled_vars) > 0) {
    data_sub[labelled_vars] <- lapply(labelled_vars, function(v) convert_labelled_to_chr(data_sub[[v]]))
  }
  
  ignore_map <- extract_ignore_map(
    vars = c(row_name, col_name),
    ignore = checks$ignore,
    var_stem_map = NULL
  )$ignore_map
  
  if (!is.null(ignore_map)) {
    data_sub <- data_sub |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(names(ignore_map)),
        .fns = ~ ifelse(. %in% ignore_map[[dplyr::cur_column()]], NA, .)
      ))
  }
  
  vars_to_filter <- c()
  if (checks$na_row$na.rm) vars_to_filter <- c(vars_to_filter, row_name)
  if (checks$na_col$na.rm) vars_to_filter <- c(vars_to_filter, col_name)
  
  for (var in vars_to_filter) {
    data_sub <- data_sub |>
      dplyr::filter(!is.na(.data[[var]]))
  }
  
  cat_group_tabl <- summarize_cat_group(
    df = data_sub,
    row_var = row_name,
    col_var = col_name,
    margins = checks$margins$margins
  )
  
  if (checks$pivot$pivot == "wider") {
    cat_group_tabl <- cat_group_tabl |>
      tidyr::pivot_wider(
        id_cols = row_name,
        names_from = col_name,
        names_glue = paste0("{.value}_", col_name, "_{", col_name, "}"),
        values_from = c("count", "percent")
      )
  }
  
  cat_group_tabl <- drop_only_cols(
    data = cat_group_tabl,
    only = checks$only$only,
    only_type = only_type(checks$table_type)
  )
  
  return(cat_group_tabl)
}

#' @keywords internal
summarize_cat_group <- function(df,
                                row_var,
                                col_var,
                                margins) {
  margin_col <- if (margins == "rows") row_var else col_var
  
  grouped_df <- df |>
    dplyr::group_by(.data[[row_var]], .data[[col_var]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup()
  
  if (margins %in% c("rows", "columns")) {
    grouped_df <- grouped_df |>
      dplyr::group_by(.data[[margin_col]]) |>
      dplyr::mutate(percent = count / sum(count)) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data[[margin_col]])
  } else if (margins == "all") {
    total <- sum(grouped_df$count)
    grouped_df <- grouped_df |>
      dplyr::mutate(percent = count / total) |>
      dplyr::arrange(.data[[row_var]], .data[[col_var]])
  }
  
  return(grouped_df)
}