#' @title Summarize two categorical variables
#'
#' @description `cat_group_tbl()` summarizes nominal or categorical 
#' variables by a grouping variable, returning frequency counts and 
#' percentages.
#'
#' @param data A data frame.
#' @param row_var A character string of the name of a variable in `data` 
#' containing categorical data. This is the primary categorical variable.
#' @param col_var A character string of the name of a variable in `data` 
#' containing categorical data. This is the secondary categorical variable.
#' @param margins A character string that determines how percentage values 
#' are calculated; whether they sum to one across rows, columns, or the 
#' entire table (i.e., all). Defaults to `all`, but can also be set to 
#' `rows` or `columns`.
#' @param na.rm.row_var A logical value indicating whether missing values for 
#' `row_var` should be removed before calculations. Default is `FALSE`.
#' @param na.rm.col_var A logical value indicating whether missing values for 
#' `col_var` should be removed before calculations. Default is `FALSE`.
#' @param pivot A character string that determines the format of the table. By 
#' default, `longer` returns the data in the long format. To return the data in 
#' the `wide` format, specify `wider`.
#' @param only A character string or vector of character strings of the types 
#' of summary data to return. Default is `NULL`, which returns both counts and 
#' percentages. To return only counts or percentages, use `count` or `percent`, 
#' respectively.
#' @param ignore An optional named vector or list that defines values to exclude 
#' from `row_var` and `col_var`. If set to `NULL` (default), all values are retained. 
#' To exclude multiple values from `row_var` or `col_var`, provide them as a named 
#' list.
#' 
#' @returns A tibble showing the count and percentage of each category in `row_var` 
#' by each category in `col_var`.
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
  set_call()
  on.exit({ .summarytabl$env <- NULL }, add = TRUE)
  
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
  check_row_name <- checks$row_var$var
  check_col_name <- checks$col_var$var
  check_ignore <- checks$ignore
  check_row_na.rm <- checks$na_row$na.rm
  check_col_na.rm <- checks$na_col$na.rm
  check_only <- checks$only$only
  check_table_type <- checks$table_type$table_type
  check_pivot <- checks$pivot$pivot
  check_margins <- checks$margins$margins
  vars_to_filter <- 
    c(if (check_row_na.rm) check_row_name, 
      if (check_col_na.rm) check_col_name)
  
  data_sub <- checks$data$df[c(check_row_name, check_col_name)]
  
  ignore_result <-
    extract_ignore_map(
      vars = c(check_row_name, check_col_name),
      ignore = check_ignore,
      var_stem_map = NULL
    )
  ignore_map <- ignore_result$ignore_map
  
  if (!is.null(ignore_map)) {
    cols_to_modify <- names(ignore_map)
    data_sub[cols_to_modify] <- lapply(cols_to_modify, function(col) {
      replace_with_na(data_sub[[col]], ignore_map[[col]])
    })
  }
  
  if (!is.null(vars_to_filter) && length(vars_to_filter) > 0) {
    for (var in vars_to_filter) {
      data_sub <- data_sub[!is.na(data_sub[[var]]), , drop = FALSE]
    }
  }
  
  cat_group_tabl <-
    summarize_cat_group(
      data = data_sub,
      row_var = check_row_name,
      col_var = check_col_name,
      margins = check_margins
    )
  
  if (check_pivot == "wider") {
    cat_group_tabl <-
      pivot_tbl_wider(
        data = cat_group_tabl,
        id_cols = check_row_name,
        names_from = check_col_name,
        names_glue = paste0("{.value}_", check_col_name, "_{", check_col_name, "}"),
        values_from = c("count", "percent")
      )
  }
  
  cat_group_tabl <-
    drop_only_cols(
      data = cat_group_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
    )
  
  return(tibble::as_tibble(cat_group_tabl))
}

#' @keywords internal
summarize_cat_group <- function(data,
                                row_var,
                                col_var,
                                margins) {
  margin_col <- if (margins == "rows") row_var else col_var
  
  count_data <- 
    data |> dplyr::count(.data[[row_var]], .data[[col_var]], name = "count")
  
  if (margins %in% c("rows", "columns")) {
    summarized_data <- 
      count_data |>
      dplyr::group_by(.data[[margin_col]]) |>
      dplyr::mutate(percent = count / sum(count)) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data[[margin_col]])
  } else if (margins == "all") {
    total <- sum(count_data$count)
    summarized_data <- 
      count_data |>
      dplyr::mutate(percent = count / total) |>
      dplyr::arrange(.data[[row_var]], .data[[col_var]])
  }
  
  return(summarized_data)
}