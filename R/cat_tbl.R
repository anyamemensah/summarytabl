#' @title Summarize a categorical variable
#'
#' @description `cat_tbl()` presents frequency counts and percentages 
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
#' of `var`.
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
  args <- list(
    data = data,
    table_type = "cat",
    group_func = FALSE,
    var_name = var,
    var_label = "var",
    variable_type = "valid_var_types",
    na.rm = na.rm,
    label_na_rm = "na.rm",
    only = only,
    ignore = ignore
  )
  
  checks <- check_cat_args(args)
  var_name <- checks$var$var
  df <- checks$data$df
  
  data_sub <- df[var_name]
  
  if (checks$dtype$dtype == "haven_labelled") {
    data_sub[[var_name]] <- convert_labelled_to_chr(data_sub[[var_name]])
  }
  
  ignore_map <- extract_ignore_map(
    vars = var_name,
    ignore = checks$ignore$ignore,
    var_stem_map = NULL
  )$ignore_map
  
  if (!is.null(ignore_map)) {
    data_sub[[var_name]] <- ifelse(
      data_sub[[var_name]] %in% ignore_map[[var_name]],
      NA,
      data_sub[[var_name]]
    )
  }
  
  if (checks$na.rm$na.rm) {
    data_sub <- stats::na.omit(data_sub)
  }
  
  cat_tabl <- data_sub |>
    dplyr::group_by(.data[[var_name]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(percent = count / sum(count))
  
  cat_tabl <- drop_only_cols(
    data = cat_tabl,
    only = checks$only$only,
    only_type = only_type(checks$table_type)
  )
  
  return(cat_tabl)
}
