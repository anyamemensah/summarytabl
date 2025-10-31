#' @title Summarize a categorical variable
#'
#' @description `cat_tbl()` summarizes nominal or categorical variables, 
#' returning frequency counts and percentages.
#'
#' @param data A data frame.
#' @param var A character string of the name of a variable in `data` 
#' containing categorical data.
#' @param na.rm A logical value indicating whether missing values should be 
#' removed before calculations. Default is `FALSE`.
#' @param only A character string or vector of character strings of the types 
#' of summary data to return. Default is `NULL`, which returns both counts and 
#' percentages. To return only counts or percentages, use `count` or `percent`, 
#' respectively.
#' @param ignore An optional vector that contains values to exclude from `var`. 
#' Default is `NULL`, which retains all values.
#'
#' @returns A tibble showing the count and percentage of each category in `var`
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
    ignore = ignore,
    .main_env = environment()
  )
  
  checks <- check_cat_args(args)
  check_var_name <- checks$var$var
  check_ignore <- checks$ignore$ignore
  check_na.rm <- checks$na.rm$na.rm
  check_only <- checks$only$only
  check_dtype <- checks$dtype$dtype
  check_table_type <- checks$table_type$table_type
  check_env <- checks$env
  
  data_sub <- checks$data$df[check_var_name]

  ignore_result <-
    extract_ignore_map(
      vars = check_var_name,
      ignore = check_ignore,
      var_stem_map = NULL
    )
  ignore_map <- ignore_result$ignore_map
  
  if (!is.null(ignore_map)) {
    data_sub[[check_var_name]] <- 
      replace_with_na(data_sub[[check_var_name]], ignore_map[[check_var_name]])
  }
  
  if (check_na.rm) {
    data_sub <- data_sub[!is.na(data_sub[[check_var_name]]), ]
  }
  
  cat_tabl <- 
    dplyr::count(data_sub, .data[[check_var_name]], name = "count") |>
    dplyr::mutate(percent = count / sum(count))
  
  cat_tabl <-
    drop_only_cols(
      data = cat_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
    )
  
  return(tibble::as_tibble(cat_tabl))
}
