#' @title Summarize multiple response variables
#'
#' @description `select_tbl()` displays frequency counts and percentages (i.e., 
#' count and percent) for multiple response variables, including binary variables 
#' (such as Unselected/Selected) and ordinal variables (such as responses ranging 
#' from strongly disagree to strongly agree), that share a common variable stem. 
#' A variable 'stem' is a shared naming pattern across related variables, often 
#' representing repeated measures of the same concept or a series of items measuring 
#' a single construct. Missing data are excluded using `listwise` deletion by default.
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
#' @param pivot A character string that determines the format of the table. By default, 
#' `longer` returns the data in the long format. To receive the data in the `wide` 
#' format, specify `wider`.
#' @param only A character string or vector of character strings of the types of 
#' summary data to return. Default is `NULL`, which returns both counts and percentages. 
#' To return only counts or percentages, use `count` or `percent`, respectively.
#' @param var_labels An optional named character vector or list used to assign custom 
#' labels to variable names. Each element should be named and correspond to a variable in 
#' the returned table. If any element is unnamed or references a variable not returned in 
#' the table, all labels will be ignored and the table will be printed without them.
#' @param ignore An optional vector of values to exclude from variables identified by 
#' `var_stem`. Defaults to `NULL`, which retains all values.
#'
#' @returns A tibble showing the relative frequencies and/or percentages of multiple 
#' response variables sharing a common variable stem.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' select_tbl(data = tas,
#'            var_stem = "involved_",
#'            na_removal = "pairwise")
#'
#' select_tbl(data = depressive,
#'            var_stem = "dep",
#'            na_removal = "listwise",
#'            pivot = "wider",
#'            only = "percent")
#'
#' var_label_example <-
#'   c("dep_1" = "how often child feels sad and blue",
#'     "dep_2" = "how often child feels nervous, tense, or on edge",
#'     "dep_3" = "how often child feels happy",
#'     "dep_4" = "how often child feels bored",
#'     "dep_5" = "how often child feels lonely",
#'     "dep_6" = "how often child feels tired or worn out",
#'     "dep_7" = "how often child feels excited about something",
#'     "dep_8" = "how often child feels too busy to get everything")
#'
#' select_tbl(data = depressive,
#'            var_stem = "dep",
#'            na_removal = "pairwise",
#'            pivot = "longer",
#'            var_labels = var_label_example)
#'
#' select_tbl(data = depressive,
#'            var_stem = "dep",
#'            na_removal = "pairwise",
#'            pivot = "wider",
#'            only = "count",
#'            var_labels = var_label_example)
#'
#' @export
select_tbl <- function(data,
                       var_stem,
                       escape_stem = FALSE,
                       ignore_stem_case = FALSE,
                       na_removal = "listwise",
                       pivot = "longer",
                       only = NULL,
                       var_labels = NULL,
                       ignore = NULL) {
  args <- list(
    data = data,
    table_type = "select",
    group_func = FALSE,
    var_stem = var_stem,
    var_label = "var_stem",
    escape_stem = escape_stem,
    ignore_stem_case = ignore_stem_case,
    na_removal = na_removal,
    pivot = pivot,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_select_args(args)
  cols <- checks$var_stem$cols
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
    
    for (col in cols_to_modify) {
      data_sub[[col]] <- 
        replace_with_na(data_sub[[col]], ignore_map[[col]])
    }
  }
  
  if (checks$na_rm$na_removal == "listwise") {
    data_sub <- stats::na.omit(data_sub)
  }
  
  select_tabl <- 
    purrr::map(cols, ~ generate_select_tabl(data_sub, .x, checks$na_rm$na_removal)) |>
    purrr::reduce(dplyr::bind_rows)
  
  if (checks$pivot$pivot == "wider") {
    select_tabl <- 
      pivot_tbl_wider(
        data = select_tabl,
        id_cols = "variable",
        names_from = "values",
        names_glue = paste0("{.value}_value_{values}"),
        values_from = c("count", "percent")
      )
  }
  
  var_labels <- checks$var_stem$var_labels
  
  if (!is.null(var_labels)) {
    select_tabl <-
      select_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!tbl_key(
          values_from = names(var_labels),
          values_to = unname(var_labels)
        ),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  select_tabl <-
    drop_only_cols(
      data = select_tabl,
      only = checks$only$only,
      only_type = only_type(checks$table_type)
    )
  
  return(tibble::as_tibble(select_tabl))
}

#' @keywords internal
generate_select_tabl <- function(data, col, na_removal) {
  data |>
    dplyr::group_by(.data[[col]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup() |> 
    dplyr::filter(if (na_removal == "pairwise") !is.na(.data[[col]]) else TRUE) |>
    dplyr::mutate(
      variable = col,
      percent = count / sum(count)
    ) |>
    dplyr::rename(values = 1) |>
    dplyr::select(variable, values, count, percent)
}