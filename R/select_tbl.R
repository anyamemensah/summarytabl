#' @title Summarize multiple response variables
#'
#' @description `select_tbl()` displays frequency counts and percentages 
#' for multiple response variables (e.g., a series of questions where 
#' participants answer "Yes" or "No" to each item) as well as ordinal 
#' variables (such as Likert or Likert-type items with responses ranging 
#' from "Strongly Disagree" to "Strongly Agree", where respondents select 
#' one response per statement, question, or item).
#' 
#' @param data A data frame.
#' @param var_stem A character vector with one or more elements, where each 
#' represents either a variable stem or the complete name of a variable present 
#' in `data`. A variable 'stem' refers to a common naming pattern shared among 
#' related variables, typically reflecting repeated measures of the same idea 
#' or a group of items assessing a single concept.
#' @param var_input A character string specifying whether the values 
#' supplied to `var_stem` should be treated as variable stems (`stem`) or 
#' as complete variable names (`name`). By default, this is set to `stem`, 
#' so the function searches for variables that begin with each stem provided. 
#' Setting this argument to `name` directs the function to look for variables 
#' that exactly match the provided names.
#' @param regex_stem A logical value indicating whether to use Perl-compatible 
#' regular expressions when searching for variable stems. Default is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for 
#' columns matching the supplied `var_stem` is case-insensitive. Default is 
#' `FALSE`.
#' @param na_removal A character string specifying how missing values are 
#' handled. Must be one of `listwise` or `pairwise`. Defaults to `listwise`.
#' - `listwise`: Removes any row that has at least one missing value 
#' across all variables returned or analyzed. (Effectively uses complete cases 
#' only.)
#' - `pairwise`: Handles missing values per variable or per pair of variables, 
#' using all available data, even if other variables in the row have missing 
#' values.
#' @param pivot A character string that determines the format of the table. By 
#' default, `longer` returns the data in the long format. To receive the data in 
#' the `wide` format, specify `wider`.
#' @param only A character string or vector of character strings of the types of 
#' summary data to return. Default is `NULL`, which returns both counts and 
#' percentages. To return only counts or percentages, use `count` or `percent`, 
#' respectively.
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
#' @param force_pivot A logical value that enables pivoting to the 'wider' 
#' format even when variables have inconsistent value sets. By default, this is 
#' set to `FALSE` to prevent reshaping errors when values differ across variables 
#' in the returned table. Set to `TRUE` to override this safeguard and pivot to 
#' the 'wider' format regardless of value inconsistencies.
#'
#' @returns A tibble displaying the count and percentage for each category in a 
#' multiple response variable.
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
#'   c(dep_1 = "how often child feels sad and blue",
#'     dep_2 = "how often child feels nervous, tense, or on edge",
#'     dep_3 = "how often child feels happy",
#'     dep_4 = "how often child feels bored",
#'     dep_5 = "how often child feels lonely",
#'     dep_6 = "how often child feels tired or worn out",
#'     dep_7 = "how often child feels excited about something",
#'     dep_8 = "how often child feels too busy to get everything")
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
                       var_input = "stem",
                       regex_stem = FALSE,
                       ignore_stem_case = FALSE,
                       na_removal = "listwise",
                       pivot = "longer",
                       only = NULL,
                       var_labels = NULL,
                       ignore = NULL,
                       force_pivot = FALSE) {
  set_call()
  on.exit({ .summarytabl$env <- NULL }, add = TRUE)
  
  args <- list(
    data = data,
    table_type = "select",
    group_func = FALSE,
    var_stem = var_stem,
    var_label = "var_stem",
    var_input = var_input,
    valid_var_type = "valid_var_types",
    regex_stem = regex_stem,
    ignore_stem_case = ignore_stem_case,
    na_removal = na_removal,
    pivot = pivot,
    only = only,
    var_labels = var_labels,
    ignore = ignore,
    force_pivot = force_pivot
  )
  
  checks <- check_select_args(args)
  check_stems <- checks$var_stem
  check_cols <- checks$cols
  check_col_labels <- checks$col_labels
  check_stem_map <- checks$var_stem_map
  check_ignore <- checks$ignore
  check_na_removal <- checks$na_removal
  check_pivot <- checks$pivot
  check_only <- checks$only
  check_force_pivot <- checks$force_pivot
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
  
  select_tabl <- 
    purrr::map(check_cols, ~ generate_select_tabl(data_sub, .x, check_na_removal)) |>
    dplyr::bind_rows()
  
  if (check_pivot == "wider" && 
      override_pivot(
        tabl = select_tabl,
        var_col = "variable",
        values_col = "values",
        allow_override = check_force_pivot)) {
    select_tabl <- 
        pivot_tbl_wider(
          data = select_tabl,
          id_cols = "variable",
          names_from = "values",
          names_glue = paste0("{.value}_value_{values}"),
          values_from = c("count", "percent")
        )
  }
  
  if (!is.null(check_col_labels)) {
    select_tabl <-
      select_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!generate_tbl_key(
          values_from = names(check_col_labels),
          values_to = unname(check_col_labels)),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  select_tabl <-
    drop_only_cols(
      data = select_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
    )
  
  return(tibble::as_tibble(select_tabl))
}

#' @keywords internal
generate_select_tabl <- function(data, col, na_removal) {
  summarized_data <- 
    data |>
    dplyr::group_by(.data[[col]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup() |> 
    dplyr::filter(if (na_removal == "pairwise") !is.na(.data[[col]]) else TRUE) |>
    dplyr::mutate(
      variable = col,
      percent = count / sum(count)
    ) |>
    dplyr::rename(values = !!rlang::sym(col)) |>
    dplyr::select(variable, values, count, percent)
  
  return(summarized_data)
}
