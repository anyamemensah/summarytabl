#' @title Summarize multiple response variables by group or pattern
#'
#' @description `select_group_tbl()` displays frequency counts and 
#' percentages for multiple response variables (e.g., a series of 
#' questions where participants answer "Yes" or "No" to each item) as 
#' well as ordinal variables (such as Likert or Likert-type items with 
#' responses ranging from "Strongly Disagree" to "Strongly Agree", where 
#' respondents select one response per statement, question, or item), 
#' grouped either by another variable in your dataset or by a matched 
#' pattern in the variable names. 
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
#' @param group A character string representing a variable name or a pattern 
#' used to search for variables in `data`.
#' @param group_type A character string that defines how the `group` argument 
#' should be interpreted. Should be one of `pattern` or `variable`. Defaults to 
#' `variable`, which searches for a matching variable name in `data`.
#' @param group_name An optional character string used to rename the `group` 
#' column in the final table When `group_type` is set to `variable`, the column 
#' name defaults to the matched variable name from `data`. When set to `pattern`, 
#' the default column name is `group`.
#' @param margins A character string that determines how percentage values are 
#' calculated; whether they sum to one across rows, columns, or the entire 
#' variable (i.e., all). Defaults to `all`, but can also be set to `rows` or 
#' `columns`. Note: This argument only affects the final table when `group_type` 
#' is `variable`.
#' @param regex_group A logical value indicating whether to use Perl-compatible 
#' regular expressions when searching for `group` variables or matching variable 
#' name patterns. Default is `FALSE`.
#' @param ignore_group_case A logical value specifying whether the search for a 
#' grouping variable (if `group_type` is `variable`) or for variables matching a 
#' pattern (if `group_type` is `pattern`) should be case-insensitive. Default is 
#' `FALSE`. Set to `TRUE` to ignore case. 
#' @param remove_group_non_alnum A logical value indicating whether to remove 
#' all non-alphanumeric characters (i.e., anything that is not a letter or number) 
#' from `group`. Default is `TRUE`.
#' @param na_removal A character string that specifies the method for handling 
#' missing values: `pairwise` or `listwise`. Defaults to `listwise`.
#' @param pivot A character string that determines the format of the table. By 
#' default, `longer` returns the data in the long format. To return the data in 
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
#' from variables matching specified stems (or names), and, if applicable, from a 
#' grouping variable in `data`. Defaults to `NULL`, indicating that all values are 
#' retained. To specify exclusions for variables identified by `var_stem`, use the 
#' corresponding stems or variable names as names in the vector or list. To exclude 
#' multiple values from these variables or a grouping variable, supply them as a 
#' named list.
#' @param force_pivot A logical value that enables pivoting to the 'wider' format 
#' even when variables have inconsistent value sets. By default, this is set to 
#' `FALSE` to prevent reshaping errors when values differ across variables in the 
#' returned table. Set to `TRUE` to override this safeguard and pivot to the 
#' 'wider' format regardless of value inconsistencies.
#'
#' @returns A tibble displaying the count and percentage for each category in 
#' a multi-response variable, grouped either by a specified variable in the 
#' dataset or by matching patterns in variable names.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' select_group_tbl(data = stem_social_psych,
#'                  var_stem = "belong_belong",
#'                  group = "\\d",
#'                  group_type = "pattern",
#'                  group_name = "wave",
#'                  na_removal = "pairwise",
#'                  pivot = "wider",
#'                  only = "count")
#' 
#' tas_recoded <-
#'   tas |>
#'   dplyr::mutate(sex = dplyr::case_when(
#'     sex == 1 ~ "female",
#'     sex == 2 ~ "male",
#'     TRUE ~ NA)) |>
#'   dplyr::mutate(dplyr::across(
#'     .cols = dplyr::starts_with("involved_"),
#'     .fns = ~ dplyr::case_when(
#'       .x == 1 ~ "selected",
#'       .x == 0 ~ "unselected",
#'       TRUE ~ NA)
#'   ))
#'
#' select_group_tbl(data = tas_recoded,
#'                  var_stem = "involved_",
#'                  group = "sex",
#'                  group_type = "variable",
#'                  na_removal = "pairwise",
#'                  pivot = "wider")
#'
#' depressive_recoded <-
#'   depressive |>
#'   dplyr::mutate(sex = dplyr::case_when(
#'     sex == 1 ~ "male",
#'     sex == 2 ~ "female",
#'     TRUE ~ NA)) |>
#'   dplyr::mutate(dplyr::across(
#'     .cols = dplyr::starts_with("dep_"),
#'     .fns = ~ dplyr::case_when(
#'       .x == 1 ~ "often",
#'       .x == 2 ~ "sometimes",
#'       .x == 3 ~ "hardly",
#'       TRUE ~ NA
#'     )
#'   ))
#'
#' select_group_tbl(data = depressive_recoded,
#'                  var_stem = "dep",
#'                  group = "sex",
#'                  group_type = "variable",
#'                  na_removal = "listwise",
#'                  pivot = "wider",
#'                  only = "percent",
#'                  var_labels =
#'                    c("dep_1" = "how often child feels sad and blue",
#'                      "dep_2" = "how often child feels nervous, tense, or on edge",
#'                      "dep_3" = "how often child feels happy",
#'                      "dep_4" = "how often child feels bored",
#'                      "dep_5" = "how often child feels lonely",
#'                      "dep_6" = "how often child feels tired or worn out",
#'                      "dep_7" = "how often child feels excited about something",
#'                      "dep_8" = "how often child feels too busy to get everything"))
#'
#' @export
select_group_tbl <- function(data,
                             var_stem,
                             group,
                             var_input = "stem",
                             regex_stem = FALSE,
                             ignore_stem_case = FALSE,
                             group_type = "variable",
                             group_name = NULL,
                             margins = "all",
                             regex_group = FALSE,
                             ignore_group_case = FALSE,
                             remove_group_non_alnum = TRUE,
                             na_removal = "listwise",
                             pivot = "longer",
                             only = NULL,
                             var_labels = NULL,
                             ignore = NULL,
                             force_pivot = FALSE) {
  args <- list(
    data = data,
    table_type = "select",
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
    margins = margins,
    regex_group = regex_group,
    ignore_group_case = ignore_group_case,
    remove_group_non_alnum = remove_group_non_alnum,
    na_removal = na_removal,
    pivot = pivot,
    only = only,
    var_labels = var_labels,
    ignore = ignore,
    force_pivot = force_pivot,
    .main_env = environment()
  )
  
  checks <- check_select_group_args(args)
  check_stems <- checks$var_stem
  check_cols <- checks$cols
  check_col_labels <- checks$col_labels
  check_group_var <- if (checks$group_type == "variable") checks$group_var else NULL
  check_group_name <- checks$group_name
  check_group_type <- checks$group_type
  check_stem_map <- checks$var_stem_map
  check_ignore <- checks$ignore
  check_na_removal <- checks$na_removal
  check_pivot <- checks$pivot
  check_only <- checks$only
  check_force_pivot <- checks$force_pivot
  check_table_type <- checks$table_type
  check_env <- checks$env
  
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
  
  select_group_tabl <-
    purrr::map(
      .x = unique(check_cols),
      .f = ~ generate_select_group_tabl(data_sub, .x, checks, check_group_var)
    ) |>
    purrr::reduce(dplyr::bind_rows)

  if (!is.null(check_group_name)) {
    select_group_tabl <-
      select_group_tabl |>
      dplyr::rename(
        !!rlang::sym(check_group_name) := ifelse(check_group_type == "pattern", "group", check_group_var)
      )
    check_group_var <- check_group_name
  }
  
  if (check_pivot == "wider" && 
      override_pivot(
        tabl = select_group_tabl,
        var_col = "variable",
        values_col = "values",
        allow_overide = check_force_pivot, 
        .main_env = check_env)) {
    id_cols <- 
      if (check_group_type == "pattern") {
        c("variable", check_group_var)
      } else {
        c("variable", "values")
      }
    
    names_from <- 
      if (check_group_type == "pattern") {
        "values"
      } else {
        check_group_var
      }
    
    names_glue <- 
      if (check_group_type == "pattern") {
        paste0("{.value}_value_{values}")
      } else {
        paste0("{.value}_", check_group_var, "_{", check_group_var, "}")
      }
    
    select_group_tabl <- 
      pivot_tbl_wider(
        data = select_group_tabl,
        id_cols = id_cols,
        names_from = names_from,
        names_glue = names_glue,
        values_from = c("count", "percent")
      )
  }
  
  if (!is.null(check_col_labels)) {
    select_group_tabl <-
      select_group_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!generate_tbl_key(
          values_from = names(check_col_labels),
          values_to = unname(check_col_labels), 
          .main_env = check_env),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  select_group_tabl <-
    drop_only_cols(
      data = select_group_tabl,
      only = check_only,
      only_type = only_type(check_table_type)
    )

  return(tibble::as_tibble(select_group_tabl))
}

#' @keywords internal
generate_select_group_tabl <- function(data,
                                       variable,
                                       checks,
                                       group_var) {
  sub_dat <- data[c(variable, group_var)]
  
  if (checks$group_type == "pattern") {
    group_pattern <- 
      extract_group_flags(
        cols = variable,
        pattern = checks$group_var,
        perl = checks$regex_group,
        ignore.case = checks$ignore_group_case,
        remove_non_alum = checks$remove_group_non_alnum
      )
    
    sub_dat <- sub_dat |> dplyr::mutate(group = group_pattern)
    group_var <- "group"
  }
  
  temp_data <- 
    sub_dat |>
    dplyr::select(dplyr::all_of(c(variable, group_var))) |>
    dplyr::filter(
      if (checks$na_removal == "pairwise") {
        !is.na(.data[[variable]]) & !is.na(.data[[group_var]])
      } else {
        TRUE
      })
  
  if (checks$group_type == "variable") {
    temp_data <- 
      summarize_select_group(
        data = temp_data,
        var_col = variable,
        group_col = group_var,
        margins = checks$margins
      )
  } else {
    temp_data <- 
      temp_data |>
      dplyr::group_by(.data[[group_var]], .data[[variable]]) |>
      dplyr::summarize(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::mutate(percent = count / sum(count)) |>
      dplyr::arrange(.data[[group_var]], .data[[variable]]) |>
      dplyr::mutate(variable = variable) |>
      dplyr::rename(values = !!rlang::sym(variable)) |>
      dplyr::relocate(variable) |>
      dplyr::arrange(variable)
  }
  
  return(temp_data)
}
#'
#' @keywords internal
summarize_select_group <- function(data, var_col, group_col, margins) {
  margin_col <- if (margins == "rows") var_col else group_col
  
  grouped_data <- 
    data |>
    dplyr::group_by(.data[[group_col]], .data[[var_col]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::ungroup()
  
  if (margins %in% c("rows", "columns")) {
    grouped_data <- 
      grouped_data |>
      dplyr::group_by(.data[[margin_col]]) |>
      dplyr::mutate(percent = count / sum(count)) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data[[margin_col]])
  } else {
    total <- sum(grouped_data$count)
    grouped_data <- 
      grouped_data |>
      dplyr::mutate(percent = count / total) |>
      dplyr::arrange(.data[[group_col]], .data[[var_col]])
  }
  
  grouped_data |>
    dplyr::mutate(variable = var_col) |>
    dplyr::rename(values = !!rlang::sym(var_col)) |>
    dplyr::relocate(variable) |>
    dplyr::arrange(variable)
}
