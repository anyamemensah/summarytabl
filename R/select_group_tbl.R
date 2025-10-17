#' @title Summarize multiple response variables by group or pattern
#'
#' @description `select_group_tbl()` displays frequency counts and percentages 
#' (i.e., count and percent) for multiple response variables, including binary 
#' variables (such as Unselected/Selected) and ordinal variables (such as responses 
#' ranging from strongly disagree to strongly agree), that share a common variable 
#' stem, grouped either by another variable in your dataset or by a matched pattern in 
#' the variable names. A variable 'stem' is a shared naming pattern across related 
#' variables, often representing repeated measures of the same concept or a series of 
#' items measuring a single construct. Missing data are excluded using `listwise` 
#' deletion by default.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a 
#' variable in `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. 
#' Default is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for 
#' columns matching the supplied `var_stem` is case-insensitive. Default is 
#' `FALSE`.
#' @param group A character string representing a variable name or a pattern used 
#' to search for variables in `data`.
#' @param group_type A character string that defines how the `group` argument should 
#' be interpreted. Should be one of `pattern` or `variable`. Defaults to `variable`, 
#' which searches for a matching variable name in `data`.
#' @param group_name An optional character string used to rename the `group` column 
#' in the final table When `group_type` is set to `variable`, the column name defaults 
#' to the matched variable name from `data`. When set to `pattern`, the default column 
#' name is `group`.
#' @param margins A character string that determines how percentage values are 
#' calculated; whether they sum to one across rows, columns, or the entire variable 
#' (i.e., all). Defaults to `all`, but can also be set to `rows` or `columns`. Note: 
#' This argument only affects the final table when `group_type` is `variable`.
#' @param escape_group A logical value indicating whether to escape string supplied 
#' to `group`.
#' @param ignore_group_case A logical value specifying whether the search for a 
#' grouping variable (if `group_type` is `variable`) or for variables matching a 
#' pattern (if `group_type` is `pattern`) should be case-insensitive. Default is 
#' `FALSE`. Set to `TRUE` to ignore case. 
#' @param remove_group_non_alnum A logical value indicating whether to remove all 
#' non-alphanumeric characters (i.e., anything that is not a letter or number) from 
#' `group`. Default is `TRUE`.
#' @param na_removal A character string that specifies the method for handling 
#' missing values: `pairwise` or `listwise`. Defaults to `listwise`.
#' @param pivot A character string that determines the format of the table. By 
#' default, `longer` returns the data in the long format. To return the data in the 
#' `wide` format, specify `wider`.
#' @param only A character string or vector of character strings of the types of 
#' summary data to return. Default is `NULL`, which returns both counts and 
#' percentages. To return only counts or percentages, use `count` or `percent`, 
#' respectively.
#' @param var_labels An optional named character vector or list used to assign 
#' custom labels to variable names. Each element should be named and correspond to 
#' a variable in the returned table. If any element is unnamed or references a variable 
#' not returned in the table, all labels will be ignored and the table will be printed 
#' without them.
#' @param ignore An optional named vector or list that defines values to exclude from 
#' variables matching the specified variable stem and, if applicable, a grouping variable 
#' in `data`. If set to `NULL` (default), all values are retained. To exclude values from 
#' variables identified by `var_stem`, use the stem name as the key. To exclude multiple 
#' values from `var_stem` variables or a grouping variable, provide them as a named list.
#'
#' @returns A tibble showing the relative frequencies and/or percentages of multiple 
#' response variables sharing a common variable stem. The statistics are grouped either 
#' by a specified grouping variable within the dataset or by a matched pattern in the 
#' variable names.
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
                             escape_stem = FALSE,
                             ignore_stem_case = FALSE,
                             group_type = "variable",
                             group_name = NULL,
                             margins = "all",
                             escape_group = FALSE,
                             ignore_group_case = FALSE,
                             remove_group_non_alnum = TRUE,
                             na_removal = "listwise",
                             pivot = "longer",
                             only = NULL,
                             var_labels = NULL,
                             ignore = NULL) {
  args <- list(
    data = data,
    table_type = "select",
    group_func = TRUE,
    var_stem = var_stem,
    var_label = "var_stem",
    escape_stem = escape_stem,
    ignore_stem_case = ignore_stem_case,
    group_var = group,
    group_type = group_type,
    group_name = group_name,
    margins = margins,
    escape_group = escape_group,
    ignore_group_case = ignore_group_case,
    remove_group_non_alnum = remove_group_non_alnum,
    na_removal = na_removal,
    pivot = pivot,
    only = only,
    var_labels = var_labels,
    ignore = ignore
  )
  
  checks <- check_select_group_args(args)
  cols <- checks$var_stem$cols
  col_labels_checked <- checks$var_stem$var_labels
  group_var <- if (checks$group_type == "variable") checks$var_stem$group else NULL
  group_name_checked <- checks$group_name$group_name
  data_sub <- checks$data$df[c(cols, group_var)]
  
  ignore_result <-
    extract_ignore_map(
      vars = c(checks$var_stem$var_stem, group_var),
      ignore = checks$ignore,
      var_stem_map = stats::setNames(cols, rep(checks$var_stem$var_stem, length(cols)))
    )
  ignore_map <- ignore_result$ignore_map
  
  if (!is.null(ignore_map)) {
    cols_to_modify <- names(ignore_map)
    data_sub[cols_to_modify] <- lapply(cols_to_modify, function(col) {
      replace_with_na(data_sub[[col]], ignore_map[[col]])
    })
  }
  
  if (checks$na_rm$na_removal == "listwise") {
    data_sub <- stats::na.omit(data_sub)
  }
  
  select_group_list <-
    purrr::map(
      .x = unique(cols),
      .f = ~ generate_select_group_tabl(data_sub, .x, checks, group_var)
    )
  
  select_group_tabl <- dplyr::bind_rows(select_group_list)
  
  if (!is.null(group_name_checked)) {
    select_group_tabl <-
      select_group_tabl |>
      dplyr::rename(
        !!rlang::sym(group_name_checked) := ifelse(checks$group_type == "pattern", "group", group_var)
      )
    group_var <- group_name_checked
  }
  
  if (checks$pivot$pivot == "wider") {
    id_cols <- 
      if (checks$group_type == "pattern") {
        c("variable", group_var)
      } else {
        c("variable", "values")
      }
    
    names_from <- 
      if (checks$group_type == "pattern") {
        "values"
      } else {
        group_var
      }
    
    names_glue <- 
      if (checks$group_type == "pattern") {
        paste0("{.value}_value_{values}")
      } else {
        paste0("{.value}_", group_var, "_{", group_var, "}")
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
  
  if (!is.null(col_labels_checked)) {
    select_group_tabl <-
      select_group_tabl |>
      dplyr::mutate(variable_label = dplyr::case_match(
        variable,
        !!!tbl_key(
          values_from = names(col_labels_checked),
          values_to = unname(col_labels_checked)
        ),
        .default = variable
      )) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  select_group_tabl <-
    drop_only_cols(
      data = select_group_tabl,
      only = checks$only$only,
      only_type = only_type(checks$table_type)
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
        group_flag = checks$var_stem$group,
        escape_pattern = checks$escape_group,
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
      if (checks$na_rm$na_removal == "pairwise") {
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
        margins = checks$margins$margins
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
