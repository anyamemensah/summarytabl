#' @title Summarize multiple response variables by group
#'
#' @description `select_group_tbl()` presents frequency counts and percentages 
#' (count, percent) for binary (e.g., Unselected/Selected) and ordinal (e.g., 
#' strongly disagree to strongly agree) variables with the same variable stem
#' by some grouping variable. A variable stem is a common prefix found in related 
#' variable names, often corresponding to similar survey items, that represents a 
#' shared concept before unique identifiers (like timep oints) are added. For 
#' example, in the `stem_social_psych` dataset, the two variables 
#' `belong_belongStem_w1` and `belong_belongStem_w2` share the variable stem 
#' `belong_belongStem` (e.g., "I feel like an outsider in STEM"), with suffixes 
#' (_w1, _w2) indicating different measurement waves. By default, missing data are 
#' excluded from the calculations in a listwise fashion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a 
#' variable in `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. 
#' Default is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param group A character string of a variable in `data` or a pattern to use to 
#' search for variables in `data`.
#' @param group_type A character string that defines the type of grouping variable. 
#' Should be one of `pattern` or `variable`. Default is `variable`, in which case the 
#' variable matching the `group` string will be searched for within `data`.
#' @param group_name A character string piped to the final table to replace the name 
#' of `group`.
#' @param margins A character string that determines how percentage values are calculated; 
#' whether they sum to one across rows, columns, or the entire variable. Defaults to `all`, 
#' but can also be set to `rows` or `columns`. Note: This argument only affects the final 
#' table when group_type is `variable`.
#' @param escape_group A logical value indicating whether to escape string supplied 
#' to `group`.
#' @param ignore_group_case A logical value indicating whether `group` is case-
#' insensitive. Default is `FALSE`.
#' @param remove_group_non_alnum A logical value indicating whether to remove all 
#' non-alphanumeric characters (anything that is not a letter or number) from `group`. 
#' Default is `TRUE`.
#' @param na_removal A character string specifying how to remove missing values. 
#' Should be one of `pairwise` or `listwise`. Default is `listwise`.
#' @param pivot A character string specifying the format of the returned summary table.
#' The default is `longer`, which returns the data in long format. To return the data in
#' wide format, use `wider`.
#' @param only A character string or vector of character strings of the kinds of summary 
#' data to return. Default is `NULL`, which returns counts (count) and percentages 
#' (percent).
#' @param var_labels An optional named character vector or list where each element 
#' maps labels to variable names. If any element is unnamed or if any labels do not 
#' match variables in returned from `data`, all labels will be ignored and the table 
#' will be printed without them.
#' @param ignore An optional named vector or list specifying values to exclude from 
#' the dataset and analysis. By default, `NULL` includes all available values. To omit 
#' values from variables returned by `var_stem`, use the provided stem as the name. To 
#' exclude values from both `var_stem` variables and a grouping variable in `data`, 
#' supply a list.
#'
#' @returns A tibble displaying frequency counts and/or percentages for each value of a 
#' set of variables sharing the same variable stem. The results are grouped by either a 
#' grouping variable in the data or by a pattern matched with variable names. When the 
#' output is in the wider format, columns beginning with `count_value` and `percent_value` 
#' prefixes report the count and percentage, respectively, for each distinct response 
#' value of the variable within each group.
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
  
  for (col in names(dtypes)) {
    if (dtypes[[col]]$dtype == "haven_labelled") {
      data_sub[[col]] <- convert_labelled_to_chr(data_sub[[col]])
    }
  }
  
  if (!is.null(group_var)) {
    group_dtype <- check_data_type(
      data_type = get_data_type(data_sub[[group_var]]),
      table_type = checks$table_type,
      variable_type = "valid_grp_type",
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
  
  select_group_tabl <- tibble::tibble()
  
  if (checks$group_type == "pattern") {
    for (current_cols in unique(checks$var_stem$cols_no_group)) {
      current_cols_set <- grep(
        pattern = paste0(current_cols, checks$var_stem$group),
        x = checks$var_stem$cols,
        value = TRUE
      )
      
      select_group <- data_sub |>
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
        dplyr::group_by(variable, group, values) |>
        dplyr::summarize(count = dplyr::n()) |>
        dplyr::mutate(percent = count / sum(count)) |>
        dplyr::ungroup() |>
        dplyr::arrange(variable)
      
      select_group_tabl <- dplyr::bind_rows(select_group_tabl, select_group)
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
      
      select_group <- summarize_select_group(
        df = temp_data,
        var_col = current_col,
        group_col = group_var,
        margins = checks$margins$margins
      )
      
      select_group_tabl <- dplyr::bind_rows(select_group_tabl, select_group)
    }
  }
  
  if (length(checks$group_name) > 0) {
    select_group_tabl <- dplyr::rename(
      select_group_tabl,
      !!rlang::sym(checks$group_name) := dplyr::all_of(
        ifelse(checks$group_type == "pattern", "group", group_var)
      )
    )
    group_var <- checks$group_name
  }
  
  if (checks$pivot$pivot == "wider") {
    if (checks$group_type == "pattern") {
      select_group_tabl <- select_group_tabl |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(c("variable", group_var)),
          names_from = values,
          names_glue = "{.value}_value_{values}",
          values_from = c("count", "percent")
        )
    } else {
      select_group_tabl <- select_group_tabl |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(c("variable", "values")),
          names_from = dplyr::all_of(group_var),
          names_glue = paste0("{.value}_", group_var, "_{", group_var, "}"),
          values_from = c("count", "percent")
        )
    }
  }
  
  var_labels <- checks$var_stem$var_labels
  
  if (!is.null(var_labels)) {
    select_group_tabl <- select_group_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(
          variable,
          !!!tbl_key(values_from = names(var_labels), values_to = unname(var_labels)),
          .default = variable
        )
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }
  
  select_group_tabl <- drop_only_cols(
    data = select_group_tabl,
    only = checks$only$only,
    only_type = only_type(checks$table_type)
  )
  
  return(select_group_tabl)
}

#' @keywords internal
summarize_select_group <- function(df, var_col, group_col, margins) {
  margin_col <- if (margins == "rows") var_col else group_col
  
  grouped_df <- df |>
    dplyr::group_by(.data[[group_col]], .data[[var_col]]) |>
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
      dplyr::arrange(.data[[group_col]], .data[[var_col]])
  }
  
  grouped_df |>
    dplyr::mutate(variable = var_col) |>
    dplyr::rename(values = !!rlang::sym(var_col)) |>
    dplyr::relocate(variable) |>
    dplyr::arrange(variable)
}