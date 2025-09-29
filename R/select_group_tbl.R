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
                             escape_group = FALSE,
                             ignore_group_case = FALSE,
                             remove_group_non_alnum = TRUE,
                             na_removal = "listwise",
                             pivot = "longer",
                             only = NULL,
                             var_labels = NULL,
                             ignore = NULL){

  # Check 'data' is a data frame with at least one row/column
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.")
  }

  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.")
  }

  # Check 'var_stem' is a character vector of length one
  if (!is.character(var_stem) || length(var_stem) != 1) {
    stop("Invalid 'var_stem' argument. 'var_stem' must be a character vector of length one.")
  }

  # Find columns using 'var_stem'
  cols <- find_columns(data = data,
                       var_stem = var_stem,
                       escape = escape_stem,
                       ignore.case = ignore_stem_case)

  # Check 'cols' is a character vector of at least length one
  if (!is.character(cols) || length(cols) == 0) {
    stop(paste0(sprintf("No columns were found with the variable stem: %s", var_stem),"."))
  }

  # Check 'group' is a character vector of length one
  if (!is.character(group) || length(group) != 1) {
    stop("Invalid 'group' argument. 'group' must be a character vector of length one.")
  }

  # Check 'group_type' is of length one, and is one of 'pattern' or 'variable'
  if (!is.character(group_type) || length(group_type) != 1) {
    stop("Invalid 'group_type' argument. 'group_type' must be a character vector of length one.")
  }

  if (!(group_type %in% c("pattern", "variable"))) {
    stop("Invalid 'group_type' argument. 'group_type' must be one of 'pattern', 'variable'.")
  }

  # If 'group_type' is 'pattern', verify 'cols_no_group' is a character 
  # vector of length one; Otherwise, check 'group' exists in 'data'
  if (group_type == "pattern") {
    cols_no_group <- unique(gsub(pattern = group, replacement = "", x = cols))
    
    if (all(cols_no_group %in% cols) || !is.character(cols_no_group) || length(cols_no_group) != 1) {
      stop(paste0(
        "Invalid 'group_type' argument. Try changing the argument to: ",
        ifelse(group_type == "pattern", "variable", "pattern"),
        "."))
    }
  } else {
    group_col <- grep(pattern = group, 
                      ignore.case = ignore_group_case, 
                      x = colnames(data), 
                      value = TRUE)
    
    if (!is.character(group_col) || length(group_col) != 1) {
      stop("The 'group' argument is not a column in 'data'.")
    }
    
    group <- group_col
  }

  # Check 'na_removal' is a character vector of length one, and is one of 'listwise', 'pairwise'
  if (!is.character(na_removal) || length(na_removal) != 1) {
    stop("Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one.")
  }

  if (!(na_removal %in% c("listwise", "pairwise"))) {
    stop("Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'.")
  }
  
  # Check 'pivot' is a character vector of length one, and is one of 'wider', 'longer'
  if (!is.character(pivot) || length(pivot) != 1) {
    stop("Invalid 'pivot' argument. 'pivot' must be a character vector of length one.")
  }
  
  if (!(pivot %in% c("wider", "longer"))) {
    stop("Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'.")
  }

  # Check 'only'
  if (is.null(only)) {
    only <- only_type("select")
  } else {
    only <- tolower(trimws(only))
  }

  if (!(all(only %in% only_type("select"))) || length(only) == 0){
    stop("Invalid 'only' argument. 'only' must be a character vector of length at least one.")
  }

  # Check 'var_labels' are valid otherwise return default (NULL)
  if (!is.null(var_labels)) {
    var_labels <- check_named_vctr(x = var_labels,
                                   names = cols,
                                   default = NULL)
  }

  # Remove values that are set to 'ignore'
  if (!is.null(ignore) && is.vector(ignore) && length(ignore) > 0 && !is.null(names(ignore))) {

    if (group %in% names(ignore) && group_type == "variable") {
      group_ignore <- ignore[group]
    } else {
      group_ignore <- NULL
    }

    if (var_stem %in% names(ignore)) {
      cols_ignore <- purrr::map(cols, ~ ignore[[var_stem]]) |> stats::setNames(cols)
    } else {
      cols_ignore <- NULL
    }

    all_ignore <- c(cols_ignore, group_ignore)

    if (length(all_ignore) > 0) {
      data <-
        data |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(unique(names(all_ignore))),
          .fns = ~ ifelse(. %in% all_ignore[[dplyr::cur_column()]], NA, .)
          ))
    }
  }

  # Choose columns for table creation
  if (group_type == "variable") {
    all_cols <- c(cols,group)
  } else {
    all_cols <- cols
  }

  # Subset data
  data <- data[all_cols]

  # Remove NAs if requested 'listwise'
  if (na_removal == "listwise") {
    data <- stats::na.omit(data[all_cols])
  }

  # Create table
  select_group_tabl <- tibble::tibble()

  if (group_type == "pattern") {
    for (current_cols in unique(cols_no_group)) {

      current_cols_set <- grep(pattern = paste0(current_cols, group), x = cols, value = TRUE)

      select_group <-
        data |>
        dplyr::select(dplyr::all_of(current_cols_set)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(current_cols_set),
          names_to = "variable", values_to = "values"
        ) |>
        dplyr::mutate(
          group = extract_group_flags(
            cols = variable,
            group_flag = group,
            escape_pattern = escape_group,
            ignore.case = ignore_group_case,
            remove_non_alum = remove_group_non_alnum
          )
        ) |>
        dplyr::filter(
          if (na_removal == "pairwise")
            !is.na(.data[["group"]]) & !is.na(.data[["values"]])
          else TRUE
        ) |>
        dplyr::group_by(variable, group, values) |>
        dplyr::summarize(count = dplyr::n()) |>
        dplyr::mutate(percent = count / sum(count)) |>
        dplyr::ungroup() |>
        dplyr::arrange(variable) |>
        dplyr::select(dplyr::all_of(c("variable", "group", "values", "count", "percent")))

      select_group_tabl <- dplyr::bind_rows(select_group_tabl, select_group)
    }

  } else {
    for (current_col in unique(cols)) {
      temp_data <-
        data |>
        dplyr::select(dplyr::all_of(c(current_col, group))) |>
        dplyr::filter(
          if (na_removal == "pairwise")
            !is.na(.data[[group]]) & !is.na(.data[[current_col]])
          else TRUE
        )

      select_group <-
        summarize_select_group(temp_data, current_col, group) |>
        dplyr::select(dplyr::all_of(c("variable", group, "values", "count", "percent")))

      select_group_tabl <- dplyr::bind_rows(select_group_tabl, select_group)
    }
  }

  # Set data format: Wider / Longer
  if (pivot == "wider") {
    select_group_tabl <-
      select_group_tabl |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c("variable", if (group_type == "pattern") "group" else group)),
        names_from = values,
        names_glue = "{.value}_value_{values}",
        values_from = c(count, percent)
      )
  }

  # Add 'group_name' if supplied
  if (length(group_name) > 0) {
    select_group_tabl <-
      dplyr::rename(select_group_tabl, !!rlang::sym(group_name) := dplyr::all_of(ifelse(group_type == "pattern", "group", group)))
  }

  # Add 'var_labels' if supplied
  if (!is.null(var_labels)) {
    select_group_tabl <-
      select_group_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(variable,
                                           !!! tbl_key(values_from = names(var_labels),
                                                       values_to = unname(var_labels)),
                                           .default = variable)
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }


  # Remove unrequested 'only' columns
  select_group_tabl <- drop_only_cols(data = select_group_tabl,
                                      only = only,
                                      only_type = only_type("select"))

  select_group_tabl
}

#' @keywords internal
summarize_select_group <- function(df, var_col, group_col) {
  df |>
    dplyr::group_by(.data[[var_col]], .data[[group_col]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::mutate(
      variable = var_col,
      percent = count / sum(count)
      ) |>
    dplyr::rename(values = !!rlang::sym(var_col)) |>
    dplyr::ungroup() |>
    dplyr::arrange(variable)
}

