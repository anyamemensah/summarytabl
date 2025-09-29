#' Summarize continuous variables by group
#'
#' @description `mean_group_tbl()` presents descriptive statistics (mean, sd, minimum, 
#' maximum, number of non-missing observations) for interval (e.g., Test scores) and 
#' ratio level (e.g., Age) variables with the same variable stem by some grouping variable. 
#' A variable stem is a common prefix found in related variable names, often corresponding 
#' to similar survey items, that represents a shared concept before unique identifiers (like 
#' time points) are added. For example, in the `stem_social_psych` dataset, the two variables 
#' 'belong_belongStem_w1' and 'belong_belongStem_w2' share the variable stem 'belong_belongStem' 
#' (e.g., "I feel like an outsider in STEM"), with suffixes (_w1, _w2) indicating different 
#' measurement waves. By default, missing data are excluded from the calculations in a listwise 
#' fashion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable in 
#' `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default is
#' `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns matching
#' the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param group A character string of a variable in `data` or a pattern to use to search for 
#' variables in `data`.
#' @param group_type A character string that defines the type of grouping variable. Should be 
#' one of `pattern` or `variable`. Default is `variable`, in which case the variable matching the 
#' `group` string will be searched for within `data`.
#' @param group_name A character string piped to the final table to replace the name of `group`.
#' @param escape_group A logical value indicating whether to escape string supplied to `group`.
#' @param ignore_group_case A logical value indicating whether `group` is case-insensitive.
#' Default is `FALSE`.
#' @param remove_group_non_alnum A logical value indicating whether to remove all non-
#' alphanumeric characters (anything that is not a letter or number) from `group`. Default 
#' is `TRUE`.
#' @param na_removal A character string specifying how to remove missing values. Should be
#' one of `pairwise` or `listwise`. Default is `listwise`.
#' @param only A character string or vector of character strings of the types of
#' summary data to return. Default is `NULL`, which returns mean (mean), standard
#' deviation (sd), minimum value (min), maximum value (max), and non-missing responses
#' (nobs).
#' @param var_labels An optional named character vector or list where each element maps
#' labels to variable names. If any element is unnamed or if any labels do not match 
#' variables in returned from `data`, all labels will be ignored and the table will be 
#' printed without them.
#' @param ignore An optional named vector or list specifying values to exclude from the 
#' dataset and analysis. By default, `NULL` includes all available values. To omit values
#' from variables returned by `var_stem`, use the provided stem as the name. To exclude 
#' values from both `var_stem` variables and a grouping variable in `data`, supply a list.
#'
#' @returns A tibble presenting summary statistics (e.g., mean, standard deviation, minimum 
#' value, maximum, number of non-missing observations) for a set of variables sharing the same 
#' variable stem. The results are grouped by either a grouping variable in the data or by a 
#' pattern matched with variable names.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#' mean_group_tbl(data = stem_social_psych,
#'                var_stem = "belong_welcomedStem",
#'                group = "_w\\d",
#'                group_type = "pattern",
#'                na_removal = "pairwise",
#'                var_labels = c(belong_welcomedStem_w1 = "I feel welcomed in STEM workplaces",
#'                               belong_welcomedStem_w2 = "I feel welcomed in STEM workplaces"),
#'                group_name = "wave")
#' 
#' mean_group_tbl(data = social_psy_data,
#'                var_stem = "belong",
#'                group = "gender",
#'                group_type = "variable",
#'                na_removal = "pairwise",
#'                var_labels = c(belong_1 = "I feel like I belong at this institution",
#'                               belong_2 = "I  feel like part of the community",
#'                               belong_3 = "I feel valued by this institution"),
#'                group_name = "gender_identity")
#' 
#' grouped_data <-
#'   data.frame(
#'     symptoms.t1 = sample(c(1:5, -999), replace = TRUE, size = 50),
#'     symptoms.t2 = sample(c(NA, 1:5, -999), replace = TRUE, size = 50)
#'   )
#' 
#' mean_group_tbl(data = grouped_data,
#'                var_stem = "symptoms",
#'                group = ".t\\d",
#'                group_type = "pattern",
#'                escape_group = TRUE,
#'                na_removal = "listwise",
#'                ignore = c(symptoms = -999))
#'                
#' @export
mean_group_tbl <- function(data,
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

  # Check 'only'
  if (is.null(only)) {
    only <- only_type("mean")
  } else {
    only <- tolower(trimws(only))
  }

  if (!(all(only %in% only_type("mean"))) || length(only) == 0){
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

    data <-
      data |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(unique(names(all_ignore))),
        .fns = ~ ifelse(. %in% all_ignore[[dplyr::cur_column()]], NA, .)
      ))
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
  mean_group_tabl <- tibble::tibble()

  if (group_type == "pattern") {
    for (current_cols in unique(cols_no_group)) {

      current_cols_set <- grep(pattern = paste0(current_cols, group), x = cols, value = TRUE)

      mean_group <-
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
        dplyr::group_by(variable, group) |>
        dplyr::summarize(
          mean = mean(values, na.rm = TRUE),
          sd = stats::sd(values, na.rm = TRUE),
          min = min(values, na.rm = TRUE),
          max = max(values, na.rm = TRUE),
          nobs = sum(!is.na(values))
        ) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::all_of(c("variable", "group", "mean", "sd", "min", "max", "nobs")))

      mean_group_tabl <- dplyr::bind_rows(mean_group_tabl, mean_group)
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

      mean_group <-
        summarize_mean_group(temp_data, current_col, group) |>
        dplyr::select(dplyr::all_of(c("variable", group, "mean", "sd", "min", "max", "nobs")))

      mean_group_tabl <- dplyr::bind_rows(mean_group_tabl, mean_group)
    }
  }

  # Add 'group_name' if supplied
  if (length(group_name) > 0) {
    mean_group_tabl <-
      dplyr::rename(mean_group_tabl, !!rlang::sym(group_name) := dplyr::all_of(ifelse(group_type == "pattern", "group", group)))
  }

  # Add 'var_labels' if supplied
  if (!is.null(var_labels)) {
    mean_group_tabl <-
      mean_group_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(variable,
                                           !!! tbl_key(values_from = names(var_labels),
                                                       values_to = unname(var_labels)),
                                           .default = variable)
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }

  # Remove unrequested 'only' columns
  mean_group_tabl <- drop_only_cols(data = mean_group_tabl,
                                    only = only,
                                    only_type = only_type("mean"))

  mean_group_tabl
}

#' @keywords internal
summarize_mean_group <- function(df, var_col, group_col) {
  df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarize(
      variable = var_col,
      mean = mean(.data[[var_col]], na.rm = TRUE),
      sd = stats::sd(.data[[var_col]], na.rm = TRUE),
      min = min(.data[[var_col]], na.rm = TRUE),
      max = max(.data[[var_col]], na.rm = TRUE),
      nobs = sum(!is.na(.data[[var_col]])),
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(variable, .data[[group_col]])
}

