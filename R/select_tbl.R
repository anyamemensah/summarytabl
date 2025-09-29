#' @title Summarize multiple response variables
#'
#' @description `select_tbl()` presents frequency counts and percentages 
#' (count, percent) for binary (e.g., Unselected/Selected) and ordinal (e.g., 
#' strongly disagree to strongly agree) variables with the same variable stem. 
#' A variable stem is a common prefix found in related variable names, often 
#' corresponding to similar survey items, that represents a shared concept before 
#' unique identifiers (like time points) are added. For example, in the `stem_social_psych` 
#' dataset, the two variables `belong_belongStem_w1` and `belong_belongStem_w2` 
#' share the variable stem `belong_belongStem` (e.g., "I feel like an outsider in 
#' STEM"), with suffixes (_w1, _w2) indicating different measurement waves. By 
#' default, missing data are excluded from the calculations in a listwise fashion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable 
#' in `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default 
#' is `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param na_removal A character string specifying how to remove missing values. Should 
#' be one of `pairwise` or `listwise`. Default is `listwise`.
#' @param pivot A character string specifying the format of the returned summary table.
#' The default is `longer`, which returns the data in long format. To return the data in
#' wide format, use `wider`.
#' @param only A character string or vector of character strings of the kinds of summary 
#' data to return. Default is `NULL`, which returns counts (count) and percentages (percent).
#' @param var_labels An optional named character vector or list where each element maps
#' labels to variable names. If any element is unnamed or if any labels do not match 
#' variables in returned from `data`, all labels will be ignored and the table will be 
#' printed without them.
#' @param ignore An optional vector that contains values to exclude from the data. Default 
#' is `NULL`, which includes all present values.
#'
#' @returns A tibble displaying frequency counts and/or percentages for each value of a 
#' set of variables sharing the same variable stem. When the output is in the wider format, 
#' columns beginning with `count_value` and `percent_value` prefixes report the count and 
#' percentage, respectively, for each distinct response  value of the variable.
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

  # Check 'cols' is a character vector of length at least one
  if (!is.character(cols) || length(cols) == 0) {
    stop(paste0(sprintf("No columns were found with the variable stem: %s", var_stem),"."))
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
  if (!is.null(ignore) && is.vector(ignore) && length(ignore) > 0) {
    data <-
      data |>
      dplyr::mutate(dplyr::mutate(dplyr::across(.cols = dplyr::all_of(cols) , 
                                                .fns = ~ ifelse(. %in% ignore, NA, .)))
      )
  }

  # Remove rows with NAs if requested listwise
  if (na_removal == "listwise") {
    data <- stats::na.omit(data[cols])
  }

  # Create table
  select_tabl <-
    purrr::map(.x = cols, .f = ~ generate_select_tabl(data, .x, na_removal)) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(variable, values, count, percent)

  # Set data format: Wider / Longer
  if (pivot == "wider") {
    select_tabl <-
      select_tabl |>
        tidyr::pivot_wider(id_cols = variable,
                           names_from = values,
                           names_glue = "{.value}_value_{values}",
                           values_from = c(count, percent))
  }

  # Add 'var_labels' if supplied
  if (!is.null(var_labels)) {
    select_tabl <-
      select_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(variable,
                                           !!! tbl_key(values_from = names(var_labels),
                                                       values_to = unname(var_labels)),
                                           .default = variable)
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }

  # Remove unrequested 'only' columns
  select_tabl <- drop_only_cols(data = select_tabl,
                                only = only,
                                only_type = only_type("select"))

  select_tabl
}

#' @keywords internal
generate_select_tabl <- function(data, col, na_removal) {
  data |>
    dplyr::group_by(.data[[col]]) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::filter(if (na_removal == "pairwise") !is.na(.data[[col]]) else TRUE) |>
    dplyr::mutate(variable = col,
                  percent = count / sum(count)) |>
    dplyr::ungroup() |>
    dplyr::rename(values = 1)
}
