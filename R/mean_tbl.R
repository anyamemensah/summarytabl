#' @title Summarize continuous variables
#'
#' @description `mean_tbl()` presents descriptive statistics (mean, sd, minimum, maximum,
#'  number of non-missing observations) for interval (e.g., Test scores) and ratio level
#'  (e.g., Age) variables with the same variable stem. A variable stem is a common prefix
#'  found in related variable names, often corresponding to similar survey items, that
#'  represents a shared concept before unique identifiers (like timep oints) are added. For
#'  example, in the `stem_social_psych` dataset, the two variables 'belong_belongStem_w1'
#'  and 'belong_belongStem_w2' share the variable stem 'belong_belongStem' (e.g., "I feel
#'  like an outsider in STEM"), with suffixes (_w1, _w2) indicating different measurement
#'  waves. By default, missing data are excluded from the calculations in a listwise 
#'  fashion.
#'
#' @param data A data frame.
#' @param var_stem A character string of a variable stem or the full name of a variable in
#' `data`.
#' @param escape_stem A logical value indicating whether to escape `var_stem`. Default is
#' `FALSE`.
#' @param ignore_stem_case A logical value indicating whether the search for columns 
#' matching the supplied `var_stem` is case-insensitive. Default is `FALSE`.
#' @param na_removal A character string specifying how to remove missing values. Should be
#' one of `pairwise` or `listwise`. Default is `listwise`.
#' @param only A character string or vector of character strings of the kinds of summary
#' statistics to return. Default is `NULL`, which returns mean (mean), standard
#' deviation (sd), minimum value (min), maximum value (max), and non-missing responses
#' (nobs).
#' @param var_labels An optional named character vector or list where each element maps
#' labels to variable names. If any element is unnamed or if any labels do not match 
#' variables in returned from `data`, all labels will be ignored and the table will be 
#' printed without them.
#' @param ignore An optional vector of values to omit from the data and subsequent analysis. 
#' Default is `NULL`, which includes all present values.
#' @param ignore An optional vector that contains values to exclude from the data. Default 
#' is `NULL`, which includes all present values.
#'
#' @returns A tibble presenting summary statistics for series of continuous variables with
#' the same variable stem.
#'
#' @author Ama Nyame-Mensah
#'
#' @examples
#'
#' mean_tbl(data = social_psy_data,
#'          var_stem = "belong")
#'
#' mean_tbl(data = social_psy_data,
#'          var_stem = "belong",
#'          na_removal = "pairwise",
#'          var_labels = c(belong_1 = "I feel like I belong at this institution",
#'                         belong_2 = "I feel like part of the community",
#'                         belong_3 = "I feel valued by this institution"))
#'
#' @export
mean_tbl <- function(data,
                     var_stem,
                     escape_stem = FALSE,
                     ignore_stem_case = FALSE,
                     na_removal = "listwise",
                     only = NULL,
                     var_labels = NULL,
                     ignore = NULL) {

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
  if (!is.null(ignore) && is.vector(ignore) && length(ignore) > 0) {
    data <-
      data |>
      dplyr::mutate(dplyr::mutate(dplyr::across(.cols = dplyr::all_of(cols) , 
                                                .fns = ~ ifelse(. %in% ignore, NA, .)))
      )
  }

  # Remove rows with NAs if requested 'listwise'
  if (na_removal == "listwise") {
    data <- stats::na.omit(data[cols])
  }

  # Create table
  mean_tabl <-
    purrr::map(.x = cols, .f = ~ generate_mean_tabl(data, .x, na_removal)) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::select(variable, mean, sd, min, max, nobs)

  # Add 'var_labels' if supplied
  if (!is.null(var_labels)) {
    mean_tabl <-
      mean_tabl |>
      dplyr::mutate(
        variable_label = dplyr::case_match(variable,
                                           !!! tbl_key(values_from = names(var_labels),
                                                       values_to = unname(var_labels)),
                                           .default = variable)
      ) |>
      dplyr::relocate(variable_label, .after = variable)
  }

  # Remove unrequested 'only' columns
  mean_tabl <- drop_only_cols(data = mean_tabl,
                              only = only,
                              only_type = only_type("mean"))

  mean_tabl
}

#' @keywords internal
generate_mean_tabl <- function(data, col, na_removal) {
  data |>
    dplyr::filter(if (na_removal == "pairwise") !is.na(.data[[col]]) else TRUE) |>
    dplyr::summarize(variable = col,
                     mean = mean(.data[[col]], na.rm = TRUE),
                     sd = stats::sd(.data[[col]], na.rm = TRUE),
                     min = min(.data[[col]], na.rm = TRUE),
                     max = max(.data[[col]], na.rm = TRUE),
                     nobs = sum(!is.na(.data[[col]]))) |>
    dplyr::ungroup()
}
