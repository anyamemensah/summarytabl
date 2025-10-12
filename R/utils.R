#### Utility functions used in 'summarytabl'
# Function that removes unrequested 'only' columns from 'data'
drop_only_cols <- function(data, only, only_type) {
  if (all(only %in% only_type)) {
    remove_patterns <- setdiff(only_type, only)
    
    if (length(remove_patterns) > 0) {
      pattern <- paste0("^", remove_patterns, collapse = "|")
      cols_to_keep <- names(data)[!grepl(pattern, names(data))]
      data <- data[cols_to_keep]
    }
  }
  
  data
}


# Function that generates a list of two-sided formulas that map 
# values from one set to another
tbl_key <- function(values_from, values_to, string = TRUE) {
  value_lengths <- vapply(list(values_from, values_to), length, numeric(1))
  
  if (!(value_lengths[[1]] == value_lengths[[2]])) {
    stop("'values_from' is not the same length as 'values_to'.")
  }
  
  if (string) {
    values_from <- as.character(values_from)
    values_to <- as.character(values_to)
  } else {
    values_from <- as.numeric(values_from)
    values_to <- as.numeric(values_to)
  }
  
  purrr::map2(.x = values_from,
              .y = values_to,
              .f = ~ rlang::new_formula(.x, .y))
}

# Function that searches for and returns the names of columns in 
# a data frame that start with a specified variable stem (i.e., prefix)
find_columns <- function(data,
                         var_stem,
                         escape = FALSE,
                         ignore.case = FALSE) {
  
  if (escape) {
    var_stem <- escape_punct(var_stem)
  }
  
  cols_found <- grep(pattern = paste0("^",var_stem),
                     ignore.case = ignore.case,
                     x = colnames(data),
                     value = TRUE)
  
  cols_found
}

# Function that escapes special punctuation characters in a 
# character vector so that they can be safely used in regular 
# expressions.
escape_punct <- function(x) {
  stopifnot("'x' is not of type vector." = is.vector(x))
  
  unlist(lapply(unlist(x), 
                gsub,
                pattern = "([][{}()+*^${|\\\\?.])", 
                replacement = "\\\\\\1"))
}


# Function that returns a set of available summary statistics 
# (descriptive types) for a specified table type
only_type <- function(table_type) {

  if (!(table_type %in% c("cat", "mean", "select"))) {
    stop("'arg' should be one of cat, mean, select.")
  }
  
  switch(
    table_type,
    cat =  c("count", "percent"),
    mean = c("mean", "sd", "min", "max", "nobs"),
    select = c("count", "percent")
  )
}


# Function that extracts and returns a specific substring (i.e., 
# 'group flag') from a vector of column names
extract_group_flags <- function(cols,
                                group_flag,
                                escape_pattern = FALSE,
                                ignore.case = FALSE,
                                remove_non_alum = FALSE) {
  if (escape_pattern) {
    pattern <- escape_punct(group_flag)
  }
  
  group_flag <- regmatches(x = cols, m = regexpr(pattern = group_flag, text = cols))
  
  if (remove_non_alum) {
    group_flag <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = group_flag)
  }
  
  group_flag
}

# Function that extracts a standardized variable 'data type'
get_data_type <- function(x) {
  class_x <- class(x)
  
  if ("haven_labelled" %in% class_x) {
    "haven_labelled"
  } else if ("ordered" %in% class_x || "factor" %in% class_x) {
    "factor"
  } else if ("POSIXt" %in% class_x || "POSIXct" %in% class_x ||
             "POSIXlt" %in% class_x || "Date" %in% class_x  || 
             "difftime" %in% class_x) {
    "datetime"
  } else if ("numeric" %in% class_x || "integer" %in% class_x ||
             "double" %in% class_x) {
    "numeric"
  } else if ("logical" %in% class_x) {
    "logical"
  } else if ("character" %in% class_x) {
    "character"
  } else {
    "other"
  }
}


# Function that returns valid variable or grouping variable data types 
# based on the specified 'table_type'
return_data_types <- function(table_type) {
  valid_var_types <- 
    switch(
      table_type,
      cat =  c("haven_labelled", "factor", "character", 
               "logical", "datetime", "numeric"),
      mean = c("numeric", "datetime"),
      select = c("haven_labelled","factor", "character", 
                 "logical", "numeric")
    )
  
  valid_grp_types <- c("haven_labelled", "factor", "character", 
                      "logical", "datetime", "numeric")

  return (list(valid_var_types = valid_var_types, valid_grp_types = valid_grp_types))
}


# Function that validates the structure of the 'ignore' argument
check_ignore_struct <- function(ignore, table_type, group_func) {
  if (!is.null(ignore) && !(is.vector(ignore) || is.list(ignore))) { 
    stop("'ignore' must be a vector, list, or NULL.") 
  }
  
  named_required <- 
    (table_type == "cat" && group_func) ||
    (table_type != "cat" && group_func) 
  
  if (!is.null(ignore) && length(ignore) > 0) { 
    if (named_required) { 
      if (!is.null(names(ignore))) return(ignore) 
    } else { 
      return(list(ignore = ignore)) 
    } 
  } 
  
  return(list(ignore = NULL))
}


# Function that defines mapping rules for handling the 'ignore' 
# argument and creating 'ignore_map'
extract_ignore_map <- function(vars, ignore, var_stem_map = NULL) {
  ignore_map <- list()
  
  if (!is.null(unlist(ignore)) && !is.null(ignore) && !is.null(names(ignore))) { 
    if (is.null(var_stem_map)) { 
      for (var in vars) { 
        if (var %in% names(ignore)) { 
          ignore_map[[var]] <- ignore[[var]] } 
      } 
    } 
    else {
      for (var in vars) {
        if (var %in% names(ignore)) {
          if (var %in% names(var_stem_map)) {
            full_names <- var_stem_map[names(var_stem_map) == var]
            for (fn in full_names) {
              ignore_map[[fn]] <- ignore[[var]]
            }
          } else {
            ignore_map[[var]] <- ignore[[var]]
          }
        }
      }
    }
  } else if (!is.null(unlist(ignore)) && !is.null(ignore) && 
             is.null(names(ignore)) && !is.null(var_stem_map)) {
    for (var in vars) { 
      if (var %in% names(var_stem_map)) {
        full_names <- var_stem_map[names(var_stem_map) == var] 
        for (fn in full_names) { ignore_map[[fn]] <- ignore 
        } 
      } 
    } 
  } else if (!is.null(unlist(ignore)) && !is.null(ignore) && 
             is.null(names(ignore)) && is.null(var_stem_map)) {
    ignore_map <- list(ignore_var = ignore)
    names(ignore_map) <- vars
  } else {
    ignore_map <- NULL
  }
  
  return(list(ignore_map = ignore_map))
}


# Function that converts a haven_labelled vector to a character 
# vector, retaining only values present in 'x'
convert_labelled_to_chr <- function(x, return_labels = FALSE) {
  x_value_labels <- attr(x, "labels", exact = TRUE)
  x_chr <- as.character(unlist(lapply(x, function(ele) ele)))
  unique_values <- na.omit(x_chr)
  
  if (return_labels) {
    if (!all(unique_values %in% unname(x_value_labels))) {
      labels_to_add <- unique_values[!unique_values %in% x_value_labels]
      x_value_labels <- c(x_value_labels, stats::setNames(labels_to_add, as.character(labels_to_add)))
    }
    
    if (!all(unname(x_value_labels) %in% unique_values)) {
      labs_to_remove <- x_value_labels[!x_value_labels %in% unique_values]
      labs_to_remove <- stats::na.omit(match(labs_to_remove, x_value_labels))
      x_value_labels <- x_value_labels[-labs_to_remove]
    }
    
    x_chr <- factor(x = x_chr, 
                    levels = unname(x_value_labels[order(unname(x_value_labels))]), 
                    labels = names(x_value_labels)[order(unname(x_value_labels))])
  }
  
  return(x_chr)
}


# Function that validates the 'data' argument
check_df <- function(data) {
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.")
  }
  
  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.")
  }
  
  return(list(valid = TRUE, df = data))
}


# Function that validates 'na.rm' arguments
check_na.rm <- function(na.rm, var_label) {
  if (!is.logical(na.rm) || length(na.rm) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a logical vector of length one.",
                 var_label, var_label))
  }
  
  return(list(valid = TRUE, na.rm = na.rm))
}


# Function that validates 'na_removal' arguments
check_na_removal <- function(na_removal) {
  if (!is.character(na_removal) || length(na_removal) != 1) { 
    stop("Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one.") 
  }
  
  if (!(na_removal %in% c("listwise", "pairwise"))) {
    stop("Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'.")
  }
  
  return(list(valid = TRUE, na_removal = na_removal))
}


# Function that validates individual variable arguments 
# (i.e., 'var', 'col_var', 'row_var')
check_var <- function(var_name, var_label, data) { 
  if (!is.character(var_name) || length(var_name) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a character vector of length one.", 
                 var_label, var_label)) 
  } 
  
  if (!(var_name %in% colnames(data))) { 
    stop(sprintf("The '%s' argument is not a column in 'data'.", var_label)) 
  } 
  
  return(list(valid = TRUE, var = var_name, label = var_label))
}


# Function that checks that functions needing only the 'var_stem' 
# argument are correctly set up, and returns the columns that match 
# the given prefix
check_var_stem <- function(data,
                           var_stem,
                           var_label,
                           var_stem_labels,
                           escape_stem,
                           ignore_stem_case,
                           table_type) {
  if (!is.character(var_stem) || length(var_stem) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a character vector of length one.", 
                 var_label, var_label)) 
  } 
  
  cols <- find_columns(data = data,
                       var_stem = var_stem,
                       escape = escape_stem,
                       ignore.case = ignore_stem_case)
  
  if (!is.character(cols) || length(cols) == 0) {
    stop(paste0(sprintf("No columns were found with the variable stem: %s", var_stem),"."))
  }
  
  col_dtypes <- stats::setNames(lapply(cols, function(x) get_data_type(data[[x]])), cols)
  valid_dtypes <- return_data_types(table_type)$valid_var_types
  dtypes <- check_multi_col_data_type(col_dtypes, valid_dtypes, var_stem)
  
  var_labels <- check_named_vctr(x = var_stem_labels, names = cols, default = NULL)
  
  
  return(
    list(
      valid = TRUE, 
      var_stem = var_stem, 
      label = var_label,
      cols = cols, 
      dtypes = dtypes, 
      var_labels = var_labels,
      dtypes = dtypes
    )
  )
}


# Function that checks that functions needing both 'var_stem' and 
# 'group' arguments are correctly set up, and returns columns that 
# match those conditions
check_group_var_stem <- function(data = data,
                                 var_stem = var_stem,
                                 var_label = var_label,
                                 escape_stem = escape_stem,
                                 ignore_stem_case = ignore_stem_case,
                                 group = group,
                                 group_type = group_type,
                                 escape_group = escape_group,
                                 ignore_group_case = ignore_group_case,
                                 remove_group_non_alnum = remove_group_non_alnum,
                                 var_stem_labels = var_labels,
                                 table_type) { 
 
  if (!is.character(var_stem) || length(var_stem) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a character vector of length one.", 
                 var_label, var_label)) 
  } 
  
  cols <- find_columns(data = data,
                       var_stem = var_stem,
                       escape = escape_stem,
                       ignore.case = ignore_stem_case)
  
  if (!is.character(cols) || length(cols) == 0) {
    stop(paste0(sprintf("No columns were found with the variable stem: %s", var_stem),"."))
  }
  
  if (group_type == "pattern") {
    cols_no_group <- unique(gsub(pattern = group, replacement = "", x = cols))
    
    if (all(cols_no_group %in% cols) || !is.character(cols_no_group) || 
        length(cols_no_group) != 1) {
      stop(
        paste("Invalid 'group' argument. The value provided to 'group' did",
              "not produce a unique or expected set of column names in 'data'.",
              "Please check for typos, spelling mistakes, or invalid characters.")
      )
    }
  } else {
    group_col <- grep(pattern = group, 
                      ignore.case = ignore_group_case, 
                      x = colnames(data), 
                      value = TRUE)
    
    if (!is.character(group_col) || length(group_col) != 1) {
      stop(
        paste("Invalid 'group' argument. The value provided to 'group' is", 
              "not a column in 'data'. Check for typos, spelling mistakes,",
              "or invalid characters.")
      )
    }
    
    group <- group_col
    
    group_data <- data[[group]]
    grp_dtype_info <-
      check_data_type(
        data_type = get_data_type(group_data),
        table_type = table_type,
        variable_type = "valid_grp_types",
        arg_name = "group"
      )
    grp_dtype <- grp_dtype_info$dtype
    names(grp_dtype) <- group
  }
  
  col_dtypes <- stats::setNames(lapply(cols, function(x) get_data_type(data[[x]])), cols)
  valid_dtypes <- return_data_types(table_type)$valid_var_types
  dtypes <- check_multi_col_data_type(col_dtypes, valid_dtypes, var_stem)
  
  var_labels <- check_named_vctr(x = var_stem_labels, names = cols, default = NULL)
  
  return(
    list(
      valid = TRUE,
      var_stem = var_stem,
      cols = cols,
      var_labels = var_labels,
      cols_no_group = if (group_type == "pattern") cols_no_group,
      group = group,
      dtypes = c(dtypes, if (group_type != "pattern") grp_dtype)
    )
  )
}


# Function that validates a single variable's data type
check_data_type <- function(data_type, table_type, variable_type, arg_name) {
  
  valid_types <- return_data_types(table_type)[[variable_type]] 
  
  if (!data_type %in% valid_types) { 
    stop(sprintf("The '%s' argument has an unsupported data type. Allowed types: %s", 
                 arg_name, paste(valid_types, collapse = ", "))) 
  }
  
  return(list(valid = TRUE, dtype = data_type))
}


# Function that validates multiple variables' data types
check_multi_col_data_type <- function(dtypes, valid_dtypes, arg_name) {
  invalid_cols <- names(dtypes)[!unlist(dtypes) %in% valid_dtypes]
  
  if (length(invalid_cols) > 0) {
    stop(sprintf(
      paste(
        "One or more columns returned using the variable stem '%s'",
        "contain an unsupported data type: %s.",
        "Allowed types: %s."
      ),
      arg_name,
      paste(invalid_cols, collapse = ", "),
      paste(valid_dtypes, collapse = ", ")
    ))
  }
  
  return(dtypes)
}

# Function that validates the 'only' argument
check_only <- function(only = NULL, table_type) {
  if (is.null(only)) {
    current_only <- only_type(table_type)
  } else {
    current_only <- tolower(trimws(only))
  }

  if (length(current_only) == 0){
    stop("Invalid 'only' argument. 'only' must be a character vector of length at least one.")
  }
  
  if (!(all(current_only %in% only_type(table_type)))){
    stop(sprintf("Invalid 'only' argument. 'only' must be any of: %s.", 
                 paste0(only_type(table_type), collapse = ", ")))
  }
  
  return(list(valid = TRUE, only = current_only))
}


# Function that validates the 'pivot' argument
check_pivot <- function(pivot = "longer") {
  if (!is.character(pivot) || length(pivot) != 1) {
    stop("Invalid 'pivot' argument. 'pivot' must be a character vector of length one.")
  }
  
  if (!(pivot %in% c("longer", "wider"))) {
    stop("Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'.")
  }
  
  return(list(valid = TRUE, pivot = pivot))
}


# Function that validates the 'margins' argument
check_margins <- function(margins) {
  if (!is.character(margins) || length(margins) != 1) {
    stop("Invalid 'margins' argument. 'margins' must be a character vector of length one.")
  }
  
  if (!(margins %in% c("rows", "columns", "all"))) {
    stop("Invalid 'margins' argument. 'margins' must be one of 'rows', 'columns', 'all'.")
  }
  
  return(list(valid = TRUE, margins = margins))
}

