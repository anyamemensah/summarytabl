# Function that removes unrequested 'only' columns
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

# Function that creates a list of two-sided formulas that map 
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

# Function that searches for and returns the names of columns 
# in a data frame that start with a specified variable stem
# (i.e., prefix)
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

# Function that extracts and return a specific substring (i.e., 
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

# Function that extracts variable 'data type'
get_data_type <- function(x) {
  class_x <- class(x)
  
  if ("haven_labelled" %in% class_x) {
    "haven_labelled"
  } else if ("ordered" %in% class_x || "factor" %in% class_x) {
    "factor"
  } else if ("POSIXt" %in% class_x || "POSIXct" %in% class_x ||
             "POSIXlt" %in% class_x || "POSIXt" %in% class_x ||
             "Date" %in% class_x  || "difftime" %in% class_x) {
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


# Permissible data types per 'table_type'
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
  
  valid_grp_type <- c("haven_labelled", "factor", "character", 
                      "logical", "datetime", "numeric")

  return (list(valid_var_types = valid_var_types, 
               valid_grp_type = valid_grp_type))
}

# Helper to check 'ignore' structure
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


# Function to apply 'ignore' values logic to variables
extract_ignore_map <- function(vars, ignore, var_stem_map = NULL) {
  ignore_map <- list()
  
  if (!is.null(ignore) && !is.null(names(ignore))) { 
    if (is.null(var_stem_map)) { 
      for (var in vars) { 
        if (var %in% names(ignore)) { 
          ignore_map[[var]] <- ignore[[var]] } 
      } 
    } else { 
      for (var in vars) { 
        if (var %in% names(var_stem_map) && var %in% names(ignore)) { 
          full_names <- var_stem_map[names(var_stem_map) == var] 
          for (fn in full_names) { ignore_map[[fn]] <- ignore[[var]] } 
        } 
      }
      missing <- setdiff(names(ignore), names(var_stem_map)) 
      if (length(missing) > 0) { 
        for (nm in missing) { 
          ignore_map[[nm]] <- ignore[[nm]] 
        } 
      } 
    } 
  } else if (!is.null(ignore) && is.null(names(ignore)) && !is.null(var_stem_map)) {
    for (var in vars) { 
      if (var %in% names(var_stem_map)) {
        full_names <- var_stem_map[names(var_stem_map) == var] 
        for (fn in full_names) { ignore_map[[fn]] <- ignore 
        } 
      } 
    } 
  } else if (!is.null(ignore) && is.null(names(ignore)) && is.null(var_stem_map)) {
    ignore_map <- list(ignore_var = ignore)
    names(ignore_map) <- vars
  } else {
    ignore_map <- NULL
  }
  
  return(list(ignore_map = ignore_map))
}


# Function to convert haven_labelled vector into character vector
# only value labels present in 'x' are retained
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

# Helper function to validate 'data' argument
check_df <- function(data) {
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.")
  }
  
  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.")
  }
  
  return(list(valid = TRUE, df = data))
}

# Function for validating 'na.rm' arguments
check_na.rm <- function(na.rm, var_label) {
  if (!is.logical(na.rm) || length(na.rm) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a logical vector of length one.",
                 var_label, var_label))
  }
  
  return(list(valid = TRUE, na.rm = na.rm))
}

check_na_removal <- function(na_removal) {
  if (!is.character(na_removal) || length(na_removal) != 1) { 
    stop("Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one.") 
  }
  
  if (!(na_removal %in% c("listwise", "pairwise"))) {
    stop("Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'.")
  }
  
  return(list(valid = TRUE, na_removal = na_removal))
}


# Function for validating variables
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

# Helper functions for validating 'var_stem'
check_var_stem <- function(data,
                           var_stem,
                           var_label,
                           var_stem_labels,
                           escape_stem,
                           ignore_stem_case) {
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
  
  var_labels <- check_named_vctr(x = var_stem_labels, names = cols, default = NULL)
  
  return(list(valid = TRUE, var_stem = var_stem, 
              label = var_label, cols = cols, 
              var_labels = var_labels))
}


# Function for validating functions with both 'var_stem' and 
# 'group' arguments
check_group_var_stem <- function(data = data,
                                 var_stem = var_stem,
                                 var_label = var_label,
                                 escape_stem = escape_stem,
                                 ignore_stem_case = ignore_stem_case,
                                 group = group,
                                 group_type = group_type,
                                 group_name = group_name,
                                 escape_group = escape_group,
                                 ignore_group_case = ignore_group_case,
                                 remove_group_non_alnum = remove_group_non_alnum,
                                 var_stem_labels = var_labels) { 
 
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
  
  var_labels <- check_named_vctr(x = var_stem_labels, names = cols, default = NULL)
  
  return(list(valid = TRUE, var_stem = var_stem, cols = cols, var_labels = var_labels, 
              cols_no_group = if (group_type == "pattern") cols_no_group else NULL,
              group = group))
}


# Function to check variable data types
check_data_type <- function(data_type, table_type, variable_type, arg_name) {
  
  valid_types <- return_data_types(table_type)[[variable_type]] 
  
  if (!data_type %in% valid_types) { 
    stop(sprintf("The '%s' column has an unsupported data type. Allowed types: %s", 
                 arg_name, paste(valid_types, collapse = ", "))) 
  }
  
  return(list(valid = TRUE, dtype = data_type))
}

# Function to validate 'only' argument
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


# Function to validate 'pivot' argument
check_pivot <- function(pivot = "longer") {
  if (!is.character(pivot) || length(pivot) != 1) {
    stop("Invalid 'pivot' argument. 'pivot' must be a character vector of length one.")
  }
  
  if (!(pivot %in% c("longer", "wider"))) {
    stop("Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'.")
  }
  
  return(list(valid = TRUE, pivot = pivot))
}

# Function to validate 'margins' argument
check_margins <- function(margins) {
  if (!is.character(margins) || length(margins) != 1) {
    stop("Invalid 'margins' argument. 'margins' must be a character vector of length one.")
  }
  
  if (!(margins %in% c("rows", "columns", "all"))) {
    stop("Invalid 'margins' argument. 'margins' must be one of 'rows', 'columns', 'all'.")
  }
  
  return(list(valid = TRUE, margins = margins))
}


# Helper function to validate 'cat_tbl' args
check_cat_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var = function(args)
      check_var(
        var_name = args$var_name,
        var_label = args$var_label,
        data = args$data
      ),
    dtype = function(args)
      check_data_type(
        data_type = get_data_type(args$data[[args$var_name]]), 
        table_type = args$table_type,
        variable_type = args$variable_type,
        arg_name = args$var_label
      ),
    na.rm = function(args)
      check_na.rm(na.rm = args$na.rm, var_label = args$label_na_rm),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    table_type = function(args)
      args$table_type
  )
  
  lapply(checks, function(chk) chk(args))
}

# Helper function to validate 'cat_group_tbl' args
check_cat_group_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    row_var = function(args)
      check_var(
        var_name = args$row_var,
        var_label = args$var_label_row,
        data = args$data
      ),
    col_var = function(args)
      check_var(
        var_name = args$col_var,
        var_label = args$var_label_col,
        data = args$data
      ),
    margins = function(args)
      check_margins(margins = args$margins),
    row_dtype = function(args)
      check_data_type(
        data_type = get_data_type(args$data[[args$row_var]]), 
        table_type = args$table_type,
        variable_type = args$variable_type,
        arg_name = args$var_label_row
      ),
    col_dtype = function(args)
      check_data_type(
        data_type = get_data_type(args$data[[args$col_var]]), 
        table_type = args$table_type,
        variable_type = args$variable_type,
        arg_name = args$var_label_col
      ),
    na_row = function(args)
      check_na.rm(na.rm = args$na_rm_row_var, var_label = args$label_na_rm_row),
    na_col = function(args)
      check_na.rm(na.rm = args$na_rm_col_var, var_label = args$label_na_rm_col),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    table_type = function(args)
      args$table_type
  )
  
  lapply(checks, function(chk) chk(args))
}


# Helper function to validate 'select_tbl' args
check_select_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_stem_labels = args$var_labels,
        escape_stem = args$escape_stem,
        ignore_stem_case = args$ignore_stem_case
      ),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    table_type = function(args)
      args$table_type
  )
  
  lapply(checks, function(chk) chk(args))
}


# Helper function to validate 'select_group_tbl' args
check_select_group_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_group_var_stem(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        escape_stem = args$escape_stem,
        ignore_stem_case = args$ignore_stem_case,
        group = args$group_var, 
        group_type = args$group_type, 
        escape_group = args$escape_group, 
        ignore_group_case = args$ignore_group_case, 
        remove_group_non_alnum = args$remove_group_non_alnum, 
        var_stem_labels = args$var_labels
      ),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    margins = function(args)
      check_margins(args$margins),
    table_type = function(args)
      args$table_type,
    group_type = function(args)
      args$group_type,
    group_name = function (args)
      args$group_name,
    escape_group = function (args)
      args$escape_group,
    ignore_group_case = function (args)
      args$ignore_group_case,
    remove_group_non_alnum = function (args)
      args$remove_group_non_alnum
  )
  
  lapply(checks, function(chk) chk(args))
}


# Helper function to validate 'mean_tbl' args
check_mean_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_stem_labels = args$var_labels,
        escape_stem = args$escape_stem,
        ignore_stem_case = args$ignore_stem_case
      ),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    table_type = function(args)
      args$table_type
  )
  
  lapply(checks, function(chk) chk(args))
}

# Helper function to validate 'mean_group_tbl' args
check_mean_group_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_group_var_stem(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        escape_stem = args$escape_stem,
        ignore_stem_case = args$ignore_stem_case,
        group = args$group_var, 
        group_type = args$group_type, 
        escape_group = args$escape_group, 
        ignore_group_case = args$ignore_group_case, 
        remove_group_non_alnum = args$remove_group_non_alnum, 
        var_stem_labels = args$var_labels
      ),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    only = function(args)
      check_only(only = args$only, table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, table_type = args$table_type,
                          group_func = args$group_func),
    table_type = function(args)
      args$table_type,
    group_type = function(args)
      args$group_type,
    group_name = function (args)
      args$group_name,
    escape_group = function (args)
      args$escape_group,
    ignore_group_case = function (args)
      args$ignore_group_case,
    remove_group_non_alnum = function (args)
      args$remove_group_non_alnum
  )
  
  lapply(checks, function(chk) chk(args))
}

