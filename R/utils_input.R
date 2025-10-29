#### Utility functions used to check or validate function inputs
# Function that validates the 'table_type' argument
check_table_type <- function(table_type) {
  if (!is.character(table_type) || length(table_type) != 1) { 
    stop("Invalid 'table_type' argument. 'table_type' must be a character vector of length one.",
         call. = FALSE) 
  }
  
  if (!(table_type %in% c("cat", "select", "mean"))) {
    stop("Invalid 'table_type' argument. 'table_type' must be one of 'cat', 'select', 'mean'.",
         call. = FALSE)
  }
  
  return(list(valid = TRUE, table_type = table_type))
}

# Function that validates the 'data' argument
check_df <- function(data) {
  if (!is.data.frame(data)) {
    stop("The 'data' argument is not a data frame.", call. = FALSE)
  }
  
  if (prod(dim(data)) == 0) {
    stop("The 'data' argument is empty.", call. = FALSE)
  }
  
  return(list(valid = TRUE, df = data))
}


# Function that validates generic logical arguments
# that should be either TRUE or FALSE
check_logical <- function(x, label) {
  if (!is.logical(x) || length(x) != 1) {
    stop(sprintf("Invalid '%s' argument. '%s' must be a logical vector of length one.",label, label),
         call. = FALSE)
  }
  
  if (!(x %in% c(TRUE, FALSE))) {
    stop(sprintf("Invalid '%s' argument. '%s' must be one of 'TRUE', 'FALSE'.", label, label),
         call. = FALSE)
  }
  
  return(list(valid = TRUE, x = x))
}


# Function that validates the 'margins' argument
check_margins <- function(margins) {
  if (!is.character(margins) || length(margins) != 1) {
    stop("Invalid 'margins' argument. 'margins' must be a character vector of length one.",
         call. = FALSE)
  }
  
  if (!(margins %in% c("rows", "columns", "all"))) {
    stop("Invalid 'margins' argument. 'margins' must be one of 'rows', 'columns', 'all'.",
         call. = FALSE)
  }
  
  return(list(valid = TRUE, margins = margins))
}


# Function that validates 'na.rm' arguments
check_na.rm <- function(na.rm, var_label) {
  if (!is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm)) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a logical vector of length one.",
                 var_label, var_label),
         call. = FALSE)
  }
  
  return(list(valid = TRUE, na.rm = na.rm))
}


# Function that validates 'na_removal' arguments
check_na_removal <- function(na_removal) {
  if (!is.character(na_removal) || length(na_removal) != 1) { 
    stop("Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one.",
         call. = FALSE) 
  }
  
  if (!(na_removal %in% c("listwise", "pairwise"))) {
    stop("Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'.",
         call. = FALSE)
  }
  
  return(list(valid = TRUE, na_removal = na_removal))
}


# Function that validates the 'only' argument
check_only <- function(only = NULL, table_type) {
  current_only <- 
    if (is.null(only)) {
      only_type(table_type)
    } else {
      tolower(trimws(only))
    }
  
  if (length(current_only) == 0){
    stop("Invalid 'only' argument. 'only' must be a character vector of length at least one.",
         call. = FALSE)
  }
  
  if (!(all(current_only %in% only_type(table_type)))){
    stop(sprintf("Invalid 'only' argument. 'only' must be any of: %s.",
                 paste0(sprintf("'%s'", only_type(table_type)), collapse = ", ")),
         call. = FALSE)
  }
  
  return(list(valid = TRUE, only = current_only))
}


# Function that validates the 'pivot' argument
check_pivot <- function(pivot) {
  if (!is.character(pivot) || length(pivot) != 1) {
    stop("Invalid 'pivot' argument. 'pivot' must be a character vector of length one.",
         call. = FALSE)
  }
  
  if (!(pivot %in% c("longer", "wider"))) {
    stop("Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'.",
         call. = FALSE)
  }
  
  return(list(valid = TRUE, pivot = pivot))
}


# Function that validates the 'group_name' argument;
# (It should not contain any characters that are not 
# letters, digits, periods, or underscores)
check_group_name <- function(group_name) {
  group_name <-
    if (is.null(group_name) || is.na(group_name)) NULL else group_name
  
  if (!is.null(group_name)) {
    if(length(group_name) != 1 || !is.character(group_name)) {
      stop("Invalid 'group_name' argument. 'group_name' must be NULL or a character vector of length one.",
           call. = FALSE)
    }
    
    has_invalid_chrs <- string_has_invalid_chars(group_name)
    if (has_invalid_chrs || is.na(group_name)) {
      stop(paste("The 'group_name' argument contains invalid characters.",
                 "Column names must only include letters, digits, periods (.), or underscores (_)."),
           call. = FALSE)
    }
  }
  
  return(list(valid = TRUE, group_name = group_name))
}


# Function that validates the 'var_stem' argument. Each element
# must be a character vector of at least length one and should 
# not contain invalid characters
check_var_stem <- function(var_stem) {
  if (!is.character(var_stem) || length(var_stem) == 0) {
    stop("Invalid 'var_stem' argument. 'var_stem' must be a character vector of at least length one.",
         call. = FALSE)
  }
  
  var_stem_has_invalid_chars <- sapply(var_stem, string_has_invalid_chars)
  invalid_names <- names(which(var_stem_has_invalid_chars))
  
  if (length(invalid_names) > 0) {
    stop(paste(
      sprintf("The 'var_stem' argument contains invalid characters: %s.", 
              paste(invalid_names, collapse = ", ")),
      "\nColumn names must only include letters, digits, periods (.), or underscores (_)."),
      call. = FALSE)
  }
  
  return(list(valid = TRUE, var_stem = var_stem))
}


# Function that validates individual variable arguments 
# (i.e., 'var', 'col_var', 'row_var')
check_var <- function(var_name, var_label, data) { 
  if (!is.character(var_name) || length(var_name) != 1) { 
    stop(sprintf("Invalid '%s' argument. '%s' must be a character vector of length one.", 
                 var_label, var_label), call. = FALSE) 
  } 
  
  if (!(var_name %in% colnames(data))) { 
    stop(sprintf("The '%s' argument is not a column in 'data'.", var_label), call. = FALSE) 
  } 
  
  if (string_has_invalid_chars(var_name)) {
    stop(sprintf(paste("The '%s' argument contains invalid characters.",
                       "Column names must only include letters, digits, periods (.),",
                       "or underscores (_)."), var_label), call. = FALSE) 
  }
  
  return(list(valid = TRUE, var = var_name, label = var_label))
}


# Function that validates the 'var_input' argument
check_var_input <- function(var_input) {
  if (!is.character(var_input) || length(var_input) != 1) {
    stop("Invalid 'var_input' argument. 'var_input' must be a character vector of length one.",
         call. = FALSE)
  }
  
  if (!(var_input %in% c("stem", "name"))) {
    stop("Invalid 'var_input' argument. 'var_input' must be one of 'stem', 'name'.",
         call. = FALSE)
  }
}


# Function that validates the 'group' argument 
check_group_var <- function(group_var, group_type, col_names, ignore_case, use_regex) { 
  if (!is.character(group_var) || length(group_var) != 1) { 
    stop("Invalid 'group' argument. 'group' must be a character vector of length one.",
         call. = FALSE)
  } 
  
  if (group_type == "variable") { 
    has_invalid_chars <- string_has_invalid_chars(group_var)
    
    if (has_invalid_chars) {
      stop(paste("The 'group' argument contains invalid characters.",
                 "Column names must only include letters, digits, periods (.),",
                 "or underscores (_)."),
           call. = FALSE)
    }
    
    group_cols_matched <- grep(pattern = paste0("^", group_var, "$"), 
                               ignore.case = ignore_case, 
                               perl = use_regex,
                               x = col_names, 
                               value = TRUE)
    
    
    if (is.character(group_cols_matched) && length(group_cols_matched) == 1) {
      group_var_clean <- group_cols_matched
    } else if (is.character(group_cols_matched) && length(group_cols_matched) > 1) {
      stop(sprintf("Invalid 'group' argument. Multiple columns in 'data' matched the 'group' argument: %s.",
                   paste(group_cols_matched, collapse = ", ")),
           call. = FALSE)
    } else {
      stop(paste("Invalid 'group' argument. The value provided to 'group' is", 
                 "not a column in 'data'. Check for typos, spelling mistakes,",
                 "or invalid characters."),
           call. = FALSE)
    }
  } else {
    group_var_clean <- group_var
  }
  
  return(list(valid = TRUE, group_var = group_var_clean, orig_group_var = group_var))
}


# Function that validates the 'group_type' argument
check_group_type <- function(group_type) {
  if (!is.character(group_type) || length(group_type) != 1) {
    stop("Invalid 'group_type' argument. 'group_type' must be a character vector of length one.",
         call. = FALSE)
  }
  
  if (!(group_type %in% c("pattern", "variable"))) {
    stop("Invalid 'group_type' argument. 'group_type' must be one of 'pattern', 'variable'.",
         call. = FALSE)
  }
  
  return(list(valid = TRUE, group_type = group_type))
}


# Function that checks whether a string, 'x', contains any 
# characters that are not letters, digits, periods, or 
# underscores; returns TRUE/FALSE
string_has_invalid_chars <- function(x) {
  grepl(pattern = "[^a-zA-Z0-9._]", x = x)
}




