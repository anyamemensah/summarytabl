#### Utility functions used to check or validate function inputs
# Function to set environment; for use with cli::cli_abort() 
# and cli::cli_warn()
set_call <- function(env = parent.frame()) {
  .summarytabl$env <- env
  invisible()
}

# Function to get environment; for use with cli::cli_abort() 
# and cli::cli_warn()
get_call <- function() {
  if(!is.null(.summarytabl$env)) {
    return(.summarytabl$env)
  }
  
  return(sys.call(sys.parent()))
}

# Function that validates the 'table_type' argument
check_table_type <- function(table_type) {
  if (!is.character(table_type) || length(table_type) != 1) {
    stop(
      "Invalid 'table_type' argument. 'table_type' must be a character vector of length one.",
      call. = FALSE
    )
  }
  
  if (!(table_type %in% c("cat", "select", "mean"))) {
    stop(
      "Invalid 'table_type' argument. 'table_type' must be one of: 'cat', 'select', 'mean'.",
      call. = FALSE
    )
  }
  
  return(list(valid = TRUE, table_type = table_type))
}

# Function that validates the 'data' argument
check_df <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c("Invalid {.arg data} argument.", "i" = "The {.arg data} argument must be a {.cls data.frame} or {.cls tibble}."),
      call = get_call()
    )
  }
  
  if (prod(dim(data)) == 0) {
    cli::cli_abort(
      c("Invalid {.arg data} argument.", "i" = "The {.arg data} argument must have at least one row and one column."),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, df = data))
}


# Function that validates generic logical arguments
# that should be either TRUE or FALSE
check_logical <- function(x, label) {
  if (!is.logical(x) || length(x) != 1) {
    cli::cli_abort(
      c("Invalid {.arg {label}} argument.", "i" = "The {.arg {label}} argument must be a logical vector of length one."),
      call = get_call()
    )
  }
  
  if (!(x %in% c(TRUE, FALSE))) {
    cli::cli_abort(
      c("Invalid {.arg {label}} argument.", "i" = "The {.arg {label}} argument must be one of: {.val {TRUE}} or {.val {FALSE}}."),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, x = x))
}


# Function that validates the 'margins' argument
check_margins <- function(margins) {
  if (!is.character(margins) || length(margins) != 1) {
    cli::cli_abort(
      c("Invalid {.arg margins} argument.", "i" = "The {.arg margins} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(margins %in% c("rows", "columns", "all"))) {
    valid_margs <- c("rows", "columns", "all")
    cli::cli_abort(c(
      "Invalid {.arg margins} argument.",
      "i" = paste(
        "The {.arg margins} argument must be one of: {.val rows},",
        "{.val columns}, or {.val all}."
      )
    ), call = get_call())
  }
  
  return(list(valid = TRUE, margins = margins))
}


# Function that validates 'na.rm' arguments
check_na.rm <- function(na.rm, var_label) {
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    cli::cli_abort(
      c("Invalid {.arg {var_label}} argument.", "i" = "The {.arg {var_label}} argument must be a logical vector of length one."),
      call = get_call()
    )
  }
  
  if (!(na.rm %in% c(TRUE, FALSE))) {
    cli::cli_abort(c(
      "Invalid {.arg {var_label}} argument.",
      "i" = paste(
        "The {.arg {var_label}} argument must be one of:",
        "{.val {TRUE}} or {.val {FALSE}}."
      )
    ),
    call = get_call())
  }
  
  return(list(valid = TRUE, na.rm = na.rm))
}


# Function that validates 'na_removal' arguments
check_na_removal <- function(na_removal) {
  if (!is.character(na_removal) || length(na_removal) != 1) {
    cli::cli_abort(
      c("Invalid {.arg na_removal} argument.", "i" = "The {.arg na_removal} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(na_removal %in% c("listwise", "pairwise"))) {
    cli::cli_abort(c(
      "Invalid {.arg na_removal} argument.",
      "i" = paste(
        "The {.arg na_removal} argument must be one of {.val listwise}",
        "or {.val pairwise}."
      )
    ),
    call = get_call())
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
  
  if (length(current_only) == 0) {
    cli::cli_abort(
      c("Invalid {.arg only} argument.", "i" = "The {.arg only} argument must be a character vector of length at least one."),
      call = get_call()
    )
  }
  
  if (!(all(current_only %in% only_type(table_type)))) {
    cli::cli_abort(
      c("Invalid {.arg only} argument.", "i" = "The {.arg only} argument must be one or more of: {.val {only_type(table_type)}}."),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, only = current_only))
}


# Function that validates the 'pivot' argument
check_pivot <- function(pivot) {
  if (!is.character(pivot) || length(pivot) != 1) {
    cli::cli_abort(
      c("Invalid {.arg pivot} argument.", "i" = "The {.arg pivot} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(pivot %in% c("longer", "wider"))) {
    cli::cli_abort(
      c("Invalid {.arg pivot} argument.", "i" = "The {.arg pivot} argument must be one of: {.val wider} or {.val longer}."),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, pivot = pivot))
}


# Function that validates the 'group_name' argument;
# (It should not contain any characters that are not
# letters, digits, periods, or underscores)
check_group_name <- function(group_name) {
  group_name <-
    if (is.null(group_name) ||
        is.na(group_name))
      NULL
  else
    group_name
  
  if (!is.null(group_name)) {
    if (length(group_name) != 1 || !is.character(group_name)) {
      cli::cli_abort(
        c("Invalid {.arg group_name} argument.", "i" = 'The {.arg group_name} argument must be NULL or a character vector of length one.'),
        call = get_call()
      )
    }
    
    has_invalid_chrs <- string_has_invalid_chars(group_name)
    if (has_invalid_chrs || is.na(group_name)) {
      cli::cli_abort(
        c(
          "Invalid {.arg group_name} argument.",
          "i" = "The {.arg group_name} argument contains invalid characters.",
          "i" = "Column names must only include letters, digits, periods (.), or underscores (_)."
        ),
        call = get_call()
      )
    }
  }
  
  return(list(valid = TRUE, group_name = group_name))
}


# Function that validates the 'var_stem' argument. Each element
# must be a character vector of at least length one and should
# not contain invalid characters
check_var_stem <- function(var_stem) {
  if (!is.character(var_stem) || length(var_stem) == 0) {
    cli::cli_abort(
      c("Invalid {.arg var_stem} argument.", "i" = "The {.arg var_stem} argument must be a character vector of at least length one."),
      call = get_call()
    )
  }
  
  var_stem_has_invalid_chars <- sapply(var_stem, string_has_invalid_chars)
  invalid_names <- names(which(var_stem_has_invalid_chars))
  
  if (length(invalid_names) > 0) {
    cli::cli_abort(
      c(
        "Invalid {.arg var_stem} argument.",
        "i" = paste(
          'The {.arg var_stem} argument contains elements with invalid characters:',
          '{.val {invalid_names}}.'
        ),
        "i" = "Column names must only include letters, digits, periods (.), or underscores (_)."
      ),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, var_stem = var_stem))
}


# Function that validates individual variable arguments
# (i.e., 'var', 'col_var', 'row_var')
check_var <- function(var_name, var_label, data) {
  if (!is.character(var_name) || length(var_name) != 1) {
    cli::cli_abort(
      c("Invalid {.arg {var_label}} argument.", "i" = "The {.arg {var_label}} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(var_name %in% colnames(data))) {
    cli::cli_abort(
      c("Invalid {.arg {var_label}} argument.", "i" = "The {.arg {var_label}} argument, {.val {var_name}}, is not a column in {.arg data}."),
      call = get_call()
    )
  }
  
  if (string_has_invalid_chars(var_name)) {
    cli::cli_abort(
      c(
        "Invalid {.arg {var_label}} argument.",
        "i" = 'The {.arg {var_label}} argument contains invalid characters.',
        "i" = "Column names must only include letters, digits, periods (.), or underscores (_)."
      ),
      call = get_call()
    )
  }
  
  return(list(
    valid = TRUE,
    var = var_name,
    label = var_label
  ))
}


# Function that validates the 'var_input' argument
check_var_input <- function(var_input) {
  if (!is.character(var_input) || length(var_input) != 1) {
    cli::cli_abort(
      c("Invalid {.arg var_input} argument.", "i" = "The {.arg var_input} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(var_input %in% c("stem", "name"))) {
    cli::cli_abort(
      c("Invalid {.arg var_input} argument.", "i" = "The {.arg var_input} argument must be one of: {.val stem} or {.val name}."),
      call = get_call()
    )
  }
}


# Function that validates the 'group' argument
check_group_var <- function(group_var,
                            group_type,
                            col_names,
                            ignore_case,
                            use_regex) {
  if (!is.character(group_var) || length(group_var) != 1) {
    cli::cli_abort(
      c("Invalid {.arg group} argument.", "i" = "The {.arg group} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (group_type == "variable") {
    has_invalid_chars <- string_has_invalid_chars(group_var)
    
    if (has_invalid_chars) {
      cli::cli_abort(
        c(
          "Invalid {.arg group} argument.",
          "i" = 'The {.arg group} argument contains invalid characters.',
          "i" = paste(
            "Column names must only include letters, digits, periods (.),",
            "or underscores (_)."
          )
        ),
        call = get_call()
      )
    }
    
    group_cols_matched <- grep(
      pattern = paste0("^", group_var, "$"),
      ignore.case = ignore_case,
      perl = use_regex,
      x = col_names,
      value = TRUE
    )
    
    
    if (is.character(group_cols_matched) &&
        length(group_cols_matched) == 1) {
      group_var_clean <- group_cols_matched
    } else if (is.character(group_cols_matched) &&
               length(group_cols_matched) > 1) {
      cli::cli_abort(c(
        "Invalid {.arg group} argument.",
        "i" = paste(
          "Multiple columns in {.arg data} matched the",
          "{.arg group} argument:",
          "{.val {group_cols_matched}}."
        )
      ), call = get_call())
    } else {
      cli::cli_abort(c(
        "Invalid {.arg group} argument.",
        "i" = paste(
          "The value provided to {.arg group}, {.val {group_var}},",
          "is not a column in {.arg data}. Check for typos,",
          "spelling mistakes or invalid characters."
        )
      ), call = get_call())
    }
  } else {
    group_var_clean <- group_var
  }
  
  return(list(
    valid = TRUE,
    group_var = group_var_clean,
    orig_group_var = group_var
  ))
}


# Function that validates the 'group_type' argument
check_group_type <- function(group_type) {
  if (!is.character(group_type) || length(group_type) != 1) {
    cli::cli_abort(
      c("Invalid {.arg group_type} argument.", "i" = "The {.arg group_type} argument must be a character vector of length one."),
      call = get_call()
    )
  }
  
  if (!(group_type %in% c("pattern", "variable"))) {
    cli::cli_abort(
      c("Invalid {.arg group_type} argument.", "i" = "The {.arg group_type} argument must be one of: {.val pattern} or {.val variable}."),
      call = get_call()
    )
  }
  
  return(list(valid = TRUE, group_type = group_type))
}


# Function that checks whether a string, 'x', contains any
# characters that are not letters, digits, periods, or
# underscores; returns TRUE/FALSE
string_has_invalid_chars <- function(x) {
  grepl(pattern = "[^a-zA-Z0-9._]", x = x)
}
