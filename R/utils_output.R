#### Utility functions used to generate function outputs
# Function that validates variable data types (also used in inputs)
check_data_types <- function(data, cols, table_type, allowed_type, arg_name) {
  valid_types <- return_data_types(table_type)[[allowed_type]]
  
  dtypes <- sapply(cols, function(x) get_data_type(data[[x]]))
  
  if (length(dtypes) == 1 && !(dtypes %in% valid_types)) {
    stop(sprintf("The '%s' argument has an unsupported data type. Allowed types: %s.", 
                 arg_name, paste(valid_types, collapse = ", ")))
  }
  
  invalid_cols <- names(dtypes)[!unlist(dtypes) %in% valid_types]
  
  if (length(invalid_cols) > 0) {
    stop(sprintf(
      paste("One or more columns returned using the variable stem '%s'",
            "contain an unsupported data type: %s.",
            "Allowed types: %s."),
      arg_name,
      paste(invalid_cols, collapse = ", "),
      paste(valid_types, collapse = ", ")
    ))
  }
  
  return(list(valid = TRUE, dtype = dtypes))
}


# Function that extracts and retrieves relevant information 
# from the 'group' argument
extract_group_info <- function(group,
                               group_type,
                               ignore_group_case,
                               regex_group,
                               cols,
                               data,
                               table_type,
                               allowed_type) {
  grp_dtype <- NULL
  
  if (group_type == "pattern") {
    cols_no_group <- unique(gsub(pattern = group, 
                                 replacement = "", x = cols,
                                 ignore.case = ignore_group_case, 
                                 perl = regex_group))
    
    if (!is.character(cols_no_group) || length(cols_no_group) != 1 || 
        all(cols_no_group %in% cols)) {
      stop(
        paste("Invalid 'group' argument. The value provided to 'group' did",
              "not produce a unique or expected set of column names in 'data'.",
              "Please check for typos, spelling mistakes, or invalid characters.")
      )
    }
    
  } else {
    grp_dtype <- 
      check_data_types(data = data,
                       cols = group, 
                       table_type = table_type,
                       allowed_type = allowed_type,
                       arg_name = "group")
    
  }
  
  return(list(group = group, grp_dtype = grp_dtype))
}


# Function that validates the setup of functions requiring 
# 'var_stem', and returns the corresponding columns and 
# column information (e.g., data types, variable stem 
# mapping)
extract_var_stem_info <- function(data,
                                  var_stem,
                                  var_label,
                                  var_input,
                                  valid_var_type,
                                  var_stem_labels,
                                  regex_stem,
                                  ignore_stem_case,
                                  table_type) {
  find_exact_match <- var_input == "name"
  
  cols <- get_valid_cols(data,
                         var_stem,
                         var_input,
                         regex_stem,
                         ignore_stem_case,
                         find_exact_match)
  
  dtypes <- 
    check_data_types(data = data,
                     cols = cols, 
                     table_type = table_type,
                     allowed_type = valid_var_type,
                     arg_name = var_label)
  
  var_labels <- check_var_labels(cols, var_stem_labels)
  var_stem_map <- check_stem_mapping(cols, var_stem, var_input)
  
  return(
    list(
      valid = TRUE, 
      cols = cols, 
      dtypes = dtypes, 
      var_labels = var_labels,
      var_stem_map = var_stem_map    
    )
  )
}



# Function that validates the setup of functions requiring both 
# 'var_stem' and 'group' arguments, and returns the corresponding 
# columns, column information (e.g., data types, variable stem 
# mapping), and group variable information
extract_group_var_stem_info <- function(data,
                                        var_stem,
                                        var_label,
                                        var_input,
                                        valid_var_type,
                                        regex_stem,
                                        ignore_stem_case,
                                        group,
                                        group_type,
                                        valid_grp_type,
                                        regex_group,
                                        ignore_group_case,
                                        var_stem_labels,
                                        table_type) { 
  find_exact_match <- var_input == "name"
  
  cols <- get_valid_cols(data,
                         var_stem,
                         var_input,
                         regex_stem,
                         ignore_stem_case,
                         find_exact_match)
  
  group_info <- 
    extract_group_info(group,
                       group_type,
                       ignore_group_case,
                       regex_group,
                       cols,
                       data,
                       table_type,
                       valid_grp_type)
  
  col_dtypes <- 
    check_data_types(data = data,
                     cols = cols, 
                     table_type = table_type,
                     allowed_type = valid_var_type,
                     arg_name = var_label)
  
  var_labels <- check_var_labels(cols, var_stem_labels)
  
  var_stem_map <- check_stem_mapping(cols, var_stem, var_input)
  
  return(
    list(
      valid = TRUE,
      var_stem = var_stem,
      cols = cols,
      group = group_info$group,
      group_type = group_type,
      var_labels = var_labels,
      var_stem_map = var_stem_map,
      dtypes = c(col_dtypes, if (group_type != "pattern") group_info$grp_dtype)
    )
  )
}


# Function that checks the structure of the 'ignore' argument
check_ignore_struct <- function(ignore, table_type, group_func) {
  if (!is.null(ignore) && !(is.vector(ignore) || is.list(ignore))) { 
    stop("'ignore' must be a vector, list, or NULL.") 
  }
  
  named_required <- !(table_type == "cat" && group_func == FALSE)
  
  if (!is.null(ignore) && length(ignore) > 0) { 
    if (named_required) { 
      if (!is.null(names(ignore))) return(ignore) 
    } else { 
      return(list(ignore = ignore)) 
    } 
  } 
  
  return(list(ignore = NULL))
}


# Function that validates returned columns
check_returned_cols <- function(x, label, var_input) {
  message <- 
    if (var_input == "name") {
      sprintf("No matching columns found for the following names: %s.",
              paste0(label, collapse = ", "))
    } else {
      sprintf("No matching columns found for the following variable stems: %s.",
              paste0(label, collapse = ", "))
    }
  
  if (!is.character(x) || length(x) == 0) {
    stop(message)
  }
  
  col_has_invalid_chars <- sapply(x, string_has_invalid_chars)
  invalid_names <- names(which(col_has_invalid_chars))
  
  if (length(invalid_names) > 0) {
    stop(
      sprintf(
        paste("One or more columns returned using the variable stem '%s'",
              "contain invalid characters: %s. Column names must only include",
              "letters, digits, periods (.), or underscores (_)."), 
        label, paste0(invalid_names, collapse = ", "))
    )
  }
}

# Function that retrieves validated columns
get_valid_cols <- function(data,
                           var_stem,
                           var_input,
                           regex_stem,
                           ignore_stem_case,
                           find_exact_match) {
  cols <- 
    find_columns(data = data,
                 var_stem = var_stem,
                 perl = regex_stem,
                 ignore.case = ignore_stem_case,
                 exact = find_exact_match)
  
  check_returned_cols(cols, var_stem, var_input)
  
  return(cols)
}


# Function for returning the 'var_stem_map' object used to 
# create 'ignore_map'
check_stem_mapping <- function(cols, var_stem, var_input) {
  var_stem_map <- 
    if (var_input == "name") {
      NULL
    } else {
      stats::setNames(cols, rep(var_stem, length(cols)))
    }
  
  return(var_stem_map)
}


# Function for returning variable/column labels
check_var_labels <- function(cols, var_stem_labels) {
  var_labels <- 
    check_named_vctr(x = var_stem_labels[names(var_stem_labels) %in% cols],
                     names = cols,
                     default = NULL)
  
  return(var_labels)
}


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
  
  return(data)
}


# Function that extracts and returns a specific substring 
# (i.e., 'group_flag') from a vector of column names
extract_group_flags <- function(cols,
                                pattern,
                                ignore.case = FALSE,
                                perl = perl,
                                remove_non_alum = FALSE) {
  group_flag <- 
    regmatches(
      x = cols,
      m = regexpr(
        pattern = pattern,
        text = cols,
        ignore.case = ignore.case,
        perl = perl
      )
    )
  
  if (remove_non_alum) {
    group_flag <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = group_flag)
  }
  
  return(group_flag)
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
        for (fn in full_names) { ignore_map[[fn]] <- ignore } 
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


# Function that searches for and returns the names of columns in 
# 'data' that start with a specific variable stem (i.e., 'var_stem')
find_columns <- function(data,
                         var_stem,
                         perl = FALSE,
                         ignore.case = FALSE,
                         exact = FALSE) {
  
  pattern <- 
    if (exact) {
      paste0("^", var_stem, "$")
    } else {
      paste0("^", var_stem)
    }
  
  cols_found <- grep(pattern = pattern,
                     ignore.case = ignore.case,
                     x = colnames(data),
                     perl = perl,
                     value = TRUE)
  
  return(cols_found)
}


# Function that generates a list of two-sided formulas that map 
# values from one set to another
generate_tbl_key <- function(values_from, values_to, string = TRUE) {
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


# Function that returns a set of available summary statistics 
# (descriptive types) for a specified table type
only_type <- function(table_type) {
  
  if (!(table_type %in% c("cat", "mean", "select"))) {
    stop("'table_type' should be one of cat, mean, select.")
  }
  
  switch(
    table_type,
    cat =  c("count", "percent"),
    mean = c("mean", "sd", "min", "max", "nobs"),
    select = c("count", "percent")
  )
}


# Function for pivoting summary table to the 'wider' format 
pivot_tbl_wider <- function(data,
                            id_cols,
                            names_from,
                            names_glue,
                            values_from) {
  wider_tbl <-
    data |>
    tidyr::pivot_wider (
      id_cols = dplyr::all_of(id_cols),
      names_from = dplyr::all_of(names_from),
      names_glue = names_glue,
      values_from = dplyr::all_of(values_from)
    )
  
  return(wider_tbl)
}


# Generalized function to pluck results from 'checks' list
pluck_results <- function(list_obj,
                          check_name,
                          check_output,
                          use.names = TRUE,
                          unlist = TRUE,
                          strip_inner_names = FALSE,
                          repeat_outer_names = TRUE) {
  collapsed_obj <- lapply(list_obj, function(x) {
    result <- x[[check_name]][[check_output]]
    if (strip_inner_names) result <- unname(result)
    result
  })
  
  if (unlist) {
    if (repeat_outer_names) {
      collapsed_obj <- setNames(
        unlist(collapsed_obj, use.names = FALSE),
        rep(names(collapsed_obj), lengths(collapsed_obj))
      )
    } else {
      collapsed_obj <- unlist(collapsed_obj, use.names = use.names)
    }
  }
  
  return(collapsed_obj)
}

# Function to pluck 'cols' from 'checks' list
pluck_cols <- function(list_obj, check_name, check_output) {
  cols_obj <- 
    pluck_results(
      list_obj = list_obj,
      check_name = check_name,
      check_output = check_output,
      use.names = FALSE,
      unlist = TRUE,
      strip_inner_names = FALSE,
      repeat_outer_names = TRUE
    )
  
  return(cols_obj)
}

# Function to pluck 'stem_map' from 'checks' list
pluck_stem_map <- function(list_obj, check_name, check_output) {
  stem_map_list <- 
    pluck_results(
      list_obj = list_obj,
      check_name = check_name,
      check_output = check_output,
      use.names = FALSE,
      unlist = FALSE,
      strip_inner_names = FALSE,
      repeat_outer_names = TRUE
    )
  
  flat_stem_map <- 
    if (!all(sapply(stem_map_list, is.null))) {
      unlist(unname(stem_map_list))
    } else {
      NULL
    }
  
  return(flat_stem_map)
}


# Function to pluck 'var_labels' from 'checks' list
pluck_var_labels <- function(list_obj, check_name, check_output) {
  labels_list <-
    pluck_results(
      list_obj = list_obj,
      check_name = check_name,
      check_output = check_output,
      use.names = TRUE,
      unlist = FALSE,
      strip_inner_names = FALSE,
      repeat_outer_names = FALSE
    )
  
  flat_labels <- unlist(unname(labels_list))
  
  return(flat_labels)
}


# Function to replace 'ignore' values with NAs
replace_with_na <- function(x, ignore_vals) {
  if (is.factor(x)) {
    original_levels <- levels(x)
    x <- as.character(x)
    x[x %in% ignore_vals] <- NA
    x <- factor(x, levels = original_levels)
  } else {
    x[x %in% ignore_vals] <- NA
  }
  return(x)
}


# Function that returns valid variable or grouping variable 
# data types based on a specific 'table_type'
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


# Function to override the 'pivot' argument when at least one variable 
# in 'tabl' contains different values. This function applies only to 
# the select_* functions.
override_pivot <- function(tabl, var_col, values_col, allow_overide) {
  value_list <- split(tabl[[values_col]], tabl[[var_col]])
  value_list <- lapply(value_list, function(x) sort(unique(x)))
  
  first_levels <- value_list[[1]]
  i <- 2
  override <- TRUE
  
  if (allow_overide) {return(override)}
  
  while (i <= length(value_list)) {
    if (!identical(first_levels, value_list[[i]])) {
      override <- FALSE
      warning(paste(
        "Some variables have different values, so pivoting",
        "to the 'wider' format has been disabled. The table",
        "will be displayed in the 'long' format instead.",
        "To override this behavior and force pivoting, set `force_pivot = TRUE`.")
        )
      break
    }
    i <- i + 1
  }
  
  return(override)
}

