# Function that removes unrequested 'only' columns
drop_only_cols <- function(data, only, only_type) {
  if (all(only %in% only_type)) {
    to_remove <- setdiff(only_type,only)
    if (length(to_remove)) {
      data <- data |>
        dplyr::select(-dplyr::starts_with(to_remove))
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
only_type <- function(table_type = c("cat", "mean", "select")) {

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

