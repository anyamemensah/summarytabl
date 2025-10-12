#### Helper functions used to validate main function (i.e., cat.*_tbl,
## mean.*_tbl, and select.*_tbl) arguments
# Helper function to validate 'cat_tbl' arguments
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


# Helper function to validate 'cat_group_tbl' arguments
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


# Helper function to validate 'select_tbl' arguments
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
        ignore_stem_case = args$ignore_stem_case,
        table_type = args$table_type
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


# Helper function to validate 'select_group_tbl' arguments
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
        var_stem_labels = args$var_labels,
        table_type = args$table_type
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
    group_name = function(args)
      args$group_name,
    escape_group = function(args)
      args$escape_group,
    ignore_group_case = function(args)
      args$ignore_group_case,
    remove_group_non_alnum = function(args)
      args$remove_group_non_alnum
  )
  
  lapply(checks, function(chk) chk(args))
}


# Helper function to validate 'mean_tbl' arguments
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
        ignore_stem_case = args$ignore_stem_case,
        table_type = args$table_type
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


# Helper function to validate 'mean_group_tbl' arguments
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
        var_stem_labels = args$var_labels,
        table_type = args$table_type
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
    group_name = function(args)
      args$group_name,
    escape_group = function(args)
      args$escape_group,
    ignore_group_case = function(args)
      args$ignore_group_case,
    remove_group_non_alnum = function(args)
      args$remove_group_non_alnum
  )
  
  lapply(checks, function(chk) chk(args))
}

