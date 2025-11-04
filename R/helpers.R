##### Helper functions used to validate main function (i.e., cat.*_tbl,
## mean.*_tbl, and select.*_tbl) arguments
## Helper function to validate 'cat_tbl' arguments
check_cat_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    var = function(args)
      check_var(var_name = args$var_name,
                var_label = args$var_label,
                data = args$data),
    table_type = function(args)
      check_table_type(table_type = args$table_type),
    na.rm = function(args)
      check_na.rm(na.rm = args$na.rm, 
                  var_label = args$label_na_rm),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func),
    dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$var_name,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label)
  )
  
  lapply(checks, function(chk) chk(args))
}


## Helper function to validate 'cat_group_tbl' arguments
check_cat_group_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data),
    row_var = function(args)
      check_var(var_name = args$row_var,
                var_label = args$var_label_row,
                data = args$data),
    col_var = function(args)
      check_var(var_name = args$col_var,
                var_label = args$var_label_col, 
                data = args$data),
    margins = function(args)
      check_margins(margins = args$margins),
    na_row = function(args)
      check_na.rm(na.rm = args$na_rm_row_var, 
                  var_label = args$label_na_rm_row),
    na_col = function(args)
      check_na.rm(na.rm = args$na_rm_col_var, 
                  var_label = args$label_na_rm_col),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func),
    row_dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$row_var,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label_row),
    col_dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$col_var,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label_col)
  )
  
  lapply(checks, function(chk) chk(args))
}


## Helper function to validate 'select_tbl' arguments
check_select_args <- function(args) {
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem),
    var_input = function(args)
      check_var_input(var_input = args$var_input),
    table_type = function(args)
      check_table_type(args$table_type),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem"),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case"),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func),
    force_pivot = function(args)
      check_logical(x = args$force_pivot, 
                    label = "force_pivot")
  )
  
  shared_results <- lapply(shared_checks, function(chk) chk(args))
  
  col_info_checks <- list(
    var_stem = function(args)
      extract_var_stem_info(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_input = args$var_input,
        valid_var_type = args$valid_var_type,
        var_stem_labels = args$var_labels,
        regex_stem = args$regex_stem,
        ignore_stem_case = args$ignore_stem_case,
        table_type = args$table_type
      )
  )
  
  col_info_results <- 
    setNames(
      lapply(args$var_stem, function(stem) {
        args$var_stem <- stem
        lapply(col_info_checks, function(chk) chk(args))
      }),
      args$var_stem
    )
  
  list(
    df = shared_results$data$df,
    var_stem = names(col_info_results),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    na_removal = shared_results$na_rm$na_removal,
    pivot = shared_results$pivot$pivot,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    force_pivot = shared_results$force_pivot$x,
    table_type = shared_results$table_type$table_type
  )
}


## Helper function to validate 'select_group_tbl' arguments
check_select_group_args <- function(args) {
  # shared checks
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem),
    var_input = function(args)
      check_var_input(var_input = args$var_input),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    pivot = function(args)
      check_pivot(pivot = args$pivot),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem"),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case"),
    regex_group = function(args)
      check_logical(x = args$regex_group, 
                    label = "regex_group"),
    ignore_group_case = function(args)
      check_logical(x = args$ignore_group_case, 
                    label = "ignore_group_case"),
    remove_group_non_alnum = function(args)
      check_logical(x = args$remove_group_non_alnum, 
                    label = "remove_group_non_alnum"),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    margins = function(args)
      check_margins(margins = args$margins),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type, 
                          group_func = args$group_func),
    force_pivot = function(args)
      check_logical(x = args$force_pivot, 
                    label = "force_pivot"),
    group_type = function(args) 
      check_group_type(group_type = args$group_type),
    group_var = function(args) 
      check_group_var(group_var = args$group_var, 
                      group_type = args$group_type,
                      col_names = colnames(args$data), 
                      ignore_case = args$ignore_group_case,
                      use_regex = args$regex_group),
    group_name = function(args)
      check_group_name(group_name = args$group_name)
  )
  
  shared_results <- lapply(shared_checks, function(chk) chk(args))
  
  # update group_var with 'cleaned' name
  args$group_var <- shared_results$group_var$group_var
  
  # var_stem-specific checks
  col_info_checks <- list(
    var_stem = function(args)
      extract_group_var_stem_info(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_input = args$var_input,
        valid_var_type = args$valid_var_type,
        regex_stem = args$regex_stem,
        ignore_stem_case = args$ignore_stem_case,
        group = args$group_var, 
        group_type = args$group_type, 
        valid_grp_type = args$valid_grp_type,
        regex_group = args$regex_group, 
        ignore_group_case = args$ignore_group_case, 
        var_stem_labels = args$var_labels,
        table_type = args$table_type
      )
  )
  
  col_info_results <- 
    setNames(
      lapply(args$var_stem, function(stem) {
        args$var_stem <- stem
        lapply(col_info_checks, function(chk) chk(args))
      }),
      args$var_stem
    )
  
  # information to return
  list(
    df = shared_results$data$df,
    var_stem = names(col_info_results),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    group_var = shared_results$group_var$group_var,
    group_name = shared_results$group_name$group_name,
    group_type = shared_results$group_type$group_type,
    margins = shared_results$margins$margins,
    regex_group = shared_results$regex_group$x,
    ignore_group_case = shared_results$ignore_group_case$x,
    remove_group_non_alnum = shared_results$remove_group_non_alnum$x,
    na_removal = shared_results$na_rm$na_removal,
    pivot = shared_results$pivot$pivot,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    force_pivot = shared_results$force_pivot$x,
    table_type = shared_results$table_type$table_type
  )
}


## Helper function to validate 'mean_tbl' arguments
check_mean_args <- function(args) {
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem),
    var_input = function(args)
      check_var_input(var_input = args$var_input),
    table_type = function(args)
      check_table_type(args$table_type),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem"),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case"),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func)
  )
  
  shared_results <- lapply(shared_checks, function(chk) chk(args))
  
  col_info_checks <- list(
    var_stem = function(args)
      extract_var_stem_info(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_input = args$var_input,
        valid_var_type = args$valid_var_type,
        var_stem_labels = args$var_labels,
        regex_stem = args$regex_stem,
        ignore_stem_case = args$ignore_stem_case,
        table_type = args$table_type
      )
  )
  
  col_info_results <- 
    setNames(
      lapply(args$var_stem, function(stem) {
        args$var_stem <- stem
        lapply(col_info_checks, function(chk) chk(args))
      }),
      args$var_stem
    )
  
  list(
    df = shared_results$data$df,
    var_stem = names(col_info_results),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    na_removal = shared_results$na_rm$na_removal,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    table_type = shared_results$table_type$table_type
  )
}


## Helper function to validate 'mean_group_tbl' arguments
check_mean_group_args <- function(args) {
  # shared checks
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem),
    var_input = function(args)
      check_var_input(var_input = args$var_input),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem"),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case"),
    regex_group = function(args)
      check_logical(x = args$regex_group, 
                    label = "regex_group"),
    ignore_group_case = function(args)
      check_logical(x = args$ignore_group_case, 
                    label = "ignore_group_case"),
    remove_group_non_alnum = function(args)
      check_logical(x = args$remove_group_non_alnum, 
                    label = "remove_group_non_alnum"),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type, 
                          group_func = args$group_func),
    group_type = function(args) 
      check_group_type(group_type = args$group_type),
    group_var = function(args) 
      check_group_var(group_var = args$group_var, 
                      group_type = args$group_type,
                      col_names = colnames(args$data), 
                      ignore_case = args$ignore_group_case,
                      use_regex = args$regex_group),
    group_name = function(args)
      check_group_name(group_name = args$group_name)
  )
  
  shared_results <- lapply(shared_checks, function(chk) chk(args))
  
  # update group_var with 'cleaned' name
  args$group_var <- shared_results$group_var$group_var
  
  # var_stem-specific checks
  col_info_checks <- list(
    var_stem = function(args)
      extract_group_var_stem_info(
        data = args$data,
        var_stem = args$var_stem,
        var_label = args$var_label,
        var_input = args$var_input,
        valid_var_type = args$valid_var_type,
        regex_stem = args$regex_stem,
        ignore_stem_case = args$ignore_stem_case,
        group = args$group_var, 
        group_type = args$group_type, 
        valid_grp_type = args$valid_grp_type,
        regex_group = args$regex_group, 
        ignore_group_case = args$ignore_group_case, 
        var_stem_labels = args$var_labels,
        table_type = args$table_type
      )
  )
  
  col_info_results <- 
    setNames(
      lapply(args$var_stem, function(stem) {
        args$var_stem <- stem
        lapply(col_info_checks, function(chk) chk(args))
      }),
      args$var_stem
    )
  
  # information to return
  list(
    df = shared_results$data$df,
    var_stem = names(col_info_results),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    group_var = shared_results$group_var$group_var,
    group_name = shared_results$group_name$group_name,
    group_type = shared_results$group_type$group_type,
    regex_group = shared_results$regex_group$x,
    ignore_group_case = shared_results$ignore_group_case$x,
    remove_group_non_alnum = shared_results$remove_group_non_alnum$x,
    na_removal = shared_results$na_rm$na_removal,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    table_type = shared_results$table_type$table_type
  )
}
