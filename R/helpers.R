##### Helper functions used to validate main function (i.e., cat.*_tbl,
## mean.*_tbl, and select.*_tbl) arguments
## Helper function to validate 'cat_tbl' arguments
check_cat_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    var = function(args)
      check_var(var_name = args$var_name,
                var_label = args$var_label,
                data = args$data,
                .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(table_type = args$table_type),
    na.rm = function(args)
      check_na.rm(na.rm = args$na.rm, 
                  var_label = args$label_na_rm, 
                  .main_env = args$.main_env),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func, 
                          .main_env = args$.main_env),
    dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$var_name,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label,
                       .main_env = args$.main_env),
    env = function(args)
      args$.main_env
  )
  
  lapply(checks, function(chk) chk(args))
}


## Helper function to validate 'cat_group_tbl' arguments
check_cat_group_args <- function(args) {
  checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    row_var = function(args)
      check_var(var_name = args$row_var,
                var_label = args$var_label_row,
                data = args$data,
                .main_env = args$.main_env),
    col_var = function(args)
      check_var(var_name = args$col_var,
                var_label = args$var_label_col, 
                data = args$data,
                .main_env = args$.main_env),
    margins = function(args)
      check_margins(margins = args$margins, 
                    .main_env = args$.main_env),
    na_row = function(args)
      check_na.rm(na.rm = args$na_rm_row_var, 
                  var_label = args$label_na_rm_row,
                  .main_env = args$.main_env),
    na_col = function(args)
      check_na.rm(na.rm = args$na_rm_col_var, 
                  var_label = args$label_na_rm_col,
                  .main_env = args$.main_env),
    pivot = function(args)
      check_pivot(pivot = args$pivot, 
                  .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func,
                          .main_env = args$.main_env),
    row_dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$row_var,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label_row,
                       .main_env = args$.main_env),
    col_dtype = function(args)
      check_data_types(data = args$data,
                       cols = args$col_var,
                       table_type = args$table_type,
                       allowed_type = args$variable_type,
                       arg_name = args$var_label_col,
                       .main_env = args$.main_env),
    env = function(args)
      args$.main_env
  )
  
  lapply(checks, function(chk) chk(args))
}


## Helper function to validate 'select_tbl' arguments
check_select_args <- function(args) {
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem, 
                     .main_env = args$.main_env),
    var_input = function(args)
      check_var_input(var_input = args$var_input, 
                      .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(args$table_type),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal, 
                       .main_env = args$.main_env),
    pivot = function(args)
      check_pivot(pivot = args$pivot, .main_env = args$.main_env),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem",
                    .main_env = args$.main_env),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case",
                    .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func,
                          .main_env = args$.main_env),
    force_pivot = function(args)
      check_logical(x = args$force_pivot, 
                    label = "force_pivot",
                    .main_env = args$.main_env),
    env = function(args)
      args$.main_env
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
        table_type = args$table_type,
        .main_env = args$.main_env
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
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    na_removal = shared_results$na_rm$na_removal,
    pivot = shared_results$pivot$pivot,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    force_pivot = shared_results$force_pivot$x,
    table_type = shared_results$table_type$table_type,
    env = shared_results$env
  )
}


## Helper function to validate 'select_group_tbl' arguments
check_select_group_args <- function(args) {
  # shared checks
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem, 
                     .main_env = args$.main_env),
    var_input = function(args)
      check_var_input(var_input = args$var_input, 
                      .main_env = args$.main_env),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal, 
                       .main_env = args$.main_env),
    pivot = function(args)
      check_pivot(pivot = args$pivot, 
                  .main_env = args$.main_env),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem",
                    .main_env = args$.main_env),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case",
                    .main_env = args$.main_env),
    regex_group = function(args)
      check_logical(x = args$regex_group, 
                    label = "regex_group",
                    .main_env = args$.main_env),
    ignore_group_case = function(args)
      check_logical(x = args$ignore_group_case, 
                    label = "ignore_group_case",
                    .main_env = args$.main_env),
    remove_group_non_alnum = function(args)
      check_logical(x = args$remove_group_non_alnum, 
                    label = "remove_group_non_alnum",
                    .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    margins = function(args)
      check_margins(margins = args$margins, 
                    .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type, 
                          group_func = args$group_func,
                          .main_env = args$.main_env),
    force_pivot = function(args)
      check_logical(x = args$force_pivot, 
                    label = "force_pivot",
                    .main_env = args$.main_env),
    group_type = function(args) 
      check_group_type(group_type = args$group_type, 
                       .main_env = args$.main_env),
    group_var = function(args) 
      check_group_var(group_var = args$group_var, 
                      group_type = args$group_type,
                      col_names = colnames(args$data), 
                      ignore_case = args$ignore_group_case,
                      use_regex = args$regex_group, 
                      .main_env = args$.main_env),
    group_name = function(args)
      check_group_name(group_name = args$group_name, 
                       .main_env = args$.main_env),
    env = function(args)
      args$.main_env
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
        table_type = args$table_type,
        .main_env = args$.main_env
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
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    group_var = shared_results$group_var$group_var,
    group_name = shared_results$group_name$group_name,
    group_type = shared_results$group_type$group_type,
    margins = shared_results$margins$margins,
    regex_group = shared_results$regex_group$x,
    ignore_group_case = shared_results$ignore_group_case$x,
    remove_group_non_alnum = shared_results$remove_group_non_alnum$x,
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    na_removal = shared_results$na_rm$na_removal,
    pivot = shared_results$pivot$pivot,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    force_pivot = shared_results$force_pivot$x,
    table_type = shared_results$table_type$table_type,
    env = shared_results$env
  )
}


## Helper function to validate 'mean_tbl' arguments
check_mean_args <- function(args) {
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem, 
                     .main_env = args$.main_env),
    var_input = function(args)
      check_var_input(var_input = args$var_input, 
                      .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(args$table_type),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal, 
                       .main_env = args$.main_env),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem", 
                    .main_env = args$.main_env),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case",
                    .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type,
                          group_func = args$group_func,
                          .main_env = args$.main_env),
    env = function(args)
      args$.main_env
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
        table_type = args$table_type,
        .main_env = args$.main_env
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
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    na_removal = shared_results$na_rm$na_removal,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    table_type = shared_results$table_type$table_type,
    env = shared_results$env
  )
}


## Helper function to validate 'mean_group_tbl' arguments
check_mean_group_args <- function(args) {
  # shared checks
  shared_checks <- list(
    data = function(args)
      check_df(data = args$data, 
               .main_env = args$.main_env),
    var_stem = function(args)
      check_var_stem(var_stem = args$var_stem, 
                     .main_env = args$.main_env),
    var_input = function(args)
      check_var_input(var_input = args$var_input, 
                      .main_env = args$.main_env),
    na_rm = function(args)
      check_na_removal(na_removal = args$na_removal, 
                       .main_env = args$.main_env),
    regex_stem = function(args)
      check_logical(x = args$regex_stem, 
                    label = "regex_stem", 
                    .main_env = args$.main_env),
    ignore_stem_case = function(args)
      check_logical(x = args$ignore_stem_case, 
                    label = "ignore_stem_case",
                    .main_env = args$.main_env),
    regex_group = function(args)
      check_logical(x = args$regex_group, 
                    label = "regex_group",
                    .main_env = args$.main_env),
    ignore_group_case = function(args)
      check_logical(x = args$ignore_group_case, 
                    label = "ignore_group_case",
                    .main_env = args$.main_env),
    remove_group_non_alnum = function(args)
      check_logical(x = args$remove_group_non_alnum, 
                    label = "remove_group_non_alnum",
                    .main_env = args$.main_env),
    table_type = function(args)
      check_table_type(args$table_type),
    only = function(args)
      check_only(only = args$only, 
                 table_type = args$table_type,
                 .main_env = args$.main_env),
    ignore = function(args)
      check_ignore_struct(ignore = args$ignore, 
                          table_type = args$table_type, 
                          group_func = args$group_func,
                          .main_env = args$.main_env),
    group_type = function(args) 
      check_group_type(group_type = args$group_type, 
                       .main_env = args$.main_env),
    group_var = function(args) 
      check_group_var(group_var = args$group_var, 
                      group_type = args$group_type,
                      col_names = colnames(args$data), 
                      ignore_case = args$ignore_group_case,
                      use_regex = args$regex_group, 
                      .main_env = args$.main_env),
    group_name = function(args)
      check_group_name(group_name = args$group_name, 
                       .main_env = args$.main_env),
    env = function(args)
      args$.main_env
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
        table_type = args$table_type,
        .main_env = args$.main_env
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
    cols = pluck_cols(col_info_results, "var_stem", "cols"),
    col_labels = pluck_var_labels(col_info_results, "var_stem", "var_labels"),
    group_var = shared_results$group_var$group_var,
    group_name = shared_results$group_name$group_name,
    group_type = shared_results$group_type$group_type,
    regex_group = shared_results$regex_group$x,
    ignore_group_case = shared_results$ignore_group_case$x,
    remove_group_non_alnum = shared_results$remove_group_non_alnum$x,
    var_stem_map = pluck_stem_map(col_info_results, "var_stem", "var_stem_map"),
    na_removal = shared_results$na_rm$na_removal,
    only = shared_results$only$only,
    ignore = shared_results$ignore,
    table_type = shared_results$table_type$table_type,
    env = shared_results$env
  )
}
