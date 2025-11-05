# check 'data' argument

    Code
      check_df(NA)
    Condition
      Error in `check_df()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      check_df(tibble::tibble())
    Condition
      Error in `check_df()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# check logical argument

    Code
      check_logical(x = "THIS", label = "this_var")
    Condition
      Error in `check_logical()`:
      ! Invalid `this_var` argument.
      i The `this_var` argument must be a logical vector of length one.

---

    Code
      check_logical(x = NA, label = "NA_var")
    Condition
      Error in `check_logical()`:
      ! Invalid `NA_var` argument.
      i The `NA_var` argument must be one of: TRUE or FALSE.

# check 'margins' argument

    Code
      check_margins(NA)
    Condition
      Error in `check_margins()`:
      ! Invalid `margins` argument.
      i The `margins` argument must be a character vector of length one.

---

    Code
      check_margins("diagonal")
    Condition
      Error in `check_margins()`:
      ! Invalid `margins` argument.
      i The `margins` argument must be one of: "rows", "columns", or "all".

# check 'na.rm' argument

    Code
      check_na.rm(na.rm = "this_na", var_label = "row_var")
    Condition
      Error in `check_na.rm()`:
      ! Invalid `row_var` argument.
      i The `row_var` argument must be a logical vector of length one.

---

    Code
      check_na.rm(na.rm = NA, var_label = "row_var")
    Condition
      Error in `check_na.rm()`:
      ! Invalid `row_var` argument.
      i The `row_var` argument must be one of: TRUE or FALSE.

# check 'na_removal' argument

    Code
      check_na_removal(na_removal = NA)
    Condition
      Error in `check_na_removal()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be a character vector of length one.

---

    Code
      check_na_removal(na_removal = "this_na_remove")
    Condition
      Error in `check_na_removal()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be one of "listwise" or "pairwise".

# check 'only' argument

    Code
      check_only(only = character(0), table_type = "cat")
    Condition
      Error in `check_only()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      check_only(only = c("count", "per"), table_type = "cat")
    Condition
      Error in `check_only()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "count" and "percent".

# check 'pivot' argument

    Code
      check_pivot(pivot = NA)
    Condition
      Error in `check_pivot()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be a character vector of length one.

---

    Code
      check_pivot(pivot = "diagonal")
    Condition
      Error in `check_pivot()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be one of: "wider" or "longer".

# check 'group_name' argument

    Code
      check_group_name(group_name = "THIS NAME")
    Condition
      Error in `check_group_name()`:
      ! Invalid `group_name` argument.
      i The `group_name` argument contains invalid characters.
      i Column names must only include letters, digits, periods (.), or underscores (_).

---

    Code
      check_group_name(group_name = "That$NAme")
    Condition
      Error in `check_group_name()`:
      ! Invalid `group_name` argument.
      i The `group_name` argument contains invalid characters.
      i Column names must only include letters, digits, periods (.), or underscores (_).

# check 'var_stem' argument

    Code
      check_var_stem(var_stem = character(0))
    Condition
      Error in `check_var_stem()`:
      ! Invalid `var_stem` argument.
      i The `var_stem` argument must be a character vector of at least length one.

---

    Code
      check_var_stem(var_stem = c("this_var", "THAT VAR"))
    Condition
      Error in `check_var_stem()`:
      ! Invalid `var_stem` argument.
      i The `var_stem` argument contains elements with invalid characters: "THAT VAR".
      i Column names must only include letters, digits, periods (.), or underscores (_).

# check an individual variable argument (i.e., 'var', 'row_var', 'col_var')

    Code
      check_var(var_name = character(0), "var_label_here", nlsy)
    Condition
      Error in `check_var()`:
      ! Invalid `var_label_here` argument.
      i The `var_label_here` argument must be a character vector of length one.

---

    Code
      check_var("not_a_var", "var_label_here", nlsy)
    Condition
      Error in `check_var()`:
      ! Invalid `var_label_here` argument.
      i The `var_label_here` argument, "not_a_var", is not a column in `data`.

---

    Code
      check_var("invalid col name", "invalid_var_arg", invalid_data)
    Condition
      Error in `check_var()`:
      ! Invalid `invalid_var_arg` argument.
      i The `invalid_var_arg` argument contains invalid characters.
      i Column names must only include letters, digits, periods (.), or underscores (_).

# check 'var_input' argument

    Code
      check_var_input(var_input = character(0))
    Condition
      Error in `check_var_input()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be a character vector of length one.

---

    Code
      check_var_input("stem_name")
    Condition
      Error in `check_var_input()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be one of: "stem" or "name".

# check 'group' argument

    Code
      check_group_var(group_var = NULL)
    Condition
      Error in `check_group_var()`:
      ! Invalid `group` argument.
      i The `group` argument must be a character vector of length one.

---

    Code
      check_group_var(group_var = "This VAR", group_type = "variable")
    Condition
      Error in `check_group_var()`:
      ! Invalid `group` argument.
      i The `group` argument contains invalid characters.
      i Column names must only include letters, digits, periods (.), or underscores (_).

---

    Code
      check_group_var(group_var = "group_var", group_type = "variable", col_names = names(
        data_test), ignore_case = FALSE, use_regex = FALSE)
    Condition
      Error in `check_group_var()`:
      ! Invalid `group` argument.
      i Multiple columns in `data` matched the `group` argument: "group_var" and "group_var".

---

    Code
      check_group_var(group_var = "boop", group_type = "variable", col_names = names(
        data_test), ignore_case = FALSE, use_regex = FALSE)
    Condition
      Error in `check_group_var()`:
      ! Invalid `group` argument.
      i The value provided to `group`, "boop", is not a column in `data`. Check for typos, spelling mistakes, or invalid characters.

# check 'group_type' argument

    Code
      check_group_type(group_type = character(0))
    Condition
      Error in `check_group_type()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be a character vector of length one.

---

    Code
      check_group_type(group_type = "shape")
    Condition
      Error in `check_group_type()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be one of: "pattern" or "variable".

