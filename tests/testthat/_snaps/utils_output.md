# check valid variable data types

    Code
      check_data_types(data = nlsy, cols = c("race"), table_type = "mean",
      allowed_type = "valid_var_types", arg_name = "var_stem")
    Condition
      Error in `check_data_types()`:
      ! Invalid `var_stem` argument.
      i The `var_stem` argument has returned a column containing an unsupported data type: "race".
      i Allowed types: <numeric/POSIXt/POSIXct/POSIXlt/difftime/Date>.

---

    Code
      check_data_types(data = test_data, cols = c("var_1", "var.2"), table_type = "mean",
      allowed_type = "valid_var_types", arg_name = "var_stem")
    Condition
      Error in `check_data_types()`:
      ! Invalid `var_stem` argument.
      i One or more columns returned using the `var_stem` argument contain an unsupported data type: "var_1" and "var.2".
      i Allowed types: <numeric/POSIXt/POSIXct/POSIXlt/difftime/Date>.

# extract group information

    Code
      extract_group_info(group = "_\\d", group_type = "pattern", ignore_group_case = FALSE,
        regex_group = FALSE, cols = c("var_1", "var_4", "var_10"), data = test_data,
        table_type = "select", allowed_type = "valid_grp_types")
    Condition
      Error in `extract_group_info()`:
      ! Invalid `group` argument.
      i The value provided to the `group` argument did not produce a unique or expected set of column names in `data`. Please check for typos, spelling mistakes, or invalid characters.

# check returned columns

    Code
      check_returned_cols(character(0), "this", "stem")
    Condition
      Error in `check_returned_cols()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "this".

---

    Code
      check_returned_cols(c("Meep", "beep beep"), "this", "name")
    Condition
      Error in `check_returned_cols()`:
      ! One or more columns returned using the variable stem "this" contain invalid characters: "beep beep"
      i Column names must only include letters, digits, periods (.), or underscores (_).

# get valid columns

    Code
      get_valid_cols(data = depressive, var_stem = "bloop", var_input = "name",
        regex_stem = FALSE, ignore_stem_case = FALSE, find_exact_match = TRUE)
    Condition
      Error in `check_returned_cols()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following names: "bloop".

---

    Code
      get_valid_cols(data = depressive, var_stem = "bloop", var_input = "stem",
        regex_stem = FALSE, ignore_stem_case = FALSE, find_exact_match = TRUE)
    Condition
      Error in `check_returned_cols()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "bloop".

# generate key for recoding values

    Code
      generate_tbl_key(values_from = 1:2, values_to = c("one", "two", "three"))
    Condition
      Error in `generate_tbl_key()`:
      ! Error constructing key to create variable labels column.
      i `values_from` and `values_to` must be the same length.

# Warning: override pivot wider

    Code
      override_pivot(tabl = sample_tbl, var_col = "variable", values_col = "values",
        allow_override = FALSE)
    Condition
      Warning in `override_pivot()`:
      Some variables have different values, so pivoting to the "wider" format has been disabled. The table will be displayed in the "longer" format instead. To override this behavior and force pivoting, set `force_pivot = TRUE`.
    Output
      [1] FALSE

