# Failure: 'data' argument

    Code
      select_tbl(data = NULL, var_stem = "dep")
    Condition
      Error in `select_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      select_tbl(data = data.frame(), var_stem = "dep")
    Condition
      Error in `select_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: Invalid 'var_stem' argument

    Code
      select_tbl(data = depressive, var_stem = c("dep", "gender"))
    Condition
      Error in `select_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "gender".

---

    Code
      select_tbl(data = ex_mean_dat, var_stem = "var")
    Condition
      Error in `select_tbl()`:
      ! One or more columns returned using the variable stem "var" contain invalid characters: "var 2"
      i Column names must only include letters, digits, periods (.), or underscores (_).

---

    Code
      select_tbl(data = stem_social_psych, var_stem = "BELONG_belong",
        ignore_stem_case = FALSE)
    Condition
      Error in `select_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "BELONG_belong".

# Failure: Invalid 'var_input' argument

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", var_input = NULL)
    Condition
      Error in `select_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be a character vector of length one.

---

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", var_input = "var_name")
    Condition
      Error in `select_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be one of: "stem" or "name".

# Failure: Invalid 'only' argument

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", only = character(
        0))
    Condition
      Error in `select_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", only = NA)
    Condition
      Error in `select_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "count" and "percent".

# Failure: Invalid 'na_removal' argument

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", na_removal = NULL)
    Condition
      Error in `select_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be a character vector of length one.

---

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", na_removal = "side-ways")
    Condition
      Error in `select_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be one of "listwise" or "pairwise".

# Failure: Invalid 'pivot' argument

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", pivot = NULL)
    Condition
      Error in `select_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be a character vector of length one.

---

    Code
      select_tbl(data = stem_social_psych, var_stem = "belong_belong", pivot = "side_ways")
    Condition
      Error in `select_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be one of: "wider" or "longer".

# Warning: override pivot wider

    Code
      select_tbl(data = social_psy_data, var_stem = c("belong", "identity"),
      na_removal = "pairwise", only = "count", pivot = "wider", ignore = list(belong = c(
        1, 2, 3), identity = c(3, 4, 5)))
    Condition
      Warning in `select_tbl()`:
      Some variables have different values, so pivoting to the "wider" format has been disabled. The table will be displayed in the "longer" format instead. To override this behavior and force pivoting, set `force_pivot = TRUE`.
    Output
      # A tibble: 14 x 3
         variable   values count
         <chr>       <dbl> <int>
       1 belong_1        4  3574
       2 belong_1        5  3283
       3 belong_2        4  2114
       4 belong_2        5   556
       5 belong_3        4  4137
       6 belong_3        5  2509
       7 identity_1      1   245
       8 identity_1      2   843
       9 identity_2      1   465
      10 identity_2      2  1025
      11 identity_3      1    51
      12 identity_3      2   133
      13 identity_4      1  1474
      14 identity_4      2  3333

