# Failure: 'data' argument

    Code
      select_group_tbl(data = NULL, var_stem = "dep", group = "_\\d")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      select_group_tbl(data = data.frame(), var_stem = "dep", group = "sex",
      group_type = "variable")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: Invalid 'var_stem' argument

    Code
      select_group_tbl(data = depressive, var_stem = NA, group = "sex", group_type = "variable")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `var_stem` argument.
      i The `var_stem` argument must be a character vector of at least length one.

---

    Code
      select_group_tbl(data = depressive, var_stem = c("dep", "gender"), group = "sex",
      group_type = "variable")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "gender".

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "BELONG_belong",
        ignore_stem_case = FALSE, group = "urm", group_type = "variable")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "BELONG_belong".

# Failure: Invalid 'var_input' argument

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong",
        var_input = NULL, group = "urm")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be a character vector of length one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong",
        var_input = "var_name", group = "urm")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be one of: "stem" or "name".

# Failure: Invalid 'group' argument

    Code
      select_group_tbl(data = sample_select_data, var_stem = "var", group = NULL)
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `group` argument.
      i The `group` argument must be a character vector of length one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "URMS",
        )
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `group` argument.
      i The value provided to `group`, "URMS", is not a column in `data`. Check for typos, spelling mistakes, or invalid characters.

---

    Code
      select_group_tbl(data = sample_select_data, var_stem = "var", group = "group")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `group` argument.
      i Multiple columns in `data` matched the `group` argument: "group" and "group".

# Failure: Invalid 'group_type' argument

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = NULL)
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be a character vector of length one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = "patterning")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be one of: "pattern" or "variable".

# Failure: Invalid 'only' argument

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        only = character(0))
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        only = NA)
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "count" and "percent".

# Failure: Invalid 'na_removal' argument

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        na_removal = NULL)
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be a character vector of length one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        na_removal = "side-ways")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be one of "listwise" or "pairwise".

# Failure: Invalid 'pivot' argument

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        pivot = NULL)
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be a character vector of length one.

---

    Code
      select_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "urm",
        pivot = "side_ways")
    Condition
      Error in `select_group_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be one of: "wider" or "longer".

# Warning: override pivot wider

    Code
      select_group_tbl(data = social_psy_data, var_stem = c("belong", "identity"),
      group = "gender", na_removal = "pairwise", only = "count", pivot = "wider",
      ignore = list(belong = c(1, 2, 3), identity = c(3, 4, 5)))
    Condition
      Warning in `select_group_tbl()`:
      Some variables have different values, so pivoting to the "wider" format has been disabled. The table will be displayed in the "longer" format instead. To override this behavior and force pivoting, set `force_pivot = TRUE`.
    Output
      # A tibble: 79 x 4
         variable gender values count
         <chr>     <dbl>  <dbl> <int>
       1 belong_1      1      4  1136
       2 belong_1      1      5   560
       3 belong_1      2      4  2115
       4 belong_1      2      5  2442
       5 belong_1      3      4    18
       6 belong_1      3      5     9
       7 belong_1      4      4     7
       8 belong_1      4      5    11
       9 belong_1      5      4    19
      10 belong_1      5      5    17
      # i 69 more rows

