# Failure: 'data' argument

    Code
      mean_tbl(data = NULL, var_stem = "belong_belong")
    Condition
      Error in `mean_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      mean_tbl(data = data.frame(), var_stem = "belong_belong")
    Condition
      Error in `mean_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: Invalid 'var_stem' argument

    Code
      mean_tbl(data = stem_social_psych, var_stem = c("belong_beoln", "identities"))
    Condition
      Error in `mean_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "belong_beoln".

---

    Code
      mean_tbl(data = ex_mean_dat, var_stem = "var")
    Condition
      Error in `mean_tbl()`:
      ! One or more columns returned using the variable stem "var" contain invalid characters: "var 2"
      i Column names must only include letters, digits, periods (.), or underscores (_).

---

    Code
      mean_tbl(data = stem_social_psych, var_stem = "BELONG_belong",
        ignore_stem_case = FALSE)
    Condition
      Error in `mean_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "BELONG_belong".

# Failure: Invalid 'var_input' argument

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", var_input = NULL)
    Condition
      Error in `mean_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be a character vector of length one.

---

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", var_input = "var_name")
    Condition
      Error in `mean_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be one of: "stem" or "name".

# Failure: Invalid 'only' argument

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", only = character(
        0))
    Condition
      Error in `mean_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", only = NA)
    Condition
      Error in `mean_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "mean", "sd", "min", "max", and "nobs".

# Failure: Invalid 'na_removal' argument

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", na_removal = NULL)
    Condition
      Error in `mean_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be a character vector of length one.

---

    Code
      mean_tbl(data = stem_social_psych, var_stem = "belong_belong", na_removal = "side-ways")
    Condition
      Error in `mean_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be one of "listwise" or "pairwise".

