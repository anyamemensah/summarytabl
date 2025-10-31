# Failure: 'data' argument

    Code
      mean_group_tbl(data = NULL, var_stem = "belong_outsiderStem", group = "_w\\d",
        group_type = "pattern")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      mean_group_tbl(data = data.frame(), var_stem = "belong_outsiderStem", group = "_w\\d",
      group_type = "pattern")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: Invalid 'var_stem' argument

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = c("belong_beoln",
        "identities"), group = "_w\\d", group_type = "pattern")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `var_stem` argument.
      i No matching columns found for the following variable stems: "belong_beoln".

---

    Code
      mean_group_tbl(data = ex_mean_dat, var_stem = "var", group = "\\d$",
        group_type = "pattern")
    Condition
      Error in `mean_group_tbl()`:
      ! One or more columns returned using the variable stem "var" contain invalid characters: "var 2"
      i Column names must only include letters, digits, periods (.), or underscores (_).

# Failure: Invalid 'var_input' argument

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = "pattern", var_input = NULL)
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be a character vector of length one.

---

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = "pattern", var_input = "var_name")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `var_input` argument.
      i The `var_input` argument must be one of: "stem" or "name".

# Failure: Invalid 'group' argument

    Code
      mean_group_tbl(data = sample_mean_data, var_stem = "var", group = NULL)
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `group` argument.
      i The `group` argument must be a character vector of length one.

---

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "URMS",
        )
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `group` argument.
      i The value provided to `group`, "URMS", is not a column in `data`. Check for typos, spelling mistakes or invalid characters.

---

    Code
      mean_group_tbl(data = sample_mean_data, var_stem = "var", group = "group")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `group` argument.
      i Multiple columns in `data` matched the `group` argument: "group" and "group".

# Failure: Invalid 'group_type' argument

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = NULL)
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be a character vector of length one.

---

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_belong", group = "\\d$",
        group_type = "patterning")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `group_type` argument.
      i The `group_type` argument must be one of: "pattern" or "variable".

# Failure: Invalid 'only' argument

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_outsiderStem",
        group = "_w\\d", group_type = "pattern", only = character(0))
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_outsiderStem",
        group = "_w\\d", group_type = "pattern", only = NA)
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "mean", "sd", "min", "max", and "nobs".

# Failure: Invalid 'na_removal' argument

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_outsiderStem",
        group = "_w\\d", group_type = "pattern", na_removal = NULL)
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be a character vector of length one.

---

    Code
      mean_group_tbl(data = stem_social_psych, var_stem = "belong_outsiderStem",
        group = "_w\\d", group_type = "pattern", na_removal = "side-wise")
    Condition
      Error in `mean_group_tbl()`:
      ! Invalid `na_removal` argument.
      i The `na_removal` argument must be one of "listwise" or "pairwise".

