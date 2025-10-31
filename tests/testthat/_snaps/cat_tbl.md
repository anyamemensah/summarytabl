# Failure: 'data' argument

    Code
      cat_tbl(data = NULL, var = "age")
    Condition
      Error in `cat_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      cat_tbl(data = data.frame(), var = "age")
    Condition
      Error in `cat_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: 'var' argument

    Code
      cat_tbl(data = nlsy, var = c("race", "gender"))
    Condition
      Error in `cat_tbl()`:
      ! Invalid `var` argument.
      i The `var` argument must be a character vector of length one.

---

    Code
      cat_tbl(data = nlsy, var = c("raced"))
    Condition
      Error in `cat_tbl()`:
      ! Invalid `var` argument.
      i The `var` argument, "raced", is not a column in `data`.

# Failure: 'na.rm' argument

    Code
      cat_tbl(data = nlsy, var = "race", na.rm = "TU")
    Condition
      Error in `cat_tbl()`:
      ! Invalid `na.rm` argument.
      i The `na.rm` argument must be a logical vector of length one.

---

    Code
      cat_tbl(data = nlsy, var = "race", na.rm = NA)
    Condition
      Error in `cat_tbl()`:
      ! Invalid `na.rm` argument.
      i The `na.rm` argument must be one of: TRUE or FALSE.

# Failure: 'only' argument

    Code
      cat_tbl(data = nlsy, var = "race", na.rm = TRUE, only = character(0))
    Condition
      Error in `cat_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      cat_tbl(data = nlsy, var = "race", na.rm = TRUE, only = NA)
    Condition
      Error in `cat_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "count" and "percent".

