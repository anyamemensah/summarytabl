# Failure: 'data' argument

    Code
      cat_group_tbl(data = NULL, row_var = "race", col_var = "gender")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must be a <data.frame> or <tibble>.

---

    Code
      cat_group_tbl(data = data.frame(), row_var = "race", col_var = "gender")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `data` argument.
      i The `data` argument must have at least one row and one column.

# Failure: 'row_var' or 'col_var' argument

    Code
      cat_group_tbl(data = nlsy, row_var = "raced", col_var = "gender")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `row_var` argument.
      i The `row_var` argument, "raced", is not a column in `data`.

---

    Code
      cat_group_tbl(data = nlsy, row_var = c("race", "bthwht"), col_var = "gender")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `row_var` argument.
      i The `row_var` argument must be a character vector of length one.

---

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gendered")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `col_var` argument.
      i The `col_var` argument, "gendered", is not a column in `data`.

---

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = c("gender", "race"))
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `col_var` argument.
      i The `col_var` argument must be a character vector of length one.

# Failure: 'na.rm' argument

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = "TU", na.rm.col_var = TRUE)
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `na.rm.row_var` argument.
      i The `na.rm.row_var` argument must be a logical vector of length one.

---

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = TRUE, na.rm.col_var = "ME")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `na.rm.col_var` argument.
      i The `na.rm.col_var` argument must be a logical vector of length one.

# Failure: 'only' argument

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = TRUE, na.rm.col_var = TRUE, only = character(0))
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be a character vector of length at least one.

---

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = TRUE, na.rm.col_var = TRUE, only = NA)
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `only` argument.
      i The `only` argument must be one or more of: "count" and "percent".

# Failure: 'pivot' argument

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = TRUE, na.rm.col_var = TRUE, only = NULL, pivot = 123)
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be a character vector of length one.

---

    Code
      cat_group_tbl(data = nlsy, row_var = "bthwht", col_var = "gender",
        na.rm.row_var = TRUE, na.rm.col_var = TRUE, only = NULL, pivot = "Angle")
    Condition
      Error in `cat_group_tbl()`:
      ! Invalid `pivot` argument.
      i The `pivot` argument must be one of: "wider" or "longer".

