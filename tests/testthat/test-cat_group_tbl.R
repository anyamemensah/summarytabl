# Test cat_group_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    cat_group_tbl(
      data = NULL,
      row_var = "race",
      col_var = "gender"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
    cat_group_tbl(
      data = data.frame(),
      row_var = "race",
      col_var = "gender"
    ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'row_var' or 'col_var' argument", {
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = c("race", "bthwht"),
      col_var = "gender"
    ),
    "Invalid 'row_var' argument. 'row_var' must be a character vector of length one."
  )

  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = c("gender", "race")
    ),
    "Invalid 'col_var' argument. 'col_var' must be a character vector of length one."
  )
})


test_that("Invalid 'na.rm.row_var' or 'na.rm.col_var' argument", {
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = "TU",
      na.rm.col_var = TRUE
    ),
    "Invalid 'na.rm.row_var' argument. 'na.rm.row_var' must be a logical vector of length one."
  )

  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = "ME"
    ),
    "Invalid 'na.rm.col_var' argument. 'na.rm.col_var' must be a logical vector of length one."
  )
})


test_that("Invalid 'only' argument", {
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {

  observed <-
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(count, percent),
        .fns = ~ round(., digits = 3)
      )
    )

  expected <-
    tibble::tibble(
      bthwht = rep(0:1, times = 2),
      gender = rep(0:1, each = 2),
      count = c(1340, 123, 1409, 104),
      percent = c(0.916, 0.084, 0.931, 0.069)
    )

  expect_equal(observed, expected)
})



