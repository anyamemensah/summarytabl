# Test select_group_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    select_group_tbl(
      data = NULL,
      var_stem = "dep"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
    select_group_tbl(
      data = data.frame(),
      var_stem = "dep",
      group = "sex",
      group_type = "variable"
    ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'var_stem' argument and No 'cols' found", {
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = c("dep", "gender"),
      group = "sex",
      group_type = "variable"
    ),
    "Invalid 'var_stem' argument. 'var_stem' must be a character vector of length one."
  )

  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "depress",
      group = "sex",
      group_type = "variable"
    ),
    "No columns were found with the variable stem: depress."
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {

  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable"
    ) |>
    head() |>
    dplyr::mutate(
      dplyr::across(
        .cols = percent,
        .fns = ~ round(., digits = 3)
      )
    )


  expected <-
    tibble::tibble(
      variable = rep("dep_1", times = 6),
      sex = rep(1:2, times = 3),
      values = rep(1:3, each = 2),
      count = c(55, 54, 325, 364, 440, 369),
      percent = c(0.505, 0.495, 0.472, 0.528,
                  0.544, 0.456)
    )


  expect_equal(observed, expected)
})

