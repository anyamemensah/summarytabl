# Test select_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    select_tbl(
      data = NULL,
      var_stem = "dep"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
    select_tbl(
      data = data.frame(),
      var_stem = "dep"
    ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'var_stem' argument and No 'cols' found", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = c("dep", "gender")
    ),
    "Invalid 'var_stem' argument. 'var_stem' must be a character vector of length one."
  )

  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "depress"
    ),
    "No columns were found with the variable stem: depress."
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {

  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count"
    ) |> head()

  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )


  expect_equal(observed, expected)
})



