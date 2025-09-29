# Test cat_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    cat_tbl(
      data = NULL,
      var = "race"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
      cat_tbl(
        data = data.frame(),
        var = "race"
      ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'var' argument", {
  expect_error(
    cat_tbl(
      data = nlsy,
      var = c("race", "gender")
    ),
    "Invalid 'var' argument. 'var' must be a character vector of length one."
  )

  expect_error(
    cat_tbl(
      data = nlsy,
      var = "raced"
    ),
    "The 'var' argument is not a column in 'data'."
  )
})


test_that("Invalid 'na.rm' argument", {
  expect_error(
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = "TU"
    ),
    "Invalid 'na.rm' argument. 'na.rm' must be a logical vector of length one."
  )
})


test_that("Invalid 'only' argument", {
  expect_error(
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {
  observed <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE
      ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c("percent"),
        .fns = ~ round(., digits = 3)
      )
    )

  expected <-
    tibble::tibble(
      race = c("Black", "Hispanic", "Non-Black,Non-Hispanic"),
      count = c(868, 631, 1477),
      percent = c(0.292, 0.212, 0.496)
      )
  
  expect_equal(observed, expected)
})


test_that("Expected output with 'ignore' values", {
  observed <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      ignore = "Hispanic"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c("percent"),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      race = c("Black","Non-Black,Non-Hispanic"),
      count = c(868, 1477),
      percent = c(0.37, 0.63)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      only = "count"
    )
  
  expected1 <-
    tibble::tibble(
      race = c("Black", "Hispanic", "Non-Black,Non-Hispanic"),
      count = c(868, 631, 1477)
    )
  
  observed2 <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      only = "percent"
    ) |> 
    dplyr::mutate(
      dplyr::across(
        .cols = c("percent"),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected2 <-
    tibble::tibble(
      race = c("Black","Hispanic", "Non-Black,Non-Hispanic"),
      percent = c(0.292, 0.212, 0.496)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})



