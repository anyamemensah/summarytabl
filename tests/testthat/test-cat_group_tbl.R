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
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
  )
})


test_that("Invalid 'pivot' argument", {
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = NULL,
      pivot = 123
    ),
    "Invalid 'pivot' argument. 'pivot' must be a character vector of length one."
  )
  
  expect_error(
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = NULL,
      pivot = "Angle"
    ),
    "Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'."
  )
})


test_that("Expected output", {
  observed <-
    cat_group_tbl(
      data = nlsy,
      row_var = "bthwht",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      margins = "columns"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))

  expected <-
    tibble::tibble(
      bthwht = rep(0:1, times = 2),
      gender = rep(0:1, each = 2),
      count = c(1340, 123, 1409, 104),
      percent = c(0.916, 0.084, 0.931, 0.069)
    )

  expect_equal(observed, expected)
})


test_that("Expected output with ignore values", {
  observed <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      ignore = c(race = "Non-Black,Non-Hispanic", gender = 1),
      margins = "columns"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  
  expected <-
    tibble::tibble(
      race = c("Black", "Hispanic"),
      gender = 0,
      count = c(434, 305),
      percent = c(0.587, 0.413)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = "percent",
      margins = "columns"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected1 <-
    tibble::tibble(
      race = rep(c("Black", "Hispanic", "Non-Black,Non-Hispanic"), times = 2),
      gender = rep(0:1, each = 3),
      percent = c(0.297, 0.208, 0.495, 0.287, 0.215, 0.498)
    )
  
  observed2 <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      na.rm.row_var = TRUE,
      na.rm.col_var = TRUE,
      only = "count", 
      margins = "columns"
    )
  
  expected2 <-
    tibble::tibble(
      race = rep(c("Black", "Hispanic", "Non-Black,Non-Hispanic"), times = 2),
      gender = rep(0:1, each = 3),
      count = c(434, 305, 724, 434, 326, 753)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})



test_that("Expected output with different 'margin' types", {
  observed1 <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      only = "percent",
      margins = "columns"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected1 <-
    tibble::tibble(
      race = rep(c("Black", "Hispanic", "Non-Black,Non-Hispanic"), times = 2),
      gender = rep(0:1, each = 3),
      percent = c(0.297, 0.208, 0.495, 0.287, 0.215, 0.498)
    )
  
  observed2 <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      only = "percent",
      margins = "rows"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      race = rep(c("Black", "Hispanic", "Non-Black,Non-Hispanic"), each = 2),
      gender = as.numeric(rep(0:1, times = 3)),
      percent = c(0.500, 0.500, 0.483, 
                0.517, 0.490, 0.510)
    )
  
  observed3 <-
    cat_group_tbl(
      data = nlsy,
      row_var = "race",
      col_var = "gender",
      only = "percent",
      margins = "all"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected3 <-
    tibble::tibble(
      race = rep(c("Black", "Hispanic", "Non-Black,Non-Hispanic"), each = 2),
      gender = as.numeric(rep(0:1, times = 3)),
      percent = c(0.146, 0.146, 0.102, 
                  0.110, 0.243, 0.253)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})



