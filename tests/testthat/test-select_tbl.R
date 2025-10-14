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
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
  )
})


test_that("Invalid 'na_removal' argument", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      na_removal = 123
    ),
    "Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one."
  )
  
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      na_removal = "sidewise"
    ),
    "Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'."
  )
})

test_that("Invalid 'pivot' argument", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      pivot = 1234
    ),
    "Invalid 'pivot' argument. 'pivot' must be a character vector of length one."
  )
  
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      pivot = "sidewise"
    ),
    "Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'."
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
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


test_that("Expected output with variable labels", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count",
      var_labels = c(
        dep_1="how often child feels sad and blue",
        dep_2="how often child feels nervous, tense, or on edge",
        dep_3="how often child feels happy",
        dep_4="how often child feels bored",
        dep_5="how often child feels lonely",
        dep_6="how often child feels tired or worn out",
        dep_7="how often child feels excited about something",
        dep_8="how often child feels too busy to get everything"
      )
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      variable_label = rep(c("how often child feels sad and blue", 
                             "how often child feels nervous, tense, or on edge"),
                           each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with 'ignore' values", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count",
      ignore = 1
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2", "dep_3"), each = 2),
      values = rep(2:3, times = 3),
      count = c(68, 38, 61, 45, 96, 10)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with ignore_stem_case and 'ignore' values", {
  expect_error(
    select_tbl(
      data = depressive,
      var_stem = "DEP",
      pivot = "wider",
      only = "count"
    ),
    "No columns were found with the variable stem: DEP."
  )
  
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "DEP",
      ignore_stem_case = TRUE,
      only = "count",
      ignore = 1
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2", "dep_3"), each = 2),
      values = rep(2:3, times = 3),
      count = c(68, 38, 61, 45, 96, 10)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count"
    ) |> head()
  
  expected1 <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )
  
  observed2 <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "percent"
    ) |> 
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      percent = c(0.068, 0.429, 0.503, 0.09, 0.464, 0.446)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})
