# Test mean_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    mean_tbl(
      data = NULL,
      var_stem = "belong_belong"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
    mean_tbl(
      data = data.frame(),
      var_stem = "belong_belong"
    ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'var_stem' argument and no 'cols' found", {
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = c("belong_beoln", "identities")
    ),
    "Invalid 'var_stem' argument. 'var_stem' must be a character vector of length one."
  )

  expect_error(
    mean_tbl(
      data = depressive,
      var_stem = "belong_beoln"
    ),
    "No columns were found with the variable stem: belong_beoln"
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {
  observed <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )

  expected <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      mean = c(3.87, 3.97),
      sd = c(0.980, 1.016),
      min = c(1,1),
      max = c(5,5),
      nobs = c(270, 270)
    )

  expect_equal(observed, expected)
})



test_that("Expected output with 'ignore' values and ignore_stem_case", {
  
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_BELONG",
      ignore_stem_case = FALSE
    ),
    "No columns were found with the variable stem: belong_BELONG"
  )
  
  observed <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_BELONG",
      ignore_stem_case = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      mean = c(3.87, 3.97),
      sd = c(0.980, 1.016),
      min = c(1,1),
      max = c(5,5),
      nobs = c(270, 270)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with variable labels", {
  observed <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      var_labels = c(belong_belongStem_w1 = "I feel like I belong in STEM",
                     belong_belongStem_w2 = "I feel like I belong in STEM")
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      variable_label = "I feel like I belong in STEM",
      mean = c(3.87, 3.97),
      sd = c(0.980, 1.016),
      min = c(1,1),
      max = c(5,5),
      nobs = c(270, 270)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with differnt 'only' types", {
  observed1 <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = c("mean", "sd")) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      mean = c(3.87, 3.97),
      sd = c(0.980, 1.016),
    )
  
  observed2 <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = c("nobs", "min"))
  
  expected2 <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      min = 1,
      nobs = 270
    )
  
  observed3 <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = c("mean", "max")) |>
    dplyr::mutate(
      dplyr::across(
        .cols = "mean",
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected3 <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2"),
      mean = c(3.87, 3.97),
      max = 5
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})
