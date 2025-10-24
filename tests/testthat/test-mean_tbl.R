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


test_that("No matching columns found 'cols' found", {
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = c("belong_beoln", "identities")
    ),
    "No matching columns found for the following variable stems: belong_beoln."
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: 'mean', 'sd', 'min', 'max', 'nobs'."
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


test_that("Expected output with ignore_stem_case", {
  
  expect_error(
    mean_tbl(
      data = stem_social_psych,
      var_stem = "belong_BELONG",
      ignore_stem_case = FALSE
    ),
    "No matching columns found for the following variable stems: belong_BELONG."
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

test_that("Expected output with different 'only' types", {
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
    dplyr::mutate(mean = round(mean, digits = 3))
  
  
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


test_that("Expected output with two variable stems (listwise deletion)", {
  observed1 <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = c("belong_belong", "identity_identityStem"),
      only = c("mean", "sd")) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2", 
                   "identity_identityStem_w1", "identity_identityStem_w2"),
      mean = c(3.87, 3.97, 3.552, 3.607),
      sd = c(0.980, 1.016, 1.095, 1.098),
    )
  
  expect_equal(observed1, expected1)
})


test_that("Expected output with two variable names (listwise deletion)", {
  observed1 <-
    mean_tbl(
      data = social_psy_data,
      var_stem = c("belong_1", "identity_4"),
      var_input = "name",
      only = c("mean", "sd")) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = c("belong_1", "identity_4"),
      mean = c(3.828, 2.716),
      sd = c(1.097, 1.139)
    )
  
  expect_equal(observed1, expected1)
})


test_that("Expected output with 'ignore' values set and multiple stems", {
  observed <-
    mean_tbl(
      data = stem_social_psych,
      var_stem = c("belong_belong", "selfEfficacy_passStemCourses"),
      ignore = c(belong_belong = 1, selfEfficacy_passStemCourses = 5),
      na_removal = "pairwise",
      only = c("min", "max")
    ) 
  
  expected <-
    tibble::tibble(
      variable = c("belong_belongStem_w1", "belong_belongStem_w2",
                   "selfEfficacy_passStemCourses_w1", 
                   "selfEfficacy_passStemCourses_w2"),
      min = c(2,2,1,1),
      max = c(5,5,4,4)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with 'ignore' values set and multiple names", {
  observed <-
    mean_tbl(
      data = social_psy_data,
      var_stem = c("identity_3", "selfEfficacy_5"),
      var_input = "name",
      ignore = c(identity_3 = 5, selfEfficacy_5 = 1),
      na_removal = "pairwise",
      only = c("min", "max")
    ) 
  
  expected <-
    tibble::tibble(
      variable = c("identity_3", "selfEfficacy_5"),
      min = c(1,2),
      max = c(4,5)
    )
  
  expect_equal(observed, expected)
})

