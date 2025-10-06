# Test mean_group_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    mean_group_tbl(
      data = NULL,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern"
    ),
    "The 'data' argument is not a data frame."
  )

  expect_error(
    mean_group_tbl(
      data = data.frame(),
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern"
    ),
    "The 'data' argument is empty."
  )
})


test_that("Invalid 'var_stem' argument and no columns found", {
  expect_error(
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = c("belong_beoln", "identities"),
      group = "_w\\d",
      group_type = "pattern"
    ),
    "Invalid 'var_stem' argument. 'var_stem' must be a character vector of length one."
  )

  expect_error(
    mean_group_tbl(
      data = depressive,
      var_stem = "belong_beoln",
      group = "_w\\d",
      group_type = "pattern"
    ),
    "No columns were found with the variable stem: belong_beoln"
  )
})

test_that("Invalid 'only' argument", {
  expect_error(
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: mean, sd, min, max, nobs."
  )
})

test_that("Invalid 'group'/'group_type' argument", {
  set.seed(0204)
  symptoms_data <-
    data.frame(
      symptoms_t1 = sample(c(0:10, -999), replace = TRUE, size = 50),
      symptoms_t2 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50),
      symptoms_t3 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50),
      group = sample(c("placebo","treatment"), replace = TRUE, size = 50)
    )
  
  expect_error(
    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "variable",
                   group_name = "time_point",
                   ignore = c(symptoms = -999),
                   only = c("mean", "sd")),
    paste("Invalid 'group' argument. The value provided to 'group' is", 
          "not a column in 'data'. Check for typos, spelling mistakes,",
          "or invalid characters.")
  )
  
  expect_error(
    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "group",
                   group_type = "pattern",
                   group_name = "study_group",
                   ignore = c(symptoms = -999),
                   only = c("mean", "sd")),
    paste("Invalid 'group' argument. The value provided to 'group' did",
          "not produce a unique or expected set of column names in 'data'.",
          "Please check for typos, spelling mistakes, or invalid characters.")
  )

})


test_that("Expected output for group pattern with specified group name", {
  observed <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      group_name = "wave"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )

  expected <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      wave = c("w1", "w2"),
      mean = c(2.542, 2.513),
      sd = c(1.204, 1.220),
      min = c(1,1),
      max = c(5,5),
      nobs = c(271, 271)
    )

  expect_equal(observed, expected)
})



test_that("Expected output for group pattern with specified group name and variable labels", {
  observed <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      group_name = "wave",
      var_labels = c(
        belong_outsiderStem_w1 = "I feel like an outsider in STEM",
        belong_outsiderStem_w2 = "I feel like an outsider in STEM"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      variable_label = c(
        "I feel like an outsider in STEM", "I feel like an outsider in STEM"
      ),
      wave = c("w1", "w2"),
      mean = c(2.542, 2.513),
      sd = c(1.204, 1.220),
      min = c(1,1),
      max = c(5,5),
      nobs = c(271, 271)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output for group pattern with specified group name and 'ignore' values", {
  observed <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      group_name = "wave",
      ignore = c(belong_outsiderStem = 5),
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      wave = c("w1", "w2"),
      mean = c(2.382, 2.293),
      sd = c(1.065, 1.040),
      min = 1,
      max = 4,
      nobs = 246
    )
  
  expect_equal(observed, expected)
})

test_that("Expected output for group variable with specified group name", {
  observed <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "is_male",
      group_type = "variable",
      group_name = "gender_male"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      variable = rep(c("belong_outsiderStem_w1", "belong_outsiderStem_w2"), each = 2),
      gender_male = rep(0:1, times = 2),
      mean = c(2.961, 2.137, 2.744, 2.281),
      sd = c(1.100, 1.162, 1.188, 1.204),
      min = 1,
      max = 5,
      nobs = c(129, 139, 129, 139)
    )
  
  expect_equal(observed, expected)
})



test_that("Expected output for group pattern and remove_group_non_alnum (TRUE/FALSE) witih specified group name", {
  observed1 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      group_name = "wave",
      remove_group_non_alnum = FALSE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      wave = c("_w1", "_w2"),
      mean = c(2.542, 2.513),
      sd = c(1.204, 1.220),
      min = 1,
      max = 5,
      nobs = 271
    )
  
  observed2 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_outsiderStem",
      group = "_w\\d",
      group_type = "pattern",
      group_name = "wave",
      remove_group_non_alnum = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected2 <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      wave = c("w1", "w2"),
      mean = c(2.542, 2.513),
      sd = c(1.204, 1.220),
      min = 1,
      max = 5,
      nobs = 271
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("Error and expected output with ignore_stem_case", {
  expect_error(
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_OUTSIDERStem",
      ignore_stem_case = FALSE,
      group = "_w\\d",
      group_type = "pattern"
    ),
    "No columns were found with the variable stem: belong_OUTSIDERStem."
  )
  
  
  observed1 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_OUTSIDERStem",
      ignore_stem_case = TRUE,
      group = "_w\\d",
      group_type = "pattern"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = c("belong_outsiderStem_w1", "belong_outsiderStem_w2"),
      group = c("w1", "w2"),
      mean = c(2.542, 2.513),
      sd = c(1.204, 1.220),
      min = 1,
      max = 5,
      nobs = 271
    )

  expect_equal(observed1, expected1)
})


test_that("Error and expected output with ignore_group_case", {
  expect_error(
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belongStem",
      group = "IS_MALE",
      group_type = "variable",
      ignore_group_case = FALSE
    ),
    paste("Invalid 'group' argument. The value provided to 'group' is", 
          "not a column in 'data'. Check for typos, spelling mistakes,",
          "or invalid characters.")
  )
  
  observed1 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belongStem",
      group = "IS_MALE",
      group_type = "variable",
      ignore_group_case = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("mean", "sd")),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = rep(c("belong_belongStem_w1", "belong_belongStem_w2"), each = 2),
      is_male = rep(0:1, times = 2),
      mean = c(3.620, 4.101, 3.822, 4.094),
      sd = c(1.025, 0.890, 1.004, 1.017),
      min = 1,
      max = 5,
      nobs = c(129, 138, 129, 138)
    )
  
  expect_equal(observed1, expected1)
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belongStem",
      group = "is_male",
      only = c("mean", "min")
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = "mean",
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected1 <-
    tibble::tibble(
      variable = rep(c("belong_belongStem_w1", "belong_belongStem_w2"), each = 2),
      is_male = rep(0:1, times = 2),
      mean = c(3.620, 4.101, 3.822, 4.094),
      min = 1
    )
  
  observed2 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belongStem",
      group = "is_male",
      only = c("sd", "max")
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = "sd",
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected2 <-
    tibble::tibble(
      variable = rep(c("belong_belongStem_w1", "belong_belongStem_w2"), each = 2),
      is_male = rep(0:1, times = 2),
      sd = c(1.025, 0.890, 1.004, 1.017),
      max = 5,
    )

  observed3 <-
    mean_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belongStem",
      group = "is_male",
      only = c("mean", "nobs")
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(mean),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected3 <-
    tibble::tibble(
      variable = rep(c("belong_belongStem_w1", "belong_belongStem_w2"), each = 2),
      is_male = rep(0:1, times = 2),
      mean = c(3.620, 4.101, 3.822, 4.094),
      nobs = c(129, 138, 129, 138)
    ) 
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})

