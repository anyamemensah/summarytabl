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


test_that("Invalid 'var_stem' argument and No 'cols' found", {
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
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
})


test_that("Expected output", {

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
        .cols = c(mean, sd),
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



