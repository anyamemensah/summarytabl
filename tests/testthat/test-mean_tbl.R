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


test_that("Invalid 'var_stem' argument and No 'cols' found", {
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
        .cols = c(mean, sd),
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



