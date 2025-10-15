# Test select_group_tbl

test_that("Invalid 'data' argument", {
  expect_error(
    select_group_tbl(
      data = NULL,
      var_stem = "dep",
      group = "_\\d"
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


test_that("Invalid 'na_removal' argument", {
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      na_removal = 123
    ),
    "Invalid 'na_removal' argument. 'na_removal' must be a character vector of length one."
  )
  
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      na_removal = "sidewise"
    ),
    "Invalid 'na_removal' argument. 'na_removal' must be one of 'listwise', 'pairwise'."
  )
})

test_that("Invalid 'pivot' argument", {
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      pivot = 1234
    ),
    "Invalid 'pivot' argument. 'pivot' must be a character vector of length one."
  )
  
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      pivot = "sidewise"
    ),
    "Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'."
  )
})


test_that("Invalid 'only' argument", {
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      only = character(0)
    ),
    "Invalid 'only' argument. 'only' must be a character vector of length at least one."
  )
  
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      only = NA
    ),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
  )
})

test_that("Invalid 'group'/'group_type' argument", {
  expect_error(
    select_group_tbl(data = depressive, 
                   var_stem = "dep",
                   group = "_\\d",
                   group_type = "variable",
                   group_name = "item_number"),
    paste("Invalid 'group' argument. The value provided to 'group' is", 
          "not a column in 'data'. Check for typos, spelling mistakes,",
          "or invalid characters.")
  )
  
  expect_error(
    select_group_tbl(data = depressive, 
                     var_stem = "dep",
                     group = "sex",
                     group_type = "pattern"),
    paste("Invalid 'group' argument. The value provided to 'group' did",
          "not produce a unique or expected set of column names in 'data'.",
          "Please check for typos, spelling mistakes, or invalid characters."))
  
})


test_that("Expected output longer format", {
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      margins = "rows"
    ) |>
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))

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


test_that("Expected output wider format", {
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      pivot = "wider",
      only = "count"
    )
  observed <- observed[1:3,]
  
  expected <-
    tibble::tibble(
      variable = rep("dep_1", times = 3),
      values = 1:3L,
      count_sex_1 = c(55, 325, 440),
      count_sex_2 = c(54, 364, 369)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("values", "count_sex_1", "count_sex_2")),
      .fns = as.integer)
      )
  
  expect_equal(observed, expected)
})


test_that("Expected output wider format with variable labels", {
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      pivot = "wider",
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
    )[1:3,]
  
  expected <-
    tibble::tibble(
      variable = rep("dep_1", times = 3),
      variable_label = "how often child feels sad and blue",
      values = 1:3L,
      count_sex_1 = c(55, 325, 440),
      count_sex_2 = c(54, 364, 369)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("values", "count_sex_1", "count_sex_2")),
      .fns = as.integer)
      )
  
  expect_equal(observed, expected)
})


test_that("Expected output wider for with 'ignore' values", {
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      group_type = "variable",
      pivot = "wider",
      only = "count",
      ignore = c(sex = 2, dep = 1),
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
      variable = rep(paste0("dep_", 1:3), each = 2),
      variable_label = rep(c("how often child feels sad and blue",
                             "how often child feels nervous, tense, or on edge",
                             "how often child feels happy"), each  = 2),
      values = rep(2:3L, times = 3),
      count_sex_1 = c(41, 26, 42, 25, 59, 8),
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("values", "count_sex_1")),
      .fns = as.integer
      ))
  
  expect_equal(observed, expected)
})



test_that("Expected output with remove_group_non_alnum", {
  observed1 <-
    select_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      group = "_w\\d",
      group_type = "pattern",
      remove_group_non_alnum = FALSE
    ) |>
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
    
  expected1 <-
    tibble::tibble(
      variable = c(rep("belong_belongStem_w1", times = 5), 
                   "belong_belongStem_w2"),
      group = c(rep("_w1", times = 5), "_w2"),
      values = c(1:5L, 1L),
      count = c(5, 20, 59, 107, 79, 11),
      percent = c(0.019, 0.074, 0.219, 0.396, 0.293, 0.041)
    )
  
  observed2 <-
    select_group_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      group = "_w\\d",
      group_type = "pattern",
      remove_group_non_alnum = TRUE
    ) |>
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      variable = c(rep("belong_belongStem_w1", times = 5), 
                   "belong_belongStem_w2"),
      group = c(rep("w1", times = 5), "w2"),
      values = c(1:5L, 1L),
      count = c(5, 20, 59, 107, 79, 11),
      percent = c(0.019, 0.074, 0.219, 0.396, 0.293, 0.041)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("Error and expected output with ignore_stem_case", {
  
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "DEP",
      ignore_stem_case = FALSE,
      group = "sex",
      group_type = "variable",
      pivot = "wider",
      only = "count"
    ),
    "No columns were found with the variable stem: DEP."
  )
  
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "DEP",
      ignore_stem_case = TRUE,
      group = "sex",
      group_type = "variable",
      pivot = "wider",
      only = "count"
    )[1:2,]
  
  expected <-
    tibble::tibble(
      variable = rep("dep_1", times = 2),
      values = 1:2,
      count_sex_1 = c(55, 325),
      count_sex_2 = c(54,364)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("values", "count_sex_1","count_sex_2")),
      .fns = as.integer)
      )
  
  expect_equal(observed, expected)
})



test_that("Error and expected output with ignore_group_case", {
  
  expect_error(
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "SEX",
      group_type = "variable",
      ignore_group_case = FALSE,
      pivot = "wider",
      only = "count"
    ),
    paste("Invalid 'group' argument. The value provided to 'group' is", 
          "not a column in 'data'. Check for typos, spelling mistakes,",
          "or invalid characters.")
  )
  
  observed <-
    select_group_tbl(
      data = depressive,
      var_stem = "DEP",
      ignore_stem_case = TRUE,
      group = "SEX",
      group_type = "variable",
      ignore_group_case = TRUE,
      pivot = "wider",
      only = "count"
    )[1:2,]
  
  expected <-
    tibble::tibble(
      variable = rep("dep_1", times = 2),
      values = 1:2,
      count_sex_1 = c(55, 325),
      count_sex_2 = c(54,364)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("values","count_sex_1", "count_sex_2")),
      .fns = as.integer)
      )
  
  expect_equal(observed, expected)
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      only = "count"
    ) |>
    head()
  
  expected1 <-
    tibble::tibble(
      variable = "dep_1",
      sex = rep(1:2, each = 3),
      values = rep(1:3, times = 2),
      count = c(55, 325, 440, 54, 364, 369)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("sex", "values")),
      .fns = as.integer)
      )
  
  observed2 <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      only = "percent",
      margins = "rows"
    ) |>
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  
  expected2 <-
    tibble::tibble(
      variable = "dep_1",
      sex = rep(1:2, times = 3),
      values = rep(1:3, each = 2),
      percent = c(0.505, 0.495, 0.472, 0.528, 0.544, 0.456)
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("sex", "values")),
      .fns = as.integer)
      )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("Expected output with specified group name", {
  observed1 <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "sex",
      only = "count",
      group_name = "gender_identity"
    ) |>
    head()
  
  expected1 <-
    tibble::tibble(
      variable = "dep_1",
      gender_identity = rep(1:2, each = 3),
      values = rep(1:3, times = 2),
      count = c(55, 325, 440, 54, 364, 369)
    ) |>
    dplyr::mutate(values = as.integer(values))
  
  observed2 <-
    select_group_tbl(
      data = depressive,
      var_stem = "dep",
      group = "_\\d",
      group_type = "pattern",
      group_name = "item_suffix",
      only = "percent"
    ) |>
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      variable = rep(c("dep_1","dep_2"), each = 3),
      item_suffix = rep(paste0(1:2), each = 3),
      values = rep(1:3, times = 2),
      percent = c(0.068, 0.429, 0.503, 0.090, 0.464, 0.446)
    ) |>
    dplyr::mutate(values = as.integer(values))
  
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})
