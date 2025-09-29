# Test utils functions

test_that("drop 'only' columns", {
  cat_1_observed <- 
    cat_tbl(social_psy_data, var = "gender") |>
    drop_only_cols(only = "percent", only_type = only_type("cat")) |>
    colnames()
  cat_1_expected <- c("gender", "percent")
  
  cat_2_observed <- 
    cat_group_tbl(nlsy, row_var = "gender", col_var = "race") |>
    drop_only_cols(only = "count", only_type = only_type("cat")) |>
    colnames()
  cat_2_expected <- c("gender", "race", "count")
  
  select_1_observed <- 
    select_tbl(social_psy_data, var_stem = "identity") |>
    drop_only_cols(only = "count", only_type = only_type("cat")) |>
    colnames()
  select_1_expected <- c("variable", "values", "count")
  
  select_2_observed <- 
    select_group_tbl(social_psy_data, var_stem = "identity", 
                     group = "gender", group_type = "variable") |>
    drop_only_cols(only = c("count", "percent"), only_type = only_type("cat")) |>
    colnames()
  select_2_expected <- c("variable", "gender", "values", "count","percent")
  
  mean_1_observed <- 
    mean_tbl(social_psy_data, var_stem = "belong_") |>
    drop_only_cols(only = "mean", only_type = only_type("mean")) |>
    colnames()
  mean_1_expected <- c("variable", "mean")
  
  mean_2_observed <- 
    mean_group_tbl(social_psy_data, var_stem = "belong_", group = "gender") |>
    drop_only_cols(only = "mean", only_type = only_type("mean")) |>
    colnames()
  mean_2_expected <- c("variable", "gender", "mean")
  
  expect_equal(cat_1_observed, cat_1_expected)
  expect_equal(cat_2_observed, cat_2_expected)
  expect_equal(select_1_observed, select_1_expected)
  expect_equal(select_2_observed, select_2_expected)
  expect_equal(mean_1_observed, mean_1_expected)
  expect_equal(mean_2_observed, mean_2_expected)
})


test_that("tbl_key creation", {
  key_observed <- 
    tbl_key(values_from = 1:3, values_to = c("one", "two", "three"))
  
  key_expected <- 
    purrr::map2(.x = paste0(1:3),
                .y = c("one", "two", "three"),
                .f = ~ rlang::new_formula(.x, .y))
  
  expect_equal(key_observed, key_expected, ignore_attr = TRUE)
})

test_that("tbl_key error, values_from is a different length that values_to", {
  expect_error(
    tbl_key(values_from = 1:2, 
            values_to = c("one", "two", "three")),
    "'values_from' is not the same length as 'values_to'."
  )
})


test_that("find_columns", {
  observed1 <- 
    find_columns(data = stem_social_psych, var_stem = "belong_belong")
  expected1 <- c("belong_belongStem_w1", "belong_belongStem_w2")
  
  observed2 <- 
    find_columns(data = social_psy_data, var_stem = "identity")
  expected2 <- c("identity_1", "identity_2", "identity_3", "identity_4")
  
  observed3 <- 
    find_columns(data = social_psy_data, var_stem = "NANA")
  expected3 <- character(0)
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})

test_that("escape_punct", {
  observed1 <- escape_punct("a")
  expected1 <- "a"
  
  observed2 <- escape_punct(".b")
  expected2 <- "\\.b"
  
  observed3 <- escape_punct("_c")
  expected3 <- "_c"
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})


test_that("only_type", {
  observed1 <- only_type("cat")
  expected1 <- c("count", "percent")
  
  observed2 <- only_type("mean")
  expected2 <- c("mean", "sd", "min", "max", "nobs")
  
  observed3 <- only_type("select")
  expected3 <- c("count", "percent")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})

test_that("only_type error", {
  expect_error(
    only_type("TEST"),
    "'arg' should be one of cat, mean, select."
  )
})


test_that("extract_group_flags", {
  observed1 <- extract_group_flags(cols = c("test_t1", "test_t2"), 
                                    group_flag = "_t\\d", 
                                    remove_non_alum = TRUE)
  expected1 <- c("t1", "t2")
  
  observed2 <- extract_group_flags(cols = c("new_test_1", "new_test_2"), 
                                    group_flag = "\\d", 
                                    remove_non_alum = TRUE)
  expected2 <- c("1", "2")

  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})
