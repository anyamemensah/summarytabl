# Test utils functions

test_that("replace_with_na", {
  set.seed(0815)
  factor_x <- 
    factor(x = sample(c(1:5), size = 10, replace = TRUE),
           levels = c(1:5),
           labels = c("one", "two", "three", "four", "five"))
  chr_x <- nlsy$race[sample(c(1:length(nlsy$race)), size = 10, replace = TRUE)]
  num_x <- nlsy$birthord[sample(c(1:length(nlsy$birthord)), size = 10, replace = TRUE)]
  logical_x <- sample(c(TRUE, FALSE), size = 10, replace = TRUE)
  
  observed1 <- replace_with_na(factor_x, ignore_vals = c("four","five"))
  expected1 <- factor(c("two", NA, NA, "two", "two", "two", "three", "two", NA, "two"),
                      levels = c("one", "two", "three", "four", "five"))
  
  observed2 <- replace_with_na(chr_x, ignore_vals = c("Hispanic"))
  expected2 <- c(NA, "Non-Black,Non-Hispanic", "Non-Black,Non-Hispanic", 
                 "Non-Black,Non-Hispanic", "Non-Black,Non-Hispanic", NA,
                 "Black", "Black", NA, NA)
  
  observed3 <- replace_with_na(logical_x, ignore_vals = c(TRUE))
  expected3 <- c(NA, FALSE, FALSE, FALSE, NA, NA, FALSE, NA, FALSE, NA)
  
  observed3 <- replace_with_na(num_x, ignore_vals = 2)
  expected3 <- c(3, 1, 1, 1, NA, NA, 4, 1, 1, 1)
    
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})

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



test_that("get data type: expected types", {
  set.seed(0721)
  observed1 <- get_data_type(1:4)
  expected1 <- "numeric"
  
  observed2 <- get_data_type(seq.Date(from = as.Date("2023-01-01"), 
                                      to = as.Date("2023-01-10"), 
                                      by = "day"))
  expected2 <- "datetime"
  
  observed3 <- get_data_type(seq(from = as.POSIXlt("2024-01-01 00:00:00"), 
                                by = "15 min", length.out = 5))
  expected3 <- "datetime"
  
  observed4 <- get_data_type(factor(sample(1:4, size = 10, replace = TRUE)))
  expected4 <- "factor"
  
  observed5 <- get_data_type(ordered(sample(1:4, size = 10, replace = TRUE)))
  expected5 <- "factor"
  
  observed6 <- get_data_type(ordered(sample(1:4, size = 10, replace = TRUE)))
  expected6 <- "factor"
  
  observed7 <- get_data_type(sample(c(TRUE, FALSE), size = 10, replace = TRUE))
  expected7 <- "logical"
  
  observed8 <- get_data_type(sample(letters, size = 10, replace = TRUE))
  expected8 <- "character"
  
  observed9 <- get_data_type(as.raw(sample(1:4, size = 10, replace = TRUE)))
  expected9 <- "other"
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
  expect_equal(observed7, expected7)
  expect_equal(observed8, expected8)
  expect_equal(observed9, expected9)
})

test_that("valid data types by table type", {
  observed1 <- return_data_types(table_type = "cat")$valid_var_types
  expected1 <- c("haven_labelled", "factor", "character", "logical", "datetime", "numeric")
  
  observed2 <- return_data_types(table_type = "mean")$valid_var_types
  expected2 <- c("numeric", "datetime")
  
  observed3 <- return_data_types(table_type = "select")$valid_var_types
  expected3 <-  c("haven_labelled", "factor", "character", "logical", "numeric")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})

test_that("convert labelled vector to character vector", {
  set.seed(0721)
  data <- data.frame(
    gender = haven::labelled(sample(c("M", "F", "X", "N/A"), size = 50, replace = TRUE), 
                             labels = c(Male = "M", Female = "F", Refused = "X", 
                                        "Not applicable" = "N/A")),
    race = haven::labelled(sample(1:4, size = 50, replace = TRUE), 
                           labels = c(black = 1, white = 2, asian = 3, other = 4))
  )
  
  observed1 <- head(convert_labelled_to_chr(x = data[["gender"]]))
  observed2 <- head(convert_labelled_to_chr(x = data[["race"]]))
  
  expected1 <- c("M", "X", "N/A", "M", "X", "N/A")
  expected2 <- c("2", "1", "2", "3", "1", "3")
  
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})



test_that("check_ignore_struct", {
  observed1 <- check_ignore_struct(NULL, "cat", FALSE)
  observed2 <- check_ignore_struct(NULL, "mean", FALSE)
  observed3 <- check_ignore_struct(NULL, "select", FALSE)
  observed4 <- check_ignore_struct(NULL, "cat", TRUE)
  observed5 <- check_ignore_struct(NULL, "mean", TRUE)
  observed6 <- check_ignore_struct(NULL, "select", TRUE)
  
  expected <- list(ignore = NULL)
  
  expect_equal(observed1, expected)
  expect_equal(observed2, expected)
  expect_equal(observed3, expected)
  expect_equal(observed4, expected)
  expect_equal(observed5, expected)
  expect_equal(observed6, expected)
})


test_that("extract_ignore_map", {
  observed1_result <-
    extract_ignore_map(
      vars = c("var1", "group1"),
      ignore = c(group1 = 2),
      var_stem_map = NULL
    )
  
  observed2_result <-
    extract_ignore_map(
      vars = "stem",
      ignore = c(stem = 1),
      var_stem_map = stats::setNames(c("stem_1", "stem_2", "stem_3"), rep("stem", 3))
    )
  
  observed3_result <-
    extract_ignore_map(
      vars = c("stem", "group_var"),
      ignore = list(stem = 1, group_var = "category"),
      var_stem_map = stats::setNames(c("stem_1", "stem_2", "stem_3"), rep("stem", 3))
    )
  
  observed4_result <-
    extract_ignore_map(
      vars = c("stem", "grp_var"),
      ignore = list(stem = 1, group_var = "category"),
      var_stem_map = stats::setNames(c("stem_1", "stem_2", "stem_3"), rep("stem", 3))
    )
                                        
  expected1 <- list(group1 = 2)
  expected2 <- list(stem_1 = 1, stem_2 = 1, stem_3 = 1)
  expected3 <- list(stem_1 = 1, stem_2 = 1, stem_3 = 1, group_var = "category")
  expected4 <- list(stem_1 = 1, stem_2 = 1, stem_3 = 1)
  
  expect_equal(observed1_result$ignore_map, expected1)
  expect_equal(observed2_result$ignore_map, expected2)
  expect_equal(observed3_result$ignore_map, expected3)
  expect_equal(observed4_result$ignore_map, expected4)
})



