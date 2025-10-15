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



test_that("check_ignore_struct expected output", {
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


test_that("extract_ignore_map expected output", {
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

test_that("check_string_invalid_chars expected output", {
  observed1 <- check_string_invalid_chars("here var")
  observed2 <- check_string_invalid_chars("there$var")
  observed3 <- check_string_invalid_chars("here#var")
  observed4 <- check_string_invalid_chars("there-var")
  observed5 <- check_string_invalid_chars("everywhere_var")
  observed6 <- check_string_invalid_chars("everywhere.var")
  
  expected1 <- TRUE
  expected2 <- TRUE
  expected3 <- TRUE
  expected4 <- TRUE
  expected5 <- FALSE
  expected6 <- FALSE
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
})



test_that("pivot_tbl_wider expected output", {
  data_wider_test1 <- 
    tibble::tibble(
      var_1 = c("group_1", "group_1", "group_2", "group_2"),
      var_2 = c("cat_1", "cat_2", "cat_1", "cat_2"),
      count = as.integer(c(10, 20, 30, 40)),
      percent = c(0.10, 0.20, 0.30, 0.40)
    )
  
  data_wider_test2 <- 
    tibble::tibble(
      variable = c("varStem_1", "varStem_1", "varStem_2", "varStem_2"),
      values = c("selected", "unselected", "selected", "unselected"),
      count = as.integer(c(100, 200, 300, 400)),
      percent = c(100/300, 200/300, 300/700, 400/700)
    )
  
  data_wider_test3 <- 
    tibble::tibble(
      variable = rep(c("var_a", "var_b"), each = 2),
      group = rep(c("a", "b"), each = 2),
      values = rep(c(0L, 1L), times = 2),
      count = c(10L, 10L, 12L, 8L),
      percent = c(0.5, 0.5, 0.6, 0.4)
    )
  
  data_wider_test4 <- 
    tibble::tibble(
      variable = rep(c("var_a", "var_b"), each = 4),
      group = rep(rep(c("control", "trial"), each = 2), times = 2),
      values = rep(c(0L, 1L), times = 4),
      count = c(6L, 6L, 4L, 4L, 8L, 4L, 4L, 4L),
      percent = c(0.3, 0.3, 0.2, 0.2, 0.4, 0.2, 0.2, 0.2)
    )
  
  observed1 <- 
    pivot_tbl_wider(data_wider_test1,
                    "var_1",
                    "var_2",
                    "{.value}_var_2_{var_2}",
                    c("count", "percent"))
  
  expected1 <-
    tibble::tibble(
      var_1 = c("group_1", "group_2"),
      count_var_2_cat_1 = as.integer(c(10, 30)),
      count_var_2_cat_2 = as.integer(c(20, 40)),
      percent_var_2_cat_1 = c(0.10, 0.30),
      percent_var_2_cat_2 = c(0.20, 0.40)
    )
  
  observed2 <- 
    pivot_tbl_wider(data_wider_test2,
                    "variable",
                    "values",
                    "{.value}_value_{values}",
                    c("count", "percent"))
  
  expected2 <-
    tibble::tibble(
      variable = c("varStem_1", "varStem_2"),
      count_value_selected = as.integer(c(100, 300)),
      count_value_unselected = as.integer(c(200, 400)),
      percent_value_selected = c(100/300, 300/700),
      percent_value_unselected = c(200/300, 400/700)
    )
  
  observed3 <- 
    pivot_tbl_wider(data_wider_test3,
                    "variable",
                    "values",
                    "{.value}_value_{values}",
                    c("count", "percent"))

  expected3 <-
    tibble::tibble(
      variable = c("var_a", "var_b"),
      count_value_0 = as.integer(c(10, 12)),
      count_value_1 = as.integer(c(10, 8)),
      percent_value_0 = c(0.5, 0.6),
      percent_value_1 = c(0.5, 0.4)
    )
  
  observed4 <- 
    pivot_tbl_wider(data_wider_test4,
                    c("variable", "values"),
                    "group",
                    paste0("{.value}_group_{group}"),
                    c("count", "percent"))
  
  expected4 <-
    tibble::tibble(
      variable = rep(c("var_a", "var_b"), each = 2),
      values = rep(0:1L, times = 2),
      count_group_control = as.integer(c(6,6,8,4)),
      count_group_trial = as.integer(4),
      percent_group_control = c(0.3, 0.3, 0.4, 0.2),
      percent_group_trial = c(0.2, 0.2, 0.2, 0.2)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
})


test_that("check_pivot", {
  expect_error(
    check_pivot(1),
    "Invalid 'pivot' argument. 'pivot' must be a character vector of length one."
  )
  
  expect_error(
    check_pivot('sideways'),
    "Invalid 'pivot' argument. 'pivot' must be one of 'wider', 'longer'."
  )
  
  observed1 <- check_pivot("wider")
  expected1 <- list(valid = TRUE, pivot = "wider")
  
  observed2 <- check_pivot("longer")
  expected2 <- list(valid = TRUE, pivot = "longer")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})

test_that("check_margins", {
  expect_error(
    check_margins(1),
    "Invalid 'margins' argument. 'margins' must be a character vector of length one."
  )
  
  expect_error(
    check_margins('margins'),
    "Invalid 'margins' argument. 'margins' must be one of 'rows', 'columns', 'all'."
  )
  
  observed1 <- check_margins("all")
  expected1 <- list(valid = TRUE, margins = "all")
  
  observed2 <- check_margins("columns")
  expected2 <- list(valid = TRUE, margins = "columns")
  
  observed3 <- check_margins("rows")
  expected3 <- list(valid = TRUE, margins = "rows")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})


test_that("check_only", {
  expect_error(
    check_only(1, "select"),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
  )
  
  expect_error(
    check_only(1, "cat"),
    "Invalid 'only' argument. 'only' must be any of: count, percent."
  )
  
  expect_error(
    check_only(1, "mean"),
    "Invalid 'only' argument. 'only' must be any of: mean, sd, min, max, nobs."
  )
  
  observed1 <- check_only(NULL, "select")
  expected1 <- list(valid = TRUE, only = c("count", "percent"))
  
  observed2 <- check_only(NULL, "cat")
  expected2 <- list(valid = TRUE, only = c("count", "percent"))
  
  observed3 <- check_only(NULL, "mean")
  expected3 <- list(valid = TRUE, only = c("mean", "sd", "min", "max", "nobs"))
  
  observed4 <- check_only("count", "select")
  expected4 <- list(valid = TRUE, only = "count")
  
  observed5 <- check_only("percent", "select")
  expected5 <- list(valid = TRUE, only = "percent")
  
  observed6 <- check_only("count", "cat")
  expected6 <- list(valid = TRUE, only = "count")
  
  observed7 <- check_only("percent", "cat")
  expected7 <- list(valid = TRUE, only = "percent")
  
  observed8 <- check_only("mean", "mean")
  expected8 <- list(valid = TRUE, only = "mean")
  
  observed9 <- check_only(c("mean", "nobs"), "mean")
  expected9 <- list(valid = TRUE, only = c("mean", "nobs"))
  
  observed10 <- check_only(c("mean", "sd", "nobs"), "mean")
  expected10 <- list(valid = TRUE, only = c("mean", "sd", "nobs"))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
  expect_equal(observed7, expected7)
  expect_equal(observed8, expected8)
  expect_equal(observed9, expected9)
  expect_equal(observed10, expected10)
})


test_that("check_na.rm", {
  expect_error(
    check_na.rm(1, "this_var"),
    "Invalid 'this_var' argument. 'this_var' must be a logical vector of length one."
  )
  
  expect_error(
    check_na.rm(NULL, "this_var"),
    "Invalid 'this_var' argument. 'this_var' must be a logical vector of length one."
  )
  
  expect_error(
    check_na.rm(NA, "this_var"),
    "Invalid 'this_var' argument. 'this_var' must be a logical vector of length one."
  )
  
  observed1 <- check_na.rm(TRUE, "this_label")
  expected1 <- list(valid = TRUE, na.rm =  TRUE)
  
  observed2 <- check_na.rm(FALSE, "that_label")
  expected2 <- list(valid = TRUE, na.rm =  FALSE)

  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("check_na.rm", {
  expect_error(
    check_df(NULL),
    "The 'data' argument is not a data frame."
  )
  
  expect_error(
    check_df(data.frame()),
    "The 'data' argument is empty."
  )
  
  expect_error(
    check_df(tibble::tibble()),
    "The 'data' argument is empty."
  )
  
  observed1 <- check_df(tibble::tibble(a = 1, b = 2))
  expected1 <- list(valid = TRUE, df =  tibble::tibble(a = 1, b = 2))
  
  observed2 <- check_df(data.frame(vara = 1, varb = 2))
  expected2 <- list(valid = TRUE, df =  data.frame(vara = 1, varb = 2))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


