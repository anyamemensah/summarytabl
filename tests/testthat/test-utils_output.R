# Test utils_output functions

test_that("check valid variable data types", {
  test_data <- tibble::tibble(var_1 = paste0(1:10), "var.2" = paste0(1:10))
  observed1 <- check_data_types(data = nlsy,
                                cols = c("race", "gender"),
                                table_type = "cat",
                                allowed_type = "valid_var_types",
                                arg_name = "var")
  
  observed2 <- check_data_types(data = depressive,
                                cols = c("dep_1", "dep_2"),
                                table_type = "select",
                                allowed_type = "valid_var_types",
                                arg_name = "var_stem")
  
  observed3 <- check_data_types(data = sdoh,
                                cols = c("ACS_PCT_AGE_10_14", "NOAAC_PRECIPITATION_MAR"),
                                table_type = "mean",
                                allowed_type = "valid_var_types",
                                arg_name = "var_stem")
  
  expected1 <- list(valid = TRUE, dtype = c(race = "character", gender = "numeric"))
  expected2 <- list(valid = TRUE, dtype = c(dep_1 = "numeric", dep_2 = "numeric"))
  expected3 <- list(valid = TRUE, dtype = c(ACS_PCT_AGE_10_14 = "numeric", 
                                            NOAAC_PRECIPITATION_MAR = "numeric"))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  
  expect_snapshot(error = TRUE, {
    check_data_types(data = nlsy,
                     cols = c("race"),
                     table_type = "mean",
                     allowed_type = "valid_var_types",
                     arg_name = "var_stem")
  })
  
  expect_snapshot(error = TRUE, {
    check_data_types(data = test_data,
                     cols = c("var_1", "var.2"),
                     table_type = "mean",
                     allowed_type = "valid_var_types",
                     arg_name = "var_stem")
  })
})


test_that("extract group information", {
  test_data <- 
    data.frame(
      var_1 = sample(1:3, 10, TRUE),
      var_4 = sample(1:3, 10, TRUE),
      var_10 = sample(1:3, 10, TRUE)
    )
  observed1 <- 
    extract_group_info(
      group = "race",
      group_type = "variable",
      ignore_group_case = FALSE, 
      regex_group = FALSE,
      cols = NULL,
      data = nlsy, 
      table_type = "cat", 
      allowed_type = "valid_grp_types")
  
  observed2 <-
    extract_group_info(
      group = "_\\d",
      group_type = "pattern", 
      ignore_group_case = FALSE, 
      regex_group = FALSE,
      cols = c("dep_1", "dep_2", "dep_3"),
      data = depressive, 
      table_type = "select", 
      allowed_type = "valid_grp_types")

  expected1 <- 
    list(group = "race",
         grp_dtype = list(valid = TRUE, dtype = c(race = "character")),
         cols = NULL)
  expected2 <-
    list(group = "_\\d", 
         grp_dtype = NULL, 
         cols = c("dep_1", "dep_2", "dep_3"))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("check structure of ignore values list", {
    observed1 <- check_ignore_struct(c(race = 1), "cat", FALSE)
    observed2 <- check_ignore_struct(list(var_here = 1:3), "mean", FALSE)
    observed3 <- check_ignore_struct(list(dep = 1:3), "select", FALSE)
    observed4 <- check_ignore_struct(c(race = 1, grp = 2), "cat", TRUE)
    observed5 <- check_ignore_struct(list(var_here = 1:3, grp = "no"), "mean", TRUE)
    observed6 <- check_ignore_struct(list(dep = 1:3, grping_var = "maybe"), "select", TRUE)
    
    expected1 <- list(ignore = c(race = 1))
    expected2 <- list(var_here = 1:3)
    expected3 <- list(dep = 1:3)
    expected4 <- c(race = 1, grp = 2)
    expected5 <- list(var_here = 1:3, grp = "no")
    expected6 <- list(dep = 1:3, grping_var = "maybe")
    
    expect_equal(observed1, expected1)
    expect_equal(observed2, expected2)
    expect_equal(observed3, expected3)
    expect_equal(observed4, expected4)
    expect_equal(observed5, expected5)
    expect_equal(observed6, expected6)
})


test_that("check returned columns", {
  expect_snapshot(error = TRUE, {
    check_returned_cols(character(0), "this", "stem")
  })
  
  expect_snapshot(error = TRUE, {
    check_returned_cols(c("Meep", "beep beep"), "this", "name")
  })
})


test_that("get valid columns", {
  observed1 <- get_valid_cols(cols = colnames(depressive),
                              var_stem = "dep",
                              var_input = "stem",
                              regex_stem = FALSE,
                              ignore_stem_case = FALSE,
                              find_exact_match = FALSE)
  observed2 <- get_valid_cols(cols = colnames(depressive),
                              var_stem = "dep_2",
                              var_input = "name",
                              regex_stem = FALSE,
                              ignore_stem_case = FALSE,
                              find_exact_match = TRUE)
  
  expected1 <- paste0("dep_", 1:8)
  expected2 <- "dep_2"
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, {
    get_valid_cols(cols = colnames(depressive),
                   var_stem = "bloop",
                   var_input = "name",
                   regex_stem = FALSE,
                   ignore_stem_case = FALSE,
                   find_exact_match = TRUE)
  })
  
  expect_snapshot(error = TRUE, {
    get_valid_cols(cols = colnames(depressive),
                   var_stem = "bloop",
                   var_input = "stem",
                   regex_stem = FALSE,
                   ignore_stem_case = FALSE,
                   find_exact_match = TRUE)
  })
})


test_that("check stem mapping", {
  observed1 <- check_stem_mapping(cols = c("dep_1", "dep_2"), 
                                  var_stem = "dep", 
                                  var_input = "stem")
  observed2 <- check_stem_mapping(cols = c("dep_1", "dep_2"), 
                                  var_stem = "dep", 
                                  var_input = "name")
  
  expected1 <- c(dep = "dep_1", dep = "dep_2")
  expected2 <- NULL
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("check variable labels", {
  check_var_labels
  observed1 <- 
    check_var_labels(cols = c("var_1", "var_2"), 
                     var_stem_labels = c(var_1 = "variable 1", 
                                         var_2 = "variable_2"))
  observed2 <- check_var_labels(cols = "var_1",
                                var_stem_labels = c(var_1 = "variable 1", 
                                                    var_2 = "variable_2",
                                                    var_3 = "variable_3"))
  
  expected1 <- c(var_1 = "variable 1", var_2 = "variable_2")
  expected2 <- c(var_1 = "variable 1")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("drop 'only' requested columns", {
  test_data1 <-  data.frame(variable = letters[1:3],
                            values = c("one", "two", "three"),
                            count = 1, 
                            percent = c(1/3, 1/3, 1/3))
  test_data2 <-  data.frame(variable = letters[1:3],
                            item = c("item 1", "item 2", "item 3"),
                            mean = c(3.44, 2.22, 4.5),
                            sd = c(0.2, 1.2, 4.22),
                            min = c(0, 1, 1),
                            max = c(5, 4, 3),
                            nobs = c(100, 59, 88))
  
  observed1 <- drop_only_cols(test_data1, only = c("count"), only_type = only_type("cat"))
  observed2 <- drop_only_cols(test_data1, only = c("percent"), only_type = only_type("cat"))
  observed3 <- drop_only_cols(test_data1, only = c("count"), only_type = only_type("select"))
  observed4 <- drop_only_cols(test_data1, only = c("percent"), only_type = only_type("select"))
  observed5 <- drop_only_cols(test_data2, only = c("mean", "sd"), only_type = only_type("mean"))
  observed6 <- drop_only_cols(test_data2, only = c("nobs", "min", "max"), only_type = only_type("mean"))
  
  expected1 <- data.frame(variable = letters[1:3], values = c("one", "two", "three"), count = 1)
  expected2 <- data.frame(variable = letters[1:3], values = c("one", "two", "three"), percent = 1/3)
  expected3 <- data.frame(variable = letters[1:3], values = c("one", "two", "three"), count = 1)
  expected4 <- data.frame(variable = letters[1:3], values = c("one", "two", "three"), percent = 1/3)
  expected5 <- data.frame(variable = letters[1:3], 
                          item = c("item 1", "item 2", "item 3"), 
                          mean = c(3.44, 2.22, 4.50),
                          sd = c(0.20, 1.20, 4.22))
  expected6 <- data.frame(variable = letters[1:3], 
                          item = c("item 1", "item 2", "item 3"), 
                          min = c(0,1,1),
                          max = c(5, 4, 3),
                          nobs = c(100, 59, 88))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
})


test_that("extract group flags", {
  observed1 <- extract_group_flags(cols = c("test_t1", "test_t2"), 
                                   pattern = "_t\\d", 
                                   remove_non_alum = TRUE, 
                                   ignore.case = FALSE, 
                                   perl = FALSE)
  expected1 <- c("t1", "t2")
  
  observed2 <- extract_group_flags(cols = c("new_test_1", "new_test_2"), 
                                   pattern = "\\d", 
                                   remove_non_alum = TRUE,
                                   ignore.case = FALSE, 
                                   perl = FALSE)
  expected2 <- c("1", "2")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("extract ignore_map", {
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


test_that("extract ignore_map", {
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



test_that("find_columns", {
  observed1 <- 
    find_columns(cols = colnames(stem_social_psych), var_stem = "belong_belong")
  expected1 <- c("belong_belongStem_w1", "belong_belongStem_w2")
  
  observed2 <- 
    find_columns(cols = colnames(social_psy_data), var_stem = "identity")
  expected2 <- c("identity_1", "identity_2", "identity_3", "identity_4")
  
  observed3 <- 
    find_columns(cols = colnames(social_psy_data), var_stem = "NANA")
  expected3 <- character(0)
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})


test_that("generate key for recoding values", {
  key_observed <- 
    generate_tbl_key(values_from = 1:3, 
                     values_to = c("one", "two", "three"))
  
  key_expected <- 
    purrr::map2(.x = paste0(1:3),
                .y = c("one", "two", "three"),
                .f = ~ rlang::new_formula(.x, .y))
  
  expect_equal(key_observed, key_expected, ignore_attr = TRUE)
  
  expect_snapshot(error = TRUE, {
    generate_tbl_key(values_from = 1:2, 
                     values_to = c("one", "two", "three"))
  })
})


test_that("extract a standardized variable 'data type'", {
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


test_that("test available summary statistics", {
  observed1 <- only_type("cat")
  expected1 <- c("count", "percent")
  
  observed2 <- only_type("mean")
  expected2 <- c("mean", "median", "sd", "min", "max", "nobs")
  
  observed3 <- only_type("select")
  expected3 <- c("count", "percent")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)

  expect_error(
    only_type("TEST"),
    "'table_type' should be one of cat, mean, select."
  )
})


test_that("pivoting tabl to wider format", {
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


test_that("pluck columns", {
  ex_list <-
    list(
      var_stem1 = list(
        var_stem = list(
          valid = TRUE,
          cols = stats::setNames(c("var_stem1_col1","var_stem1_col2"),
                                 rep("var_stem1", times = 2)))),
      var_stem2 = list(
        var_stem = list(
          valid = TRUE,
          cols = stats::setNames(c("var_stem2_col1","var_stem2_col2",
                                   "var_stem2_col3","var_stem2_col4"),
                                 rep("var_stem2", times = 4)))))

  observed1 <- pluck_cols(ex_list, "var_stem", "cols")
  expected1 <- c("var_stem1_col1", "var_stem1_col2", "var_stem2_col1", 
                 "var_stem2_col2", "var_stem2_col3", "var_stem2_col4") |>
    stats::setNames(c(rep("var_stem1", times = 2), rep("var_stem2", times = 4)))
  
  expect_equal(observed1, expected1)
})


test_that("pluck variable labels", {
  ex_list <-
    list(
      var_stem1 = list(
        var_stem = list(
          var_labels = c(
            var_stem1_col1 = "variable stem 1, column 1", 
            var_stem1_col2= "variable stem 1, column 2"
          ))),
      var_stem2 = list(
        var_stem = list(
          var_labels = c(
            var_stem2_col1 = "variable stem 2, column 1", 
            var_stem2_col2 = "variable stem 2, column 2",
            var_stem2_col3 = "variable stem 2, column 3"
          ))))
  
  observed1 <- pluck_var_labels(ex_list, "var_stem", "var_labels")
  expected1 <- c(var_stem1_col1 = "variable stem 1, column 1",
                 var_stem1_col2 = "variable stem 1, column 2", 
                 var_stem2_col1 = "variable stem 2, column 1", 
                 var_stem2_col2 = "variable stem 2, column 2", 
                 var_stem2_col3 = "variable stem 2, column 3")
  
  expect_equal(observed1, expected1)
})


test_that("pluck stem map", {
  ex_list <-
    list(
      var_stem1 = list(
        var_stem = list(
          var_stem_map = c(
            var_stem1 = "var_stem1_col1",
            var_stem1= "var_stem1_col2"
          ))),
      var_stem2 = list(
        var_stem = list(
          var_stem_map = c(
            var_stem2 = "var_stem2_col1", 
            var_stem2 = "var_stem2_col2",
            var_stem2 = "var_stem2_col3"
          ))))
  
  observed1 <- pluck_stem_map(ex_list, "var_stem", "var_stem_map")
  expected1 <- c(var_stem1 = "var_stem1_col1",
                 var_stem1 = "var_stem1_col2", 
                 var_stem2 = "var_stem2_col1", 
                 var_stem2 = "var_stem2_col2", 
                 var_stem2 = "var_stem2_col3")
  
  expect_equal(observed1, expected1)
})


test_that("replace with NA", {
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


test_that("return valid data types by table type", {
  observed1 <- return_data_types(table_type = "cat")$valid_var_types
  expected1 <- c(factor = "factor", character = "character", 
                 logical = "logical", numeric = "numeric", 
                 datetime = "POSIXt", datetime = "POSIXct", 
                 datetime = "POSIXlt", datetime = "difftime", 
                 datetime = "Date")
  
  observed2 <- return_data_types(table_type = "mean")$valid_var_types
  expected2 <- c(numeric = "numeric", datetime = "POSIXt", 
                 datetime = "POSIXct", datetime = "POSIXlt", 
                 datetime = "difftime", datetime = "Date")
  
  observed3 <- return_data_types(table_type = "select")$valid_var_types
  expected3 <-  c(factor = "factor", character = "character", 
                  logical = "logical", numeric = "numeric", 
                  datetime = "POSIXt", datetime = "POSIXct", 
                  datetime = "POSIXlt", datetime = "difftime", 
                  datetime = "Date")
  
  expect_equal(unname(observed1), unname(expected1))
  expect_equal(unname(observed2), unname(expected2))
  expect_equal(unname(observed3), unname(expected3))
})


test_that("Warning: override pivot wider", {
  sample_tbl <- 
    tibble::tibble(
      variable = c("var_1", "var_1", "var_2", "var2", "var_2"),
      values = as.integer(c(1,2,1,2,3)),
      count = as.integer(c(100, 899, 120, 388, 122))
    )
  
  observed1 <-
  suppressWarnings({override_pivot(tabl = sample_tbl, var_col = "variable", 
                                   values_col = "values", allow_override = FALSE)})
  observed2 <-
  override_pivot(tabl = sample_tbl, var_col = "variable", 
                 values_col = "values", allow_override = TRUE)
  
  expected1 <- FALSE
  expected2 <- TRUE
  
  expect_snapshot(error = FALSE, {
    override_pivot(tabl = sample_tbl, var_col = "variable", 
                   values_col = "values", allow_override = FALSE)
  })
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
 
})
