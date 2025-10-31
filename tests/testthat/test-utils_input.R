# Test utils_input functions

test_that("check valid table data types", {
  observed1 <- check_table_type("cat")
  observed2 <- check_table_type("select")
  observed3 <- check_table_type("mean")

  expected1 <- list(valid = TRUE, table_type = "cat")
  expected2 <- list(valid = TRUE, table_type = "select")
  expected3 <- list(valid = TRUE, table_type = "mean")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  
  expect_error(
    check_table_type(c("category1", "category2")),
    "Invalid 'table_type' argument. 'table_type' must be a character vector of length one."
  )
  
  expect_error(
    check_table_type(c("category")),
    "Invalid 'table_type' argument. 'table_type' must be one of: 'cat', 'select', 'mean'."
  )
})

test_that("check 'data' argument", {
  observed1 <- check_df(data.frame(x = c(1,2,3), y = c(4,5,6)))
  observed2 <- check_df(tibble::tibble(x = c(1,2,3), y = c(4,5,6)))
  
  expected1 <- list(valid = TRUE, df = data.frame(x = c(1,2,3), y = c(4,5,6)))
  expected2 <- list(valid = TRUE, df = tibble::tibble(x = c(1,2,3), y = c(4,5,6)))

  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_df(NA) })
  
  expect_snapshot(error = TRUE, { check_df(tibble::tibble()) })
})


test_that("check logical argument", {
  observed1 <- check_logical(x = TRUE, label = "this_var")
  observed2 <- check_logical(x = FALSE, label = "this_var")
  
  expected1 <- list(valid = TRUE, x = TRUE)
  expected2 <- list(valid = TRUE, x = FALSE)
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_logical(x = "THIS", label = "this_var") })
  expect_snapshot(error = TRUE, { check_logical(x = NA, label = "NA_var") })
})


test_that("check 'margins' argument", {
  observed1 <- check_margins("all")
  observed2 <- check_margins("rows")
  observed3 <- check_margins("columns")
  
  expected1 <- list(valid = TRUE, margins = "all")
  expected2 <- list(valid = TRUE, margins = "rows")
  expected3 <- list(valid = TRUE, margins = "columns")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  
  expect_snapshot(error = TRUE, { check_margins(NA) })
  expect_snapshot(error = TRUE, { check_margins("diagonal") })
})


test_that("check 'na.rm' argument", {
  observed1 <- check_na.rm(na.rm = TRUE, var_label = "row_var")
  observed2 <- check_na.rm(na.rm = FALSE, var_label = "col_var")

  expected1 <- list(valid = TRUE, na.rm = TRUE)
  expected2 <- list(valid = TRUE, na.rm = FALSE)

  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)

  expect_snapshot(error = TRUE, { check_na.rm(na.rm = "this_na", var_label = "row_var") })
  expect_snapshot(error = TRUE, { check_na.rm(na.rm = NA, var_label = "row_var") })
})


test_that("check 'na_removal' argument", {
  observed1 <- check_na_removal(na_removal = "listwise")
  observed2 <- check_na_removal(na_removal = "pairwise")
  
  expected1 <- list(valid = TRUE, na_removal = "listwise")
  expected2 <- list(valid = TRUE, na_removal = "pairwise")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_na_removal(na_removal = NA) })
  expect_snapshot(error = TRUE, { check_na_removal(na_removal = "this_na_remove") })
})


test_that("check 'only' argument", {
  observed1 <- check_only(only = NULL, table_type = "cat")
  observed2 <- check_only(only = c("count"), table_type = "cat")
  observed3 <- check_only(only = c("percent"), table_type = "cat")
  observed4 <- check_only(only = NULL, table_type = "select")
  observed5 <- check_only(only = c("count"), table_type = "select")
  observed6 <- check_only(only = c("percent"), table_type = "select")
  observed7 <- check_only(only = NULL, table_type = "mean")
  observed8 <- check_only(only = c("mean"), table_type = "mean")
  observed9 <- check_only(only = c("sd"), table_type = "mean")
  observed10 <- check_only(only = c("min"), table_type = "mean")
  observed11 <- check_only(only = c("max"), table_type = "mean")
  observed12 <- check_only(only = c("nobs"), table_type = "mean")
  
  expected1 <- list(valid = TRUE, only = c("count", "percent"))
  expected2 <- list(valid = TRUE, only = c("count"))
  expected3 <- list(valid = TRUE, only = c("percent"))
  expected4 <- list(valid = TRUE, only = c("count", "percent"))
  expected5 <- list(valid = TRUE, only = c("count"))
  expected6 <- list(valid = TRUE, only = c("percent"))
  expected7 <- list(valid = TRUE, only = c("mean", "sd", "min", "max", "nobs"))
  expected8 <- list(valid = TRUE, only = c("mean"))
  expected9 <- list(valid = TRUE, only = c("sd"))
  expected10 <- list(valid = TRUE, only = c("min"))
  expected11 <- list(valid = TRUE, only = c("max"))
  expected12 <- list(valid = TRUE, only = c("nobs"))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
  expect_equal(observed7, expected7)
  expect_equal(observed8, expected8)
  expect_equal(observed9, expected9)
  expect_equal(observed10,expected10)
  expect_equal(observed11,expected11)
  expect_equal(observed12,expected12)
  
  expect_snapshot(error = TRUE, { check_only(only = character(0), table_type = "cat") })
  expect_snapshot(error = TRUE, { check_only(only = c("count", "per"), table_type = "cat") })
})


test_that("check 'pivot' argument", {
  observed1 <- check_pivot(pivot = "wider")
  observed2 <- check_pivot(pivot = "longer")
  
  expected1 <- list(valid = TRUE, pivot = "wider")
  expected2 <- list(valid = TRUE, pivot = "longer")

  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_pivot(pivot = NA) })
  expect_snapshot(error = TRUE, { check_pivot(pivot = "diagonal")})
})


test_that("check 'group_name' argument", {
  observed1 <- check_group_name(group_name = "group_a")
  observed2 <- check_group_name(group_name = "group.b")
  observed3 <- check_group_name(group_name = NULL)
  
  expected1 <- list(valid = TRUE, group_name = "group_a")
  expected2 <- list(valid = TRUE, group_name = "group.b")
  expected3 <- list(valid = TRUE, group_name = NULL)
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  
  expect_snapshot(error = TRUE, { check_group_name(group_name = "THIS NAME") })
  expect_snapshot(error = TRUE, { check_group_name(group_name = "That$NAme")})
})


test_that("check 'var_stem' argument", {
  observed1 <- check_var_stem(var_stem = "var_1")
  observed2 <- check_var_stem(var_stem = c("var_1", "var_2", "var_3"))
  
  expected1 <- list(valid = TRUE, var_stem = "var_1")
  expected2 <- list(valid = TRUE, var_stem = c("var_1", "var_2", "var_3"))
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_var_stem(var_stem = character(0)) })
  expect_snapshot(error = TRUE, { check_var_stem(var_stem = c("this_var", "THAT VAR")) })

})


test_that("check an individual variable argument (i.e., 'var', 'row_var', 'col_var')", {
  invalid_data <- tibble::tibble("invalid col name" = c(1:10))
  
  observed1 <- check_var("race", "var_label_here", nlsy)

  expected1 <- list(valid = TRUE, var = "race", label = "var_label_here")

  expect_equal(observed1, expected1)
  
  expect_snapshot(error = TRUE, { check_var(var_name = character(0), "var_label_here", nlsy) })
  expect_snapshot(error = TRUE, { check_var("not_a_var", "var_label_here", nlsy) })
  expect_snapshot(error = TRUE, { check_var("invalid col name", "invalid_var_arg", invalid_data) })
})


test_that("check 'var_input' argument", {
  expect_snapshot(error = TRUE, { check_var_input(var_input = character(0)) })
  expect_snapshot(error = TRUE, { check_var_input("stem_name") })      
})


test_that("check 'group' argument", {
  data_test <- 
    data.frame(
      group_var = c(1,2,3),
      group_var = c(1,2,3),
      check.names = FALSE
    )
  
  observed1 <- check_group_var(group_var = "RACE", group_type = "variable", 
                               col_names = colnames(nlsy), ignore_case = TRUE,
                               use_regex = FALSE)
  observed2 <- check_group_var(group_var = "_w\\d", group_type = "pattern", 
                               col_names = colnames(nlsy), ignore_case = FALSE,
                               use_regex = FALSE)
  
  expected1 <- list(valid = TRUE, group_var = "race", orig_group_var = "RACE")
  expected2 <- list(valid = TRUE, group_var = "_w\\d", orig_group_var = "_w\\d")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  
  expect_snapshot(error = TRUE, { check_group_var(group_var = NULL) })
  expect_snapshot(error = TRUE, { check_group_var(group_var = "This VAR", group_type = "variable") })
  expect_snapshot(error = TRUE, { check_group_var(group_var = "group_var", group_type = "variable", 
                                                  col_names = names(data_test), ignore_case = FALSE, 
                                                  use_regex = FALSE) })
  expect_snapshot(error = TRUE, { check_group_var(group_var = "boop", group_type = "variable", 
                                                  col_names = names(data_test), ignore_case = FALSE, 
                                                  use_regex = FALSE) })
})


test_that("check 'group_type' argument", {
  observed1 <- check_group_type(group_type = "variable")
  observed2 <- check_group_type(group_type = "pattern")
  
  expected1 <- list(valid = TRUE, group_type = "variable")
  expected2 <- list(valid = TRUE, group_type = "pattern")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  
  expect_snapshot(error = TRUE, { check_group_type(group_type = character(0)) })
  expect_snapshot(error = TRUE, {check_group_type(group_type = "shape") })
})


test_that("string has invalid characters", {
  observed1 <- string_has_invalid_chars("here var")
  observed2 <- string_has_invalid_chars("there$var")
  observed3 <- string_has_invalid_chars("here#var")
  observed4 <- string_has_invalid_chars("there-var")
  observed5 <- string_has_invalid_chars("everywhere_var")
  observed6 <- string_has_invalid_chars("everywhere.var")
  
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