# Test select_tbl

test_that("Failure: 'data' argument", {
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = NULL,
      var_stem = "dep"
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = data.frame(),
      var_stem = "dep"
    )
  })
})

test_that("Failure: Invalid 'var_stem' argument", {
  ex_mean_dat <- tibble::tibble(var_1 = 1:3, `var 2` = 5:7)
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = depressive,
      var_stem = c("dep", "gender")
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = ex_mean_dat,
      var_stem = "var"
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "BELONG_belong",
      ignore_stem_case = FALSE
    )
  })
})

test_that("Failure: Invalid 'var_input' argument", {
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      var_input = NULL
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      var_input = "var_name"
    )
  })
})

test_that("Failure: Invalid 'only' argument", {
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = character(0)
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      only = NA
    )
  })
})

test_that("Failure: Invalid 'na_removal' argument", {
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      na_removal = NULL
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      na_removal = "side-ways"
    )
  })
})


test_that("Failure: Invalid 'pivot' argument", {
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      pivot = NULL
    )
  })
  
  expect_snapshot(error = TRUE, {
    select_tbl(
      data = stem_social_psych,
      var_stem = "belong_belong",
      pivot = "side_ways"
    )
  })
})

test_that("Expected output", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count"
    ) |> head()

  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )

  expect_equal(observed, expected)
})


test_that("Expected output with variable labels", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
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
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      variable_label = rep(c("how often child feels sad and blue", 
                             "how often child feels nervous, tense, or on edge"),
                           each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )
  
  expect_equal(observed, expected)
})

test_that("Expected output with 'ignore' values", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count",
      ignore = c(dep = 1)
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2", "dep_3"), each = 2),
      values = rep(2:3, times = 3),
      count = c(68, 38, 61, 45, 96, 10)
    )
  
  expect_equal(observed, expected)
})

test_that("Expected output with ignore_stem_case and 'ignore' values", {
  observed <-
    select_tbl(
      data = depressive,
      var_stem = "DEP",
      ignore_stem_case = TRUE,
      only = "count",
      ignore = c(DEP = 1)
    ) |> head()
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2", "dep_3"), each = 2),
      values = rep(2:3, times = 3),
      count = c(68, 38, 61, 45, 96, 10)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with multiple names and ignore values (pairwise deletion)", {
  observed <-
      select_tbl(
      data = depressive,
      var_stem = c("dep_1", "dep_5"),
      var_input = "name",
      na_removal = "pairwise",
      only = "count",
      ignore = c(dep_1 = 3, dep_5 = 2))
  
  expected <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_5"), each = 2),
      values = as.integer(c(1,2,1,3)),
      count = as.integer(c(120, 709, 206, 871))
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with multiple variable stems and ignore values (pairwise deletion)", {
  observed <-
    select_tbl(
      data = social_psy_data,
      var_stem = c("belong", "identity"),
      na_removal = "pairwise",
      only = "count",
      ignore = list(belong = c(1,2,3), identity = c(3, 4,5))) |>
    tail()
  
  expected <-
    tibble::tibble(
      variable = rep(c("identity_2", "identity_3", "identity_4"), each = 2),
      values = as.integer(rep(1:2, times = 3)),
      count = as.integer(c(465, 1025, 51, 133, 1474, 3333))
    )
  
  expect_equal(observed, expected)
})

test_that("Expected output with different 'only' types", {
  observed1 <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "count"
    ) |> head()
  
  expected1 <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      count = c(109, 689, 809, 144, 746, 717)
    )
  
  observed2 <-
    select_tbl(
      data = depressive,
      var_stem = "dep",
      only = "percent"
    ) |> 
    head() |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      variable = rep(c("dep_1", "dep_2"), each = 3),
      values = rep(1:3, times = 2),
      percent = c(0.068, 0.429, 0.503, 0.09, 0.464, 0.446)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})


test_that("generate_select_tabl expected output", {
  observed1 <- 
    generate_select_tabl(depressive, "dep_1", "pairwise") |>
    dplyr::mutate(percent = round(percent, digits = 3))
  expected1 <-
    tibble::tibble(
      variable = "dep_1",
      values = 1:3L,
      count = as.integer(c(120, 709, 825)),
      percent = c(0.073, 0.429, 0.499)
    )
  
  observed2 <- 
    generate_select_tabl(tas, "involved_sports", "pairwise") |>
    dplyr::mutate(percent = round(percent, digits = 3))
  expected2 <-
    tibble::tibble(
      variable = "involved_sports",
      values = 0:1L,
      count = as.integer(c(2114, 412)),
      percent = c(0.837, 0.163)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})

test_that("Warning: override pivot wider", {
  expect_snapshot(error = FALSE, {
    select_tbl(
      data = social_psy_data,
      var_stem = c("belong", "identity"),
      na_removal = "pairwise",
      only = "count",
      pivot = "wider",
      ignore = list(belong = c(1,2,3), identity = c(3,4,5)))
  })
})
