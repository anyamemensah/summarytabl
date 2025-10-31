# Test cat_tbl

test_that("Failure: 'data' argument", {
  expect_snapshot(error = TRUE, {
    cat_tbl(data = NULL, var = "age")
  })
  
  expect_snapshot(error = TRUE, {
    cat_tbl(data = data.frame(), var = "age")
  })
})

test_that("Failure: 'var' argument", {
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = c("race", "gender"))
  })
  
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = c("raced"))
  })
})


test_that("Failure: 'na.rm' argument", {
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = "race",
            na.rm = "TU")
  })
  
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = "race",
            na.rm = NA)
  })
})

test_that("Failure: 'only' argument", {
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = "race",
            na.rm = TRUE,
            only = character(0))
  })
  
  expect_snapshot(error = TRUE, {
    cat_tbl(data = nlsy,
            var = "race",
            na.rm = TRUE,
            only = NA)
  })
})

test_that("Expected output", {
  observed <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c("percent"),
        .fns = ~ round(., digits = 3)
      )
    )
  
  expected <-
    tibble::tibble(
      race = c("Black", "Hispanic", "Non-Black,Non-Hispanic"),
      count = c(868, 631, 1477),
      percent = c(0.292, 0.212, 0.496)
    )
  
  expect_equal(observed, expected)
})


test_that("Expected output with 'ignore' values", {
  observed <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      ignore = "Hispanic"
    ) |>
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected <-
    tibble::tibble(
      race = c("Black","Non-Black,Non-Hispanic"),
      count = as.integer(c(868, 1477)),
      percent = c(0.37, 0.63)
    ) 
  
  expect_equal(observed, expected)
  
})


test_that("Expected output with different 'only' types", {
  observed1 <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      only = "count"
    )
  
  expected1 <-
    tibble::tibble(
      race = c("Black", "Hispanic", "Non-Black,Non-Hispanic"),
      count = c(868, 631, 1477)
    )
  
  observed2 <-
    cat_tbl(
      data = nlsy,
      var = "race",
      na.rm = TRUE,
      only = "percent"
    ) |> 
    dplyr::mutate(percent = round(percent, digits = 3))
  
  expected2 <-
    tibble::tibble(
      race = c("Black","Hispanic", "Non-Black,Non-Hispanic"),
      percent = c(0.292, 0.212, 0.496)
    )
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
})



