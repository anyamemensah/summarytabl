# Test check_named_vctr

test_that("Expected output: NULL, numeric vector", {
  observed <-
    check_named_vctr(x = c(one = 1, two = 2, 3),
                     names = c("one", "two", "three"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})

test_that("Expected output: named vector, numeric vector", {
  observed <-
    check_named_vctr(x = c(one = 1, two = 2, three = 3),
                     names = c("one", "two", "three"),
                     default = NULL)
  
  expected <- c(one = 1, two = 2, three = 3)
  
  expect_equal(observed, expected)
})

test_that("Expected output: NULL, character vector", {
  observed <-
    check_named_vctr(x = c(a = "apple", b = "banana", "cherry"),
                     names = c("c", "b", "a"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})

test_that("Expected output: named vector, character vector", {
  observed <-
    check_named_vctr(x = c(a = "apple", b = "banana", c = "cherry"),
                     names = c("c", "b", "a"),
                     default = NULL)
  
  expected <-
    c(a = "apple", b = "banana", c = "cherry")
  
  expect_equal(observed, expected)
})


test_that("Expected output: NULL, logical vector", {
  observed <-
    check_named_vctr(x = c(this = TRUE, that = FALSE, theOther = TRUE),
                     names = c("notThis", "That", "TheOther"),
                     default = NULL)
  
  expected <- NULL
  
  expect_equal(observed, expected)
})


test_that("Expected output: named vector, logical vector", {
  observed <-
    check_named_vctr(x = c(this = TRUE, that = FALSE, theOther = TRUE),
                     names = c("theOther", "this", "that"),
                     default = NULL)
  
  expected <- c(this = TRUE, that = FALSE, theOther = TRUE)
  
  expect_equal(observed, expected)
})


test_that("Expected output: NULL, list", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "trois", "deux", "quatier", "quatre"),
                     default = NULL)
  
  expected <-  NULL
  
  expect_equal(observed, expected)
})


test_that("Expected output: named vector, list", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "trois", "deux", "quatre"),
                     default = NULL)
  
  expected <- list(un = 1, deux = 2, trois = 3, quatre = 4)
  
  expect_equal(observed, expected)
})


test_that("Expected null: list with a logical element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "trois", TRUE, "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})

test_that("Expected null: list with a numeric element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", 1, "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})

test_that("Expected null: list with an integer element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", 1L, "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})


test_that("Expected null: list of character vectors where one vector contains more than one element", {
  observed <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", c("three", "trois"), "deux", "quatre"),
                     default = NULL)
  
  expected <- NULL
  expect_equal(observed, expected)
})

test_that("Expected null: mismatch length of names", {
  observed1 <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "deux", "quatre"),
                     default = NULL)
  expected1 <- NULL
  
  observed2 <-
    check_named_vctr(x = c(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "deux", "quatre"),
                     default = NULL)
  expected2 <- NULL
  
  observed3 <-
    check_named_vctr(x = c(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "deux", "quatre"),
                     default = NULL)
  expected3 <- NULL
  
  observed4 <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = c("un", "deux", "quatre"),
                     default = NULL)
  expected4 <- NULL
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
})

test_that("Expected null: NA or NULL in names or x", {
  observed1 <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "deux", "trois", NULL, "quatre"),
                     default = NULL)
  expected1 <- NULL
  
  observed2 <-
    check_named_vctr(x = c(un = 1, deux = 2, trois = 3, quatre = 4),
                     names = list("un", "deux", "quatre", NULL),
                     default = NULL)
  expected2 <- NULL
  
  observed3 <-
    check_named_vctr(x = list(un = NULL, deux = 2, trois = 3),
                     names = c("un", "deux", "trois"),
                     default = NULL)
  expected3 <- NULL
  
  observed4 <-
    check_named_vctr(x = list(un = 1, deux = 2, trois = 3, quatre = NA),
                     names = list("un", "deux", "trois", "quatre"),
                     default = NULL)
  expected4 <- NULL
  
  observed5 <-
    check_named_vctr(x = c(un = 1, deux = 2, trois = NA, quatre = 4),
                     names = list("un", "deux", "quatre", NULL),
                     default = NULL)
  expected5 <- NULL
  
  observed6 <-
    check_named_vctr(x = list(un = NA, deux = 2, trois = 3),
                     names = c("un", "deux", "trois"),
                     default = NULL)
  expected6 <- NULL
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
  expect_equal(observed6, expected6)
})


test_that("check_name_arg", {
  observed1 <- check_name_arg(TRUE)
  expected1 <- NULL
  
  observed2 <- check_name_arg(list(TRUE))
  expected2 <- NULL
  
  observed3 <- check_name_arg(list("A"))
  expected3 <- "A"
  
  observed4 <- check_name_arg(
    list(a = "apple", b = c("blueberry", "banana"))
    )
  expected4 <- NULL
  
  observed5 <- check_name_arg(list(a = "apple", b = c("banana")))
  expected5 <- c(a = "apple", b = "banana")
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
  expect_equal(observed5, expected5)
})
  

test_that("check_invalid_list_values", {
  observed1 <- check_invalid_list_values(TRUE)
  expected1 <- TRUE
  
  observed2 <- check_invalid_list_values(list(TRUE))
  expected2 <- TRUE
  
  observed3 <- check_invalid_list_values(list("A"))
  expected3 <- TRUE
  
  observed4 <- check_invalid_list_values(
    list(a = "apple", b = c("banana"))
    )
  expected4 <- FALSE
  
  observed5 <- check_invalid_list_values(
    list(a = "apple", b = NA)
  )
  expected5 <- TRUE
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
  expect_equal(observed4, expected4)
})
  

test_that("check_invalid_values", {
  observed1 <- check_invalid_values(c(a = "apple", "blueberry"))
  expected1 <- TRUE
  
  observed2 <- check_invalid_values(c(a = "apple", b = ""))
  expected2 <- TRUE
  
  observed3 <- check_invalid_values(c(a = "apple", b = NA))
  expected3 <- TRUE
  
  expect_equal(observed1, expected1)
  expect_equal(observed2, expected2)
  expect_equal(observed3, expected3)
})


